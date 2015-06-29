(require 'dash)

(defvar retris-board
  [[? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]])

(defvar retris-old-board
  (copy-tree retris-board t))

(defvar retris-pieces
  ;; TODO add coordinates
  '((?t :tile ?c)
    (?j :tile ?b)
    (?z :tile ?a)
    (?o :tile ?c)
    (?s :tile ?b)
    (?l :tile ?a)
    (?i :tile ?c)
    (?  :tile ?x)))

(defun retris-tile-lookup (tile)
  (plist-get (cdr (assoc tile retris-pieces)) :tile))

(defvar retris-tiles
  '((?a . [[?^?a?a?a?a?a?a?.]
           [?a?^?^?a?a?a?a?.]
           [?a?^?a?a?a?a?a?.]
           [?a?a?a?a?a?a?a?.]
           [?a?a?a?a?a?a?a?.]
           [?a?a?a?a?a?a?a?.]
           [?a?a?a?a?a?a?a?.]
           [?.?.?.?.?.?.?.?.]])
    (?b . [[?^?b?b?b?b?b?b?.]
           [?b?^?^?b?b?b?b?.]
           [?b?^?b?b?b?b?b?.]
           [?b?b?b?b?b?b?b?.]
           [?b?b?b?b?b?b?b?.]
           [?b?b?b?b?b?b?b?.]
           [?b?b?b?b?b?b?b?.]
           [?.?.?.?.?.?.?.?.]])
    (?c . [[?^?b?b?b?b?b?b?.]
           [?b?^?^?^?^?^?b?.]
           [?b?^?^?^?^?^?b?.]
           [?b?^?^?^?^?^?b?.]
           [?b?^?^?^?^?^?b?.]
           [?b?^?^?^?^?^?b?.]
           [?b?b?b?b?b?b?b?.]
           [?.?.?.?.?.?.?.?.]])
    (?x . [[?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]
           [?.?.?.?.?.?.?.?.]])))

(defvar retris-colors
  '((?^ . "white")
    (?a . "light")
    (?b . "dark")
    (?. . "black")))

(defvar retris-palette
  '(("white" . "#ffffff")
    ("light" . "#64b0ff")
    ("dark"  . "#4240ff")
    ("black" . "#000000")))

(defvar retris-board-width 10)
(defvar retris-board-height 20)
(defvar retris-tile-size 8)
(defvar retris-scaling-factor 2)

(defvar retris-board-pixel-width
  (* retris-board-width retris-tile-size retris-scaling-factor))
(defvar retris-board-pixel-height
  (* retris-board-height retris-tile-size retris-scaling-factor))

(defun retris-generate-xpm-header ()
  "Returns a string resembling a valid XPM header.
WIDTH and HEIGHT should be self-explanatory, COLORS is an alist
made up of a char used in the XPM body and a string for the
symbolic name used by the palette for rendering an image."
  (format
   ;; NOTE the first comment is mandatory
   "/* XPM */
static char *graphic[] = {
/* width height colors chars_per_pixel */
\"%s %s %s 1\"
/* colors */
%s/* pixels */
"
   retris-board-pixel-width
   retris-board-pixel-height
   (length retris-colors)
   (apply 'concat (--map (format "\"%c s %s\",\n" (car it) (cdr it))
                         retris-colors))))

(defvar retris-board-header (retris-generate-xpm-header))

(defvar retris-filler (car (rassoc "black" retris-colors)))

(defun retris-generate-xpm-body ()
  "Returns a string resembling a valid XPM body.
WIDTH and HEIGHT should be self-explanatory, FILLER is a char
used for filling the lines with."
  (concat
   (->> (make-string retris-board-pixel-width retris-filler)
        (format "\"%s\",\n")
        (make-list retris-board-pixel-height)
        (apply 'concat))
   "}"))

(defvar retris-board-body (retris-generate-xpm-body))

;; NOTE this doesn't check for out-of-bounds and is very naive
(defsubst retris--xpm-body-offset (x y)
  ;; NOTE the initial offset is one quote glyph, every line after that
  ;; introduces three glyphs on the right and one of the left
  (1+ (+ (* y (+ 4 retris-board-pixel-width)) x)))

(defsubst retris-xpm-peek (x y)
  (aref retris-board-body (retris--xpm-body-offset x y)))

(defsubst retris-xpm-poke (x y char)
  (aset retris-board-body (retris--xpm-body-offset x y) char))

(defun retris-render-tile (x-offset y-offset tile-char)
  ;; first, calculate the amount of rows and cols to work on
  ;; then look up the tile and do some magic to make upscaling work
  (let ((width (* retris-tile-size retris-scaling-factor))
        (height (* retris-tile-size retris-scaling-factor))
        (tile-grid (cdr (assoc tile-char retris-tiles))))
    (dotimes (y height)
      (dotimes (x width)
        (retris-xpm-poke (+ x x-offset) (+ y y-offset)
                         (aref (aref tile-grid (/ y retris-scaling-factor))
                               (/ x retris-scaling-factor)))))))

(defvar retris-timer nil)
(defvar retris-playing-p t)
(defvar retris-dirty-p t)

(defun retris-diff-boards ()
  (let (coords)
    (dotimes (y retris-board-height)
      (dotimes (x retris-board-width)
        (let ((old-tile (aref (aref retris-old-board y) x))
              (new-tile (aref (aref retris-board y) x)))
          (when (/= old-tile new-tile)
            (push (list x y (retris-tile-lookup new-tile)) coords)))))
    coords))

(defun retris-redraw-board ()
  (when (and retris-dirty-p retris-playing-p)
    (dolist (item (retris-diff-boards))
      (-let [(x y tile) item]
        (retris-render-tile (* retris-tile-size retris-scaling-factor x)
                            (* retris-tile-size retris-scaling-factor y)
                            tile)))
    (setq retris-old-board (copy-tree retris-board t)
          retris-dirty-p nil)
    (with-current-buffer "*retris*"
      (let ((inhibit-read-only t))
        ;; TODO move point to end, disable region and clicking
        (erase-buffer)
        (insert
         (propertize " " 'display (create-image (concat retris-board-header
                                                        retris-board-body)
                                                'xpm t
                                                :color-symbols retris-palette))
         "\n")))))

;; TODO write a function for erasing and drawing a falling piece
;; TODO write a function for erasing and drawing a ghost piece
;; TODO write a function for flashing and erasing a cleared line
;; TODO write a function for redrawing the compacted stack (probably
;; erase it completely, then redraw it with every line moved down as
;; far as possible)
;; TODO write functions for drawing the HUD (next piece, statistics?)

(defun retris-play-or-pause ()
  (interactive)
  (setq retris-playing-p (not retris-playing-p)))

(define-derived-mode retris-mode special-mode "Retris"
  "A XPM game."
  (buffer-disable-undo)
  (unless retris-timer
    (setq retris-timer (run-at-time nil (/ 1.0 60) 'retris-redraw-board))))

(define-key retris-mode-map (kbd "p") 'retris-play-or-pause)

(defun retris ()
  (interactive)
  (with-current-buffer (get-buffer-create "*retris*")
    (retris-mode))
  (display-buffer "*retris*"))
