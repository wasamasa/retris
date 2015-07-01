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
   [? ? ? ? ? ? ? ? ? ? ]]
  "Board grid.")

(defvar retris-old-board
  (copy-tree retris-board t)
  "Previous snapshot of the board grid.")

(defvar retris-pieces
  ;; TODO add coordinates
  '((?t :tile-char ?c)
    (?j :tile-char ?b)
    (?z :tile-char ?a)
    (?o :tile-char ?c)
    (?s :tile-char ?b)
    (?l :tile-char ?a)
    (?i :tile-char ?c)
    (?  :tile-char ?x))
  "Alist of plists for piece chars.
It currently holds the respective tile chars only.")

(defun retris-tile-char-lookup (piece-char)
  "Associate PIECE-CHAR with the respective tile char."
  (plist-get (cdr (assoc piece-char retris-pieces)) :tile-char))

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
           [?.?.?.?.?.?.?.?.]]))
  "Alist of tile associations for tile chars.")

(defvar retris-colors
  '((?^ . "white")
    (?a . "light")
    (?b . "dark")
    (?. . "black"))
  "Alist of color associations for tiles.")

(defvar retris-palette
  '(("white" . "#ffffff")
    ("light" . "#64b0ff")
    ("dark"  . "#4240ff")
    ("black" . "#000000"))
  "Default color palette for the XPM image.")

(defvar retris-board-width 10
  "Width of the board in tiles.")
(defvar retris-board-height 20
  "Height of the board in tiles.")
(defvar retris-tile-size 8
  "Size of each tile in pixels.")
(defvar retris-scaling-factor 2
  "Integer scaling factor of the tiles.")

(defvar retris-board-pixel-width
  (* retris-board-width retris-tile-size retris-scaling-factor)
  "Width of the board image in pixels.")
(defvar retris-board-pixel-height
  (* retris-board-height retris-tile-size retris-scaling-factor)
  "Height of the board image in pixels.")

(defun retris-generate-xpm-header ()
  "Returns a string resembling a valid XPM header."
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

(defvar retris-board-header (retris-generate-xpm-header)
  "XPM header of the board image.")

(defvar retris-filler (car (rassoc "black" retris-colors))
  "Filler char used for the empty board.")

(defun retris-generate-xpm-body ()
  "Returns a string resembling a valid XPM body."
  (concat
   (->> (make-string retris-board-pixel-width retris-filler)
        (format "\"%s\",\n")
        (make-list retris-board-pixel-height)
        (apply 'concat))
   "}"))

(defvar retris-board-body (retris-generate-xpm-body)
  "XPM body of the board image.")

;; NOTE this doesn't check for out-of-bounds and is very naive
(defsubst retris--xpm-body-offset (x y)
  "Calculate the correct XPM offset for char operations.
X and Y are pixel ordinates."
  ;; NOTE the initial offset is one quote glyph, every line after that
  ;; introduces three glyphs on the right and one of the left
  (1+ (+ (* y (+ 4 retris-board-pixel-width)) x)))

(defsubst retris-xpm-peek (x y)
  "Extract a XPM character from the image.
X and Y are pixel ordinates."
  (aref retris-board-body (retris--xpm-body-offset x y)))

(defsubst retris-xpm-poke (x y char)
  "Set a XPM character from the image to CHAR.
X and Y are pixel ordinates."
  (aset retris-board-body (retris--xpm-body-offset x y) char))

(defun retris-render-tile (x-ordinate y-ordinate tile-char
                                      &optional x-offset y-offset)
  "Draw a a tile TILE-CHAR.
X-ORDINATE and Y-ORDINATE take grid ordinates.  X-OFFSET and
Y-OFFSET can be used to shift the respective offsets and default
to zero pixels."
  ;; first, calculate the amount of rows and cols to work on
  ;; then look up the tile and do some magic to make upscaling work
  (let ((width (* retris-tile-size retris-scaling-factor))
        (height (* retris-tile-size retris-scaling-factor))
        (tile (cdr (assoc tile-char retris-tiles)))
        (x-offset (+ (* x-ordinate retris-tile-size retris-scaling-factor)
                     (or x-offset 0)))
        (y-offset (+ (* y-ordinate retris-tile-size retris-scaling-factor)
                     (or y-offset 0))))
    (dotimes (y height)
      (dotimes (x width)
        (retris-xpm-poke (+ x x-offset) (+ y y-offset)
                         (aref (aref tile (/ y retris-scaling-factor))
                               (/ x retris-scaling-factor)))))))

(defvar retris-timer nil
  "Holds the game timer.")
(defvar retris-playing-p t
  "Play/pause state.")
(defvar retris-dirty-p t
  "Dirty flag for drawing.
When non-nil, a redraw of the changed parts is started.")

(defun retris-diff-boards ()
  "Compare `retris-board' and `retris-old-board'.
In case differences are detected, a list of changes is returned,
each consisting of the x-ordinate, y-ordinate and tile char."
  (let (coords)
    (dotimes (y retris-board-height)
      (dotimes (x retris-board-width)
        (let ((old-piece-char (aref (aref retris-old-board y) x))
              (new-piece-char (aref (aref retris-board y) x)))
          (when (/= old-piece-char new-piece-char)
            (push (list x y (retris-tile-char-lookup new-piece-char))
                  coords)))))
    coords))

(defun retris-redraw-board ()
  "Redrawing function for `retris-timer'.
When `retris-dirty-p' and `retris-playing-p' are set, a redraw is
initiated by looking for differences with `retris-diff-boards',
drawing these, updating the state and updating the image in the
Retris buffer."
  (when (and retris-dirty-p retris-playing-p)
    (dolist (item (retris-diff-boards))
      (-let [(x y tile-char) item]
        (retris-render-tile x y tile-char)))
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
  "Toggle play/pause state."
  (interactive)
  (setq retris-playing-p (not retris-playing-p)))

(define-derived-mode retris-mode special-mode "Retris"
  "A XPM game."
  (buffer-disable-undo)
  (unless retris-timer
    (setq retris-timer (run-at-time nil (/ 1.0 60) 'retris-redraw-board))))

(define-key retris-mode-map (kbd "p") 'retris-play-or-pause)

(defun retris ()
  "Start a game of Retris!"
  (interactive)
  (with-current-buffer (get-buffer-create "*retris*")
    (retris-mode))
  (display-buffer "*retris*"))
