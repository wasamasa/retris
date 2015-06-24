(require 'dash)

(defvar retris-board
  [[? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ?o?o? ? ? ? ]
   [? ? ? ? ?o?o? ? ? ? ]
   [? ? ? ? ? ? ? ? ? ? ]
   [? ? ? ? ? ? ? ? ?o?o]
   [? ? ? ? ? ? ? ? ?o?o]
   [? ? ? ? ? ? ? ?s?s?z]
   [? ? ? ? ?t? ?s?s?z?z]
   [? ? ? ? ?t?t?s?s?z?t]
   [? ? ? ? ?t?s?s?i?t?t]
   [? ? ? ?i?j?j?z?i?s?t]
   [? ?l? ?i?j?z?z?i?s?s]
   [? ?l? ?i?j?z?l?i?t?s]
   [? ?l?l?i?l?l?l?t?t?t]])

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

(defun retris-generate-xpm-header (width height colors)
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
   width height
   (length colors)
   (apply 'concat (--map (format "\"%c s %s\",\n" (car it) (cdr it)) colors))))

(retris-generate-xpm-header 8 8 retris-colors)

(defvar retris-filler (car (rassoc "black" retris-colors)))

(defun retris-generate-xpm-body (width height filler)
  "Returns a string resembling a valid XPM body.
WIDTH and HEIGHT should be self-explanatory, FILLER is a char
used for filling the lines with."
  (concat
   (->> (make-string width filler)
        (format "\"%s\",\n")
        (make-list height)
        (apply 'concat))
   "}"))

(retris-generate-xpm-body 8 8 retris-filler)

(defvar retris-board-width 10)
(defvar retris-board-height 20)
(defvar retris-tile-size 8)
(defvar retris-scaling-factor 2)

(defvar retris-board-pixel-width
  (* retris-board-width retris-tile-size retris-scaling-factor))
(defvar retris-board-pixel-height
  (* retris-board-height retris-tile-size retris-scaling-factor))

(defvar retris-board-header
  (retris-generate-xpm-header retris-board-pixel-width
                              retris-board-pixel-height retris-colors))

(defvar retris-board-body
  (retris-generate-xpm-body retris-board-pixel-width
                            retris-board-pixel-height retris-filler))

(defvar retris-preview-buffer-name "*retris preview*")

(defun retris-preview-xpm (xpm-string palette)
  "Display an image based on XPM-STRING in a preview buffer."
  (with-current-buffer (get-buffer-create retris-preview-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (propertize " " 'display (create-image xpm-string 'xpm t
                                              :color-symbols palette))
       "\n")
      (special-mode)))
  (display-buffer retris-preview-buffer-name))

(defun retris-preview ()
  (interactive)
  (-> (concat retris-board-header retris-board-body)
      (retris-preview-xpm retris-palette)))

;; NOTE this doesn't check for out-of-bounds and is very naive
(defun retris--xpm-body-offset (xpm-width x y)
  ;; NOTE the initial offset is one quote glyph, every line after that
  ;; introduces three glyphs on the right and one of the left
  (1+ (+ (* y (+ 4 xpm-width)) x)))

(defsubst retris-xpm-peek (xpm-string xpm-width x y)
  (aref xpm-string (retris--xpm-body-offset xpm-width x y)))

(defsubst retris-xpm-poke (xpm-string xpm-width x y char)
  (aset xpm-string (retris--xpm-body-offset xpm-width x y) char))

(retris-xpm-poke retris-board-body retris-board-pixel-width 159 319 ?.)
(retris-xpm-peek retris-board-body retris-board-pixel-width 159 319)
(retris-xpm-poke retris-board-body retris-board-pixel-width 159 319 ?^)
(retris-xpm-peek retris-board-body retris-board-pixel-width 159 319)

(defun retris-render-tile (xpm-string xpm-width x-offset y-offset
                                      tiles tile-size scale tile-char)
  ;; first, calculate the amount of rows and cols to work on
  ;; then look up the tile and do some magic to make upscaling work
  (let ((width (* tile-size scale))
        (height (* tile-size scale))
        (tile-grid (cdr (assoc tile-char tiles))))
    (dotimes (y height)
      (dotimes (x width)
        (retris-xpm-poke xpm-string xpm-width
                         (+ x x-offset) (+ y y-offset)
                         (aref (aref tile-grid (/ y scale))
                               (/ x scale)))))))

(retris-render-tile retris-board-body retris-board-pixel-width 0 0
                    retris-tiles retris-tile-size retris-scaling-factor ?a)
(retris-render-tile retris-board-body retris-board-pixel-width 16 0
                    retris-tiles retris-tile-size retris-scaling-factor ?a)
(retris-render-tile retris-board-body retris-board-pixel-width 32 0
                    retris-tiles retris-tile-size retris-scaling-factor ?a)
(retris-render-tile retris-board-body retris-board-pixel-width 16 16
                    retris-tiles retris-tile-size retris-scaling-factor ?a)

(defun retris-render-board (xpm-string xpm-width x-offset y-offset
                                       pieces tiles tile-size scale board)
  (let ((board-height (length board))
        (board-width (length (aref board 0))))
    (dotimes (y board-height)
      (dotimes (x board-width)
        (let* ((piece (aref (aref board y) x))
               (tile (plist-get (cdr (assoc piece pieces)) :tile)))
          (retris-render-tile xpm-string xpm-width
                              (+ (* x tile-size scale) x-offset)
                              (+ (* y tile-size scale) y-offset)
                              tiles tile-size scale tile))))))

(retris-render-board retris-board-body retris-board-pixel-width 0 0
                     retris-pieces retris-tiles retris-tile-size
                     retris-scaling-factor retris-board)

;; TODO:
;; - implement the basic game
;; - swap color palette for xpm
;; - introduce proper-looking console variant
;; - introduce ghost
;; - introduce proper scoring (score for drop distance and
;;   special events?)
;; - introduce game modes
;; - introduce theming
;; - introduce next piece preview and swap
;; - introduce gravity and gravity changes (essential for implementing
;;   something like TGM)
(defvar retris-timer nil)

(defun retris-redraw-board ()
  (with-current-buffer "*retris*"
    (retris-render-board retris-board-body retris-board-pixel-width 0 0
                         retris-pieces retris-tiles retris-tile-size
                         retris-scaling-factor retris-board)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (propertize " " 'display (create-image (concat retris-board-header
                                                      retris-board-body)
                                              'xpm t
                                              :color-symbols retris-palette))
       "\n"))))

(benchmark 100 ; 0.05s/0.02s
  '(retris-render-tile retris-board-body retris-board-pixel-width 0 0
                    retris-tiles retris-tile-size retris-scaling-factor ?a))
(benchmark 100 ; 10.85s/4.3s
  '(retris-render-board retris-board-body retris-board-pixel-width 0 0
                        retris-pieces retris-tiles retris-tile-size
                        retris-scaling-factor retris-board))

(define-derived-mode retris-mode special-mode "Retris"
  "A XPM game."
  (buffer-disable-undo)
  (unless retris-timer
    (setq retris-timer (run-at-time nil 0.05 'retris-redraw-board))))

(defun retris ()
  (interactive)
  (with-current-buffer (get-buffer-create "*retris*")
    (retris-mode))
  (display-buffer "*retris*"))
