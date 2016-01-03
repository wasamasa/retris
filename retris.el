;;; retris.el --- Retro Tetris

;; Copyright (C) 2015 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/retris
;; Version: 0.0.1
;; Package-Requires: ((dash "2.11.0"))
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Here's a better Tetris game than the one Emacs comes with.  The
;; goal of this demonstration is to test how far one can go with XPM
;; graphics and what limitations you're bound to run into.

;;; Code:

(require 'dash)


;;; basics

(defvar retris-buffer-name "*retris*"
  "Buffer name used for the Retris buffer.")

(defvar retris-board nil
  "Board grid.")

(defvar retris-old-board nil
  "Previous snapshot of the board grid.")

(defconst retris-pieces
  ;; TODO add flash
  ;; TODO add ghost piece
  '((?t  :tile-char ?c :coordinates [[[-1  0] [ 0  0] [ 1  0] [ 0  1]]
                                     [[ 0 -1] [-1  0] [ 0  0] [ 0  1]]
                                     [[ 0 -1] [-1  0] [ 0  0] [ 1  0]]
                                     [[ 0 -1] [ 0  0] [ 1  0] [ 0  1]]])
    (?j  :tile-char ?b :coordinates [[[-1  0] [ 0  0] [ 1  0] [ 1  1]]
                                     [[ 0 -1] [ 0  0] [-1  1] [ 0  1]]
                                     [[-1 -1] [-1  0] [ 0  0] [ 1  0]]
                                     [[ 0 -1] [ 1 -1] [ 0  0] [ 0  1]]])
    (?z  :tile-char ?a :coordinates [[[-1  0] [ 0  0] [ 0  1] [ 1  1]]
                                     [[ 1 -1] [ 0  0] [ 1  0] [ 0  1]]])
    (?o  :tile-char ?c :coordinates [[[-1  0] [ 0  0] [-1  1] [ 0  1]]])
    (?s  :tile-char ?b :coordinates [[[ 0  0] [ 1  0] [-1  1] [ 0  1]]
                                     [[ 0 -1] [ 0  0] [ 1  0] [ 1  1]]])
    (?l  :tile-char ?a :coordinates [[[-1  0] [ 0  0] [ 1  0] [-1  1]]
                                     [[-1 -1] [ 0 -1] [ 0  0] [ 0  1]]
                                     [[ 1 -1] [-1  0] [ 0  0] [ 1  0]]
                                     [[ 0 -1] [ 0  0] [ 0  1] [ 1  1]]])
    (?i  :tile-char ?c :coordinates [[[-2  0] [-1  0] [ 0  0] [ 1  0]]
                                     [[ 0 -2] [ 0 -1] [ 0  0] [ 0  1]]])
    (?\s :tile-char ?x))
  "Alist of plists for piece chars.")

(defvar retris-bag nil
  "Bag of shuffled pieces to draw from.")

(defvar retris-bag-index 0
  "Index of the next shuffled piece to be drawn.
Once it goes beyond its maximum value, it should be reset to its
initial value and a new bag set up.")

(defun retris-bag ()
  "Return a bag of shuffled pieces.
This implements a Knuth shuffle."
  (let* ((pieces (vector 0 1 2 3 4 5 6))
         (i (1- (length pieces)))
         j)
    (while (>= i 1)
      (setq j (random (1+ i)))
      (let ((a (aref pieces i))
            (b (aref pieces j)))
        (aset pieces i b)
        (aset pieces j a))
      (setq i (1- i)))
    pieces))

(defun retris-random-piece ()
  "Pick a random piece char.
If `retris-bag' is empty, refill it with `retris-bag' first."
  (when (>= retris-bag-index (length retris-bag))
    (setq retris-bag-index 0
          retris-bag (retris-bag)))
  (let ((piece (car (nth (aref retris-bag retris-bag-index)
                         retris-pieces))))
    (setq retris-bag-index (1+ retris-bag-index))
    piece))

(defun retris-tile-char-lookup (piece-char)
  "Associate PIECE-CHAR with the respective tile char."
  (plist-get (cdr (assoc piece-char retris-pieces)) :tile-char))

(defun retris-rotation-lookup (piece-char rotation &optional ccw)
  "Returns next possible rotation for PIECE-CHAR and ROTATION.
If CCW is non-nil, look-up is done for counter-clockwise
rotation, otherwise clockwise rotation."
  (let* ((piece (cdr (assoc piece-char retris-pieces)))
         (coordinates (plist-get piece :coordinates)))
    (mod (+ rotation (if ccw -1 +1)) (length coordinates))))

(defun retris-coordinates-lookup (piece-char rotation)
  "Returns coordinates for PIECE-CHAR and ROTATION."
  (let* ((piece (cdr (assoc piece-char retris-pieces)))
         (coordinates (plist-get piece :coordinates)))
    (aref coordinates rotation)))

(defconst retris-tiles
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

(defconst retris-colors
  '((?^ . "white")
    (?a . "light")
    (?b . "dark")
    (?. . "black"))
  "Alist of color associations for tiles.")

(defconst retris-palette
  '(("white" . "#ffffff")
    ("light" . "#64b0ff")
    ("dark"  . "#4240ff")
    ("black" . "#000000"))
  "Default color palette for the XPM image.")

(defvar retris-board-width 10
  "Width of the board in tiles.")
(defvar retris-board-height 22
  "Height of the board in tiles.")
(defvar retris-board-row-skip 2
  "Number of rows to skip for rendering.")
(defvar retris-tile-size 8
  "Size of each tile in pixels.")
(defvar retris-scaling-factor 2
  "Integer scaling factor of the tiles.")

(defvar retris-board-pixel-width
  (* retris-board-width retris-tile-size retris-scaling-factor)
  "Width of the board image in pixels.")
(defvar retris-board-pixel-height
  (* (- retris-board-height retris-board-row-skip)
     retris-tile-size retris-scaling-factor)
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

(defvar retris-board-header nil
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

(defvar retris-board-body nil
  "XPM body of the board image.")


;;; scheduling

;; FIXME: Introduce an OFFSET field which is decremented until its
;; zero.  Events are only considered to be runif this field is zero.

(defvar retris-frame-length (/ 1.0 60)
  "Length of an individual frame.")
(defvar retris-time 0
  "Amount of ticks that have passed since starting the game.")
(defvar retris-events nil
  "List of scheduled events.
Each event is a three-element vector consisting of INTERVAL,
REMAINDER and FUNCTION.  When `retris-time' modulo INTERVAL
equals REMAINDER, FUNCTION is run by `retris-scheduler'.")

(defun retris-scheduled-tasks ()
  "Return a list of tasks to be run."
  (let (tasks)
    (dolist (event retris-events)
      (when (= (mod retris-time (aref event 0)) (aref event 1))
        (push (aref event 2) tasks)))
    tasks))

(defun retris-scheduler ()
  "Run scheduled tasks, redraw board and advance `retris-time'."
  (when (and retris-playing-p (get-buffer-window retris-buffer-name))
    (dolist (task (retris-scheduled-tasks))
      (funcall task))
    (retris-redraw-board)
    (setq retris-time (1+ retris-time))))


;;; rendering

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
(defvar retris-dirty-p nil
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
        (when (>= y retris-board-row-skip)
          (retris-render-tile x (- y retris-board-row-skip) tile-char))))
    (setq retris-old-board (copy-tree retris-board t)
          retris-dirty-p nil)
    (with-current-buffer retris-buffer-name
      (let ((inhibit-read-only t)
            (window (get-buffer-window retris-buffer-name)))
        (erase-buffer)
        (insert
         (propertize " " 'display (create-image (concat retris-board-header
                                                        retris-board-body)
                                                'xpm t
                                                :color-symbols retris-palette)
                     'point-entered (lambda (_old _new)
                                      (goto-char (point-max))))
         "\n")
        (set-window-point window (point-max))
        (deactivate-mark)))))


;;; board manipulation

(defun retris-empty-board ()
  "Return an empty board."
  (let ((board (make-vector retris-board-height nil)))
    (dotimes (i retris-board-height)
      (aset board i (make-vector retris-board-width ?\s)))
    board))

(defun retris-board-coordinate-out-of-bounds-p (xy)
  "Non-nil if the XY coordinate is outside the board."
  (or (< (aref xy 0) 0)
      (>= (aref xy 0) retris-board-width)
      (< (aref xy 1) 0)
      (>= (aref xy 1) retris-board-height)))

(defun retris-board-coordinates-out-of-bounds-p (coordinates)
  "Non-nil if any of the COORDINATES is outside the board."
  (-any? 'retris-board-coordinate-out-of-bounds-p
         ;; turn the coordinates vector into a list
         (append coordinates nil)))

(defun retris-board-coordinate-free-p (xy)
  "Non-nil if the XY coordinate is free on the board."
  (= (aref (aref retris-board (aref xy 1)) (aref xy 0))
     ?\s))

(defun retris-board-coordinates-free-p (coordinates)
  "Non-nil if all COORDINATES are free on the board."
  (-all? 'retris-board-coordinate-free-p
         (append coordinates nil)))

(defun retris-board-coordinates-ok-p (coordinates)
  "Non-nil if COORDINATES are both on the board and free."
  (and (not (retris-board-coordinates-out-of-bounds-p coordinates))
       (retris-board-coordinates-free-p coordinates)))

(defun retris-add-to-coordinate (coordinate vector)
  "Add VECTOR to COORDINATE."
  (vector (+ (aref coordinate 0) (aref vector 0))
          (+ (aref coordinate 1) (aref vector 1))))

(defun retris-add-to-coordinates (coordinates vector)
  "Add VECTOR to all COORDINATES.
Return a vector of altered coordinates."
  (let* ((size (length coordinates))
         (i 0)
         (result (make-vector size nil)))
    (dotimes (i size)
      (aset result i (retris-add-to-coordinate (aref coordinates i) vector)))
    result))

(defun retris-board-set-coordinates (coordinates filler)
  "Set specified COORDINATES to FILLER on the board."
  (dotimes (i (length coordinates))
    (let ((xy (aref coordinates i)))
      (aset (aref retris-board (aref xy 1))
            (aref xy 0) filler))))

(defconst retris-board-insertion-coordinate [5 2]
  "Insertion coordinate for a newly spawned piece.")
(defconst retris-board-initial-piece-rotation 0
  "Rotation for a newly spawned piece.")

(defvar retris-board-current-piece-coordinate nil
  "Coordinate of the currently moving piece.")
(defvar retris-board-current-piece-rotation nil
  "Rotation of the currently moving piece.")
(defvar retris-board-current-piece-char ?t
  "Piece character of the currently moving piece.")

(defun retris-board-insert-piece ()
  "Insert the current piece at the top of the board"
  (when retris-playing-p
    (setq retris-board-current-piece-coordinate retris-board-insertion-coordinate
          retris-board-current-piece-rotation retris-board-initial-piece-rotation)
    (retris-board-set-coordinates
     (retris-add-to-coordinates (retris-coordinates-lookup
                                 retris-board-current-piece-char
                                 retris-board-current-piece-rotation)
                                retris-board-current-piece-coordinate)
     retris-board-current-piece-char)
    (setq retris-dirty-p t)))

(defun retris-board-move-piece (vector)
  "Move the current piece by VECTOR."
  (when retris-playing-p
    (let* ((old-coordinates
            (retris-add-to-coordinates (retris-coordinates-lookup
                                        retris-board-current-piece-char
                                        retris-board-current-piece-rotation)
                                       retris-board-current-piece-coordinate))
           (new-coordinates (retris-add-to-coordinates old-coordinates vector))
           success)
      (retris-board-set-coordinates old-coordinates ?\s)
      (if (retris-board-coordinates-ok-p new-coordinates)
          (progn
            (setq retris-board-current-piece-coordinate
                  (retris-add-to-coordinate retris-board-current-piece-coordinate
                                            vector))
            (retris-board-set-coordinates new-coordinates
                                          retris-board-current-piece-char)
            (setq success t))
        (retris-board-set-coordinates old-coordinates
                                      retris-board-current-piece-char))
      (setq retris-dirty-p t)
      success)))

(defun retris-board-move-piece-down ()
  "Move the current piece down."
  (interactive)
  (retris-board-move-piece [0 1]))

(defun retris-board-move-piece-left ()
  "Move the current piece left."
  (interactive)
  (retris-board-move-piece [-1 0]))

(defun retris-board-move-piece-right ()
  "Move the current piece right."
  (interactive)
  (retris-board-move-piece [1 0]))

(defun retris-board-move-piece-bottom ()
  "Drop the current piece all the way to the bottom."
  (interactive)
  ;; FIXME this isn't ideal as you can see flicker from the individual
  ;; movements; it would be better to put that logic into the movement
  ;; commands and fix the abstraction of the movement code, too
  (while (retris-board-move-piece-down)))

(defun retris-board-rotate-piece (&optional ccw)
  "Rotate the current piece clockwise or counter-clockwise.
Rotate the current piece clockwise if CCW nil, otherwise
counter-clockwise."
  (when retris-playing-p
    (let* ((rotation
            (retris-rotation-lookup retris-board-current-piece-char
                                    retris-board-current-piece-rotation ccw))
           (old-coordinates
            (retris-add-to-coordinates (retris-coordinates-lookup
                                        retris-board-current-piece-char
                                        retris-board-current-piece-rotation)
                                       retris-board-current-piece-coordinate))
           (new-coordinates
            (retris-add-to-coordinates (retris-coordinates-lookup
                                        retris-board-current-piece-char
                                        rotation)
                                       retris-board-current-piece-coordinate))
           success)
      (retris-board-set-coordinates old-coordinates ?\s)
      (if (retris-board-coordinates-ok-p new-coordinates)
          (progn
            (setq retris-board-current-piece-rotation rotation)
            (retris-board-set-coordinates new-coordinates
                                          retris-board-current-piece-char)
            (setq success t))
        (retris-board-set-coordinates old-coordinates
                                      retris-board-current-piece-char))
      (setq retris-dirty-p t)
      success)))

(defun retris-board-rotate-piece-cw ()
  "Rotate the current piece clockwise."
  (interactive)
  (retris-board-rotate-piece))

(defun retris-board-rotate-piece-ccw ()
  "Rotate the current piece counter-clockwise"
  (interactive)
  (retris-board-rotate-piece t))

(defun retris-row-state (row)
  "Return the state of ROW.
It can be either of 'blank for a row of spaces, 'full for a row
of letters or 'mixed for anything else."
  (let ((i 0)
        (size (length row))
        blank-char
        full-char)
    ;; to determine whether a row is mixed, you only need to encounter
    ;; one blank and one non-blank char and can terminate the search
    (while (and (not (and blank-char full-char))
                (< i size))
      (let ((char (aref row i)))
        (if (= char ?\s)
            (setq blank-char t)
          (setq full-char t)))
      (setq i (1+ i)))
    (cond
     ((and blank-char full-char) 'mixed)
     (blank-char 'blank)
     (full-char 'full))))

(defun retris-compact-stack ()
  "Remove full rows from board and compact the stack.
This checks the rows of `retris-board' in reverse order, then
copies rows which are neither full nor empty to an empty board.
Returns the number of deleted rows."
  (interactive)
  (let ((new-board (retris-empty-board))
        (j (1- retris-board-height))
        (changed 0))
    (dotimes (i retris-board-height)
      (let* ((row (aref retris-board (1- (- retris-board-height i))))
             (state (retris-row-state row)))
        (cond
         ((eq state 'mixed)
          (aset new-board j row)
          (setq j (1- j)))
         ((eq state 'full)
          (setq changed (1+ changed))))))
    (setq retris-board new-board)
    changed))


;;; frontend

;; FIXME resetting should probably be distinct from starting a new game
(defun retris-reset ()
  "Reset the game to its initial state."
  (interactive)
  (when retris-timer
    (cancel-timer retris-timer))
  (unless retris-old-board
    (setq retris-old-board (retris-empty-board)))
  (setq retris-board-current-piece-coordinate retris-board-insertion-coordinate
        retris-board-current-piece-char (retris-random-piece)
        retris-board-header (retris-generate-xpm-header)
        retris-board-body (retris-generate-xpm-body)
        retris-time 0
        retris-timer (run-at-time nil retris-frame-length 'retris-scheduler)
        retris-board (retris-empty-board)
        retris-bag (retris-bag)
        retris-playing-p t
        retris-dirty-p t))

(defun retris-play-or-pause ()
  "Toggle play/pause state."
  (interactive)
  (setq retris-playing-p (not retris-playing-p)))

(define-derived-mode retris-mode special-mode "Retris"
  "A XPM game."
  (buffer-disable-undo)
  (retris-reset))

(define-key retris-mode-map (kbd "p") 'retris-play-or-pause)
(define-key retris-mode-map (kbd "g") 'retris-reset)
(define-key retris-mode-map (kbd "h") 'retris-board-move-piece-left)
(define-key retris-mode-map (kbd "j") 'retris-board-move-piece-down)
(define-key retris-mode-map (kbd "k") 'retris-board-rotate-piece-cw)
(define-key retris-mode-map (kbd "l") 'retris-board-move-piece-right)
(define-key retris-mode-map (kbd "z") 'retris-board-rotate-piece-ccw)
(define-key retris-mode-map (kbd "y") 'retris-board-rotate-piece-ccw)
(define-key retris-mode-map (kbd "x") 'retris-board-rotate-piece-cw)
(define-key retris-mode-map (kbd "SPC") 'retris-board-move-piece-bottom)
(define-key retris-mode-map (kbd "<left>") 'retris-board-move-piece-left)
(define-key retris-mode-map (kbd "<right>") 'retris-board-move-piece-right)
(define-key retris-mode-map (kbd "<down>") 'retris-board-move-piece-down)
(define-key retris-mode-map (kbd "<up>") 'retris-board-rotate-piece-cw)

(defun retris-advance-game ()
  "Try moving the current piece down.
If this fails, spawn a new piece."
  (when (not (retris-board-move-piece-down))
    (retris-compact-stack)
    (setq retris-board-current-piece-char (retris-random-piece))
    (retris-board-insert-piece)
    (setq retris-dirty-p t)))

(defun retris ()
  "Start a game of Retris!"
  (interactive)
  (with-current-buffer (get-buffer-create retris-buffer-name)
    (retris-mode)
    (retris-board-insert-piece)
    (sleep-for 0.5)
    (discard-input)
    (setq retris-events '([30 0 retris-advance-game])))
  (display-buffer retris-buffer-name))

(provide 'retris)
;;; retris.el ends here
