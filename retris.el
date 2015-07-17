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


;; variables

(defvar retris-board nil
  "Board grid.")

(defvar retris-old-board nil
  "Previous snapshot of the board grid.")

(defconst retris-pieces
  ;; TODO add flash
  ;; TODO add ghost piece
  '((?t  :tile-char ?c :coordinates [[ 0 0] [1 0] [2 0] [1 1]])
    (?j  :tile-char ?b :coordinates [[ 0 0] [1 0] [2 0] [2 1]])
    (?z  :tile-char ?a :coordinates [[ 0 0] [1 0] [1 1] [2 1]])
    (?o  :tile-char ?c :coordinates [[ 0 0] [1 0] [0 1] [1 1]])
    (?s  :tile-char ?b :coordinates [[ 1 0] [2 0] [0 1] [1 1]])
    (?l  :tile-char ?a :coordinates [[ 0 0] [1 0] [2 0] [0 1]])
    (?i  :tile-char ?c :coordinates [[-1 0] [0 0] [1 0] [2 0]])
    (?\s :tile-char ?x))
  "Alist of plists for piece chars.")

(defun retris-tile-char-lookup (piece-char)
  "Associate PIECE-CHAR with the respective tile char."
  (plist-get (cdr (assoc piece-char retris-pieces)) :tile-char))

(defun retris-coordinates-lookup (piece-char)
  (plist-get (cdr (assoc piece-char retris-pieces)) :coordinates))

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


;; rendering

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


;; board manipulation

(defun retris-empty-board ()
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
    (while (< i size)
      (aset result i (retris-add-to-coordinate (aref coordinates i) vector))
      (setq i (1+ i)))
    result))

(defun retris-board-set-coordinates (coordinates filler)
  "Set specified COORDINATES to FILLER on the board."
  (dotimes (i (length coordinates))
    (let ((xy (aref coordinates i)))
      (aset (aref retris-board (aref xy 1))
            (aref xy 0) filler))))

(defconst retris-board-insertion-coordinate [4 0])
(defvar retris-board-current-piece-coordinate nil)
(defvar retris-board-current-piece-char ?t)

(defun retris-fill-piece (xy piece-char &optional erase)
  "Commit a piece to the board.
XY is the insertion coordinate vector, PIECE-CHAR is used to
look up the piece, If ERASE is non-nil, the coordinates of the
given piece are blanked out instead."
  (let ((coordinates (retris-coordinates-lookup piece-char)))
    (dotimes (i (length coordinates))
      (let ((x (+ (aref (aref coordinates i) 0) (aref xy 0)))
            (y (+ (aref (aref coordinates i) 1) (aref xy 1))))
        (aset (aref retris-board y) x (if erase ?\s piece-char))))))

(defun retris-draw-current-piece ()
  "Draw the current piece to the board."
  (retris-fill-piece retris-board-current-piece-coordinate
                     retris-board-current-piece-char))

(defun retris-erase-current-piece ()
  "Erase the current piece from the board."
  (retris-fill-piece retris-board-current-piece-coordinate
                     retris-board-current-piece-char t))

(defun retris-board-insert-piece ()
  "Insert the current piece at the top of the board"
  (interactive)
  (retris-fill-piece retris-board-insertion-coordinate
                     retris-board-current-piece-char)
  (setq retris-board-current-piece-coordinate retris-board-insertion-coordinate
        retris-dirty-p t))

(defun retris-board-move-piece (vector)
  "Move the current piece by VECTOR."
  (let* ((old-coordinates
          (retris-add-to-coordinates (retris-coordinates-lookup
                                      retris-board-current-piece-char)
                                     retris-board-current-piece-coordinate))
         (new-coordinates (retris-add-to-coordinates old-coordinates vector)))
    (retris-board-set-coordinates old-coordinates ?\s)
    (if (and (not (retris-board-coordinates-out-of-bounds-p new-coordinates))
             (retris-board-coordinates-free-p new-coordinates))
        (progn
          (setq retris-board-current-piece-coordinate
                (retris-add-to-coordinate retris-board-current-piece-coordinate
                                          vector))
          (retris-board-set-coordinates new-coordinates
                                        retris-board-current-piece-char))
      (retris-board-set-coordinates old-coordinates
                                    retris-board-current-piece-char))
    (setq retris-dirty-p t)))

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


;; frontend

(defun retris-reset ()
  "Reset the game to its initial state."
  (interactive)
  (when retris-timer
    (cancel-timer retris-timer))
  (unless retris-old-board
    (setq retris-old-board (retris-empty-board)))
  (setq retris-board-current-piece-coordinate retris-board-insertion-coordinate
        retris-timer (run-at-time nil (/ 1.0 60) 'retris-redraw-board)
        retris-board (retris-empty-board)
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
(define-key retris-mode-map (kbd "i") 'retris-board-insert-piece)
(define-key retris-mode-map (kbd "j") 'retris-board-move-piece-down)
(define-key retris-mode-map (kbd "h") 'retris-board-move-piece-left)
(define-key retris-mode-map (kbd "l") 'retris-board-move-piece-right)

(defun retris ()
  "Start a game of Retris!"
  (interactive)
  (with-current-buffer (get-buffer-create "*retris*")
    (retris-mode))
  (display-buffer "*retris*"))

(provide 'retris)
;;; retris.el ends here
