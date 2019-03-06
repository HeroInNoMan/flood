;;; flood.el --- Famous game of Flood! -*- lexical-binding: t -*-

;; Author: Arthur Léothaud
;; Maintainer: Arthur Léothaud
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: game


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is an attempt at a LISP version of the famous flood game.
;; The aim is to flood the board with one single color in as few moves as
;; possible. Click on a square to switch to that color.

;;; Code:

(define-derived-mode flood-mode special-mode "flood-mode"
  (define-key flood-mode-map (kbd "b") '(lambda () "" (interactive) (flood-change 0)))
  (define-key flood-mode-map (kbd "r") '(lambda () "" (interactive) (flood-change 1)))
  (define-key flood-mode-map (kbd "y") '(lambda () "" (interactive) (flood-change 2)))
  (define-key flood-mode-map (kbd "g") '(lambda () "" (interactive) (flood-change 3)))
  (define-key flood-mode-map (kbd "p") '(lambda () "" (interactive) (flood-change 4)))
  (define-key flood-mode-map (kbd "o") '(lambda () "" (interactive) (flood-change 5))))

;;;###autoload
(defun flood-game () "Start playing Flood."
       (interactive)
       (switch-to-buffer "flood")
       (flood-mode)
       (text-scale-adjust 3)
       (flood-init))

(require 'cl-lib)

(defface flood-face-0 '((t . (:background "blue" :foreground "blue"))) "Face for the 0th color" :group 'flood-faces)
(defface flood-face-1 '((t . (:background "red" :foreground "red"))) "Face for the 1st color" :group 'flood-faces)
(defface flood-face-2 '((t . (:background "yellow" :foreground "yellow"))) "Face for the 2nd color" :group 'flood-faces)
(defface flood-face-3 '((t . (:background "green" :foreground "green"))) "Face for the 3th color" :group 'flood-faces)
(defface flood-face-4 '((t . (:background "purple" :foreground "purple"))) "Face for the 4th color" :group 'flood-faces)
(defface flood-face-5 '((t . (:background "orange" :foreground "orange"))) "Face for the 5th color" :group 'flood-faces)

(defface flood-face-button-0 '((t . (:background "blue" :foreground "white" :weight bold))) "Face for the button of the 0th color" :group 'flood-faces)
(defface flood-face-button-1 '((t . (:background "red" :foreground "white" :weight bold))) "Face for the button of the 1st color" :group 'flood-faces)
(defface flood-face-button-2 '((t . (:background "yellow" :foreground "black" :weight bold))) "Face for the button of the 2nd color" :group 'flood-faces)
(defface flood-face-button-3 '((t . (:background "green" :foreground "black" :weight bold))) "Face for the button of the 3th color" :group 'flood-faces)
(defface flood-face-button-4 '((t . (:background "purple" :foreground "white" :weight bold))) "Face for the button of the 4th color" :group 'flood-faces)
(defface flood-face-button-5 '((t . (:background "orange" :foreground "black" :weight bold))) "Face for the button of the 5th color" :group 'flood-faces)

(defvar flood-board nil "The actual board.")
(defvar flood-flooded-cells nil "A matrix with the already flooded cells.")
(defvar flood-moves 0 "The number of moves performed by the player.")
(defvar flood-max-moves 22 "The number of moves allowed before loosing the game.")

(defvar flood-rows 12 "Board height.")
(defvar flood-columns 12 "Board width.")

(defun flood-init ()
  "Initialize the board with random colors."
  (setq flood-board (make-vector (* flood-rows flood-columns) nil))
  (setq flood-flooded-cells (make-vector (* flood-rows flood-columns) nil))
  (aset flood-flooded-cells 0 t) ;; flood the first cell
  (flood-populate-board)
  (flood-init-flooded-cells)
  (setq flood-moves 0)
  (flood-draw-board))

(defun flood-populate-board ()
  "Populate the board with random values."
  (dotimes (row flood-rows)
    (dotimes (col flood-columns)
      (flood-set-cell row col (number-to-string (random 6))))))

(defun flood-init-flooded-cells ()
  "Initialize already flooded cells at beginning of the game."
  (flood-try-to-flood-neighbors 0 0 (string-to-number (flood-get-cell 0 0))))

(defun flood-set-cell (row col val)
  "Set the value in (ROW, COL) to VAL."
  (aset flood-board (flood-get-index row col) val))

(defun flood-get-face (value)
  "Get face for VALUE."
  (intern (concat "flood-face-" value)))

(defun flood-get-index (row col)
  "Get the index in the board for (ROW, COL)."
  (+ (* row flood-columns) col))

(defun flood-cell-flooded-p (row col)
  "Return t if cell (ROW, COL) is flooded, nil otherwise."
  (when (and (>= row 0)
             (< row flood-rows)
             (>= col 0)
             (< col flood-columns))
    (elt flood-flooded-cells (flood-get-index row col))))

(defun flood-do-flood-cell (row col)
  "Mark the cell (ROW COL) as flooded."
  (aset flood-flooded-cells (flood-get-index row col) t))

(defun flood-draw-board ()
  "Draw the flood board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "\n")

    ;; for each row
    (dotimes (row flood-rows)
      (insert " ")
      ;; print the cells of each line
      (dotimes (col flood-columns)
        (let* ((val (flood-get-cell row col)))
          (insert (propertize (concat "  ") 'face (flood-get-face val) 'pointer 'finger))))
      (insert "\n"))

    ;; display number of moves
    (insert (concat "\n Moves: " (number-to-string flood-moves) "/" (number-to-string flood-max-moves) "\n\n"))

    ;; check if grid successful
    (cond ((flood-finished-p)
           (progn
             (insert (concat " Congratulations,\n you won in " (number-to-string flood-moves) " moves!\n Now go back to work.\n"))
             (when (y-or-n-p "Try again? ")
               (flood-init))))
          ((>= flood-moves flood-max-moves)
           (progn
             (insert " Alas, you lost.")
             (when (y-or-n-p "Try again? ")
               (flood-init))))
          (t (progn
               (insert " Controls:\n")
               (insert (concat " " (propertize " b " 'face 'flood-face-button-0 'pointer 'finger)) "\n")
               (insert (concat " " (propertize " r " 'face 'flood-face-button-1 'pointer 'finger)) "\n")
               (insert (concat " " (propertize " y " 'face 'flood-face-button-2 'pointer 'finger)) "\n")
               (insert (concat " " (propertize " g " 'face 'flood-face-button-3 'pointer 'finger)) "\n")
               (insert (concat " " (propertize " p " 'face 'flood-face-button-4 'pointer 'finger)) "\n")
               (insert (concat " " (propertize " o " 'face 'flood-face-button-5 'pointer 'finger)) "\n\n"))))))

(defun flood-get-cell (row col)
  "Get the value in (ROW, COL)."
  (when (and (>= row 0)
             (< row flood-rows)
             (>= col 0)
             (< col flood-columns))
    (elt flood-board (flood-get-index row col))))

(defun flood-try-to-flood-neighbors (row col color)
  "Try to recursively propagate flood to cells adjacent to (ROW, COL) if they match COLOR."
  (progn
    (when (and (> col 0) ;; if cell on the left exists
               (not (flood-cell-flooded-p row (1- col))) ;; and it is not flooded
               (string-equal (number-to-string color) (flood-get-cell row (1- col)))) ;; and is the same color
      (progn
        (flood-do-flood-cell row (1- col)) ;; then flood it
        (flood-try-to-flood-neighbors row (1- col) color))) ;; and recursively try to flood neighbors

    (when (and (> flood-columns col) ;; if cell on the right exists
               (not (flood-cell-flooded-p row (1+ col))) ;; and it is not flooded
               (string-equal (number-to-string color) (flood-get-cell row (1+ col)))) ;; and it is the same color
      (progn
        (flood-do-flood-cell row (1+ col)) ;; then flood it
        (flood-try-to-flood-neighbors row (1+ col) color))) ;; and recursively try to flood neighbors

    (when (and (> row 0) ;; if cell above exists
               (not (flood-cell-flooded-p (1- row) col)) ;; and it is not flooded
               (string-equal (number-to-string color) (flood-get-cell (1- row) col))) ;; and it is the same color
      (progn
        (flood-do-flood-cell (1- row) col) ;; then flood it
        (flood-try-to-flood-neighbors (1- row) col color))) ;; and recursively try to flood neighbors

    (when (and (> flood-rows row) ;; if cell below exists
               (not (flood-cell-flooded-p (1+ row) col)) ;; and it is not flooded
               (string-equal (number-to-string color) (flood-get-cell (1+ row) col))) ;; and it is the same color
      (progn
        (flood-do-flood-cell (1+ row) col) ;; then flood it
        (flood-try-to-flood-neighbors (1+ row) col color))))) ;; and recursively try to flood neighbors

(defun flood-finished-p ()
  "Return t if only one color left on the board, nil otherwise."
  (not (cl-position nil flood-flooded-cells)))

(defun flood-change (color)
  "Play a move, changing the color of the first cell to COLOR."
  ;; change color of already flooded cells.
  (dotimes (row flood-rows)
    (dotimes (col flood-columns)
      (when (flood-cell-flooded-p row col)
        (flood-set-cell row col (number-to-string color)))))

  ;; try to flood neighbors of flooded cells
  (dotimes (row flood-rows)
    (dotimes (col flood-columns)
      (when (flood-cell-flooded-p row col)
        (flood-try-to-flood-neighbors row col color))))

  ;; increment move count
  (setq-local flood-moves (1+ flood-moves))

  ;; redraw the board
  (flood-draw-board))

(provide 'flood)

;;; flood.el ends here
