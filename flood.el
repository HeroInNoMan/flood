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

(define-derived-mode flood-mode special-mode "flood-mode")

;;;###autoload
(defun flood-game () "Start playing Flood."
       (interactive)
       (switch-to-buffer "flood")
       (flood-mode)
       (flood-init))

(defface flood-face-0  '((t . (:background "blue" :foreground "white"))) "Face for the 0th color" :group 'flood-faces)
(defface flood-face-1  '((t . (:background "red" :foreground "white"))) "Face for the 1st color" :group 'flood-faces)
(defface flood-face-2  '((t . (:background "yellow" :foreground "black"))) "Face for the 2nd color" :group 'flood-faces)
(defface flood-face-3 '((t . (:background "green" :foreground "black"))) "Face for the 3th color" :group 'flood-faces)
(defface flood-face-4 '((t . (:background "purple" :foreground "white"))) "Face for the 4th color" :group 'flood-faces)
(defface flood-face-5 '((t . (:background "orange" :foreground "black"))) "Face for the 5th color" :group 'flood-faces)



(defvar flood-board nil "The actual board.")
(defvar flood-moves 0 "The number of moves performed by the player.")
(defvar flood-max-moves 21 "The number of moves allowed before loosing the game.")

(defvar flood-rows 15 "Board hight.")
(defvar flood-columns 15 "Board width.")


(defun flood-init ()
  "Initialize the board with random colors."
  (setq flood-board (make-vector (* flood-rows flood-columns) "0"))
  (flood-populate-board)
  (setq flood-moves 0)
  (setq flood-max-moves 21)
  (flood-draw-board))

(defun flood-populate-board ()
  "Populate the board with random values."
  (dotimes (num (* flood-rows flood-columns))
    (flood-set-cell num (number-to-string (random 6)))))

(defun flood-set-cell (index val)
  "Set the value in INDEX to VAL."
  (aset flood-board index val))

(defun flood-get-face (value)
  "Get face for VALUE."
  (intern (concat "flood-face-" value)))

(defun flood-get-index (row col)
  "Get the index in the board for (ROW, COL)."
  (+ (* row flood-columns) col))

(defun flood-draw-board ()
  "Draw the flood board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (text-scale-adjust 3)

    (insert "\n\n")

    ;; print the separator on top
    (insert "+")
    (dotimes (col flood-columns)
      (insert "--"))
    (insert "+\n")

    ;; for each row
    (dotimes (row flood-rows)
      (insert "|")
      ;; print the cells of each line
      (dotimes (col flood-columns)
        (insert (propertize "  " 'face (flood-get-face (flood-get-cell row col)))))
      (insert "|\n"))
    ;;print the separator at the bottom
    (insert "+")
    (dotimes (col flood-columns)
      (insert "--"))
    (insert "+\n\n")))

  (defun flood-get-cell (row col)
    "Get the value in (ROW, COL)."
    (elt flood-board (flood-get-index row col)))

  (provide 'flood)

;;; flood.el ends here
