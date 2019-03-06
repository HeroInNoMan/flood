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
  (define-key flood-mode-map (kbd "b") 'flood-change-0)
  (define-key flood-mode-map (kbd "r") 'flood-change-1)
  (define-key flood-mode-map (kbd "y") 'flood-change-2)
  (define-key flood-mode-map (kbd "g") 'flood-change-3)
  (define-key flood-mode-map (kbd "p") 'flood-change-4)
  (define-key flood-mode-map (kbd "o") 'flood-change-5))

;;;###autoload
(defun flood-game () "Start playing Flood."
       (interactive)
       (switch-to-buffer "flood")
       (flood-mode)
       (text-scale-adjust 3)
       (flood-init))

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
(defvar flood-moves 0 "The number of moves performed by the player.")
(defvar flood-max-moves 21 "The number of moves allowed before loosing the game.")

(defvar flood-rows 15 "Board height.")
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
    (insert "\n")

    ;; for each row
    (dotimes (row flood-rows)
      (insert " ")
      ;; print the cells of each line
      (dotimes (col flood-columns)
        (let* ((val (flood-get-cell row col)))
          (insert (propertize (concat "  ") 'face (flood-get-face val) 'pointer 'finger))))
      (insert "\n"))

    (insert "\n Controls:\n")
    (insert (concat " " (propertize " b " 'face 'flood-face-button-0 'pointer 'finger)) "\n")
    (insert (concat " " (propertize " r " 'face 'flood-face-button-1 'pointer 'finger)) "\n")
    (insert (concat " " (propertize " y " 'face 'flood-face-button-2 'pointer 'finger)) "\n")
    (insert (concat " " (propertize " g " 'face 'flood-face-button-3 'pointer 'finger)) "\n")
    (insert (concat " " (propertize " p " 'face 'flood-face-button-4 'pointer 'finger)) "\n")
    (insert (concat " " (propertize " o " 'face 'flood-face-button-5 'pointer 'finger)) "\n\n")))

(defun flood-get-cell (row col)
  "Get the value in (ROW, COL)."
  (elt flood-board (flood-get-index row col)))

(defun flood-change-0 () "Call with parameter." (interactive) (flood-change 0))
(defun flood-change-1 () "Call with parameter." (interactive) (flood-change 1))
(defun flood-change-2 () "Call with parameter." (interactive) (flood-change 2))
(defun flood-change-3 () "Call with parameter." (interactive) (flood-change 3))
(defun flood-change-4 () "Call with parameter." (interactive) (flood-change 4))
(defun flood-change-5 () "Call with parameter." (interactive) (flood-change 5))

(defun flood-change (num)
  "Play a move, changing the color of the first cell to NUM."
  (message "change to %s!" (number-to-string num)))

(provide 'flood)

;;; flood.el ends here
