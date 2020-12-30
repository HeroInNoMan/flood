;;; flood.el --- Famous game of Flood! -*- lexical-binding: t -*-

;; Author: Arthur Léothaud
;; Maintainer: Arthur Léothaud
;; Version: 0.0.1
;; Package-Requires: (cl-lib)
;; Homepage: homepage
;; Keywords: game flood


;; This file is not part of GNU Emacs

;; DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;; Version 2, December 2004

;; Copyright (C) 2020 Arthur Léothaud <arthur@leothaud.eu>

;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.

;; DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;; TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

;; 0. You just DO WHAT THE FUCK YOU WANT TO.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is an attempt at a LISP version of the famous flood game.
;; The aim is to flood the board with one single color in as few moves as
;; possible. Click on a square to switch to that color.

;;; Code:

(require 'cl-lib)

(define-derived-mode flood-mode special-mode "flood-mode"
  (define-key flood-mode-map (kbd "b") '(lambda () "" (interactive) (flood-change 0)))
  (define-key flood-mode-map (kbd "r") '(lambda () "" (interactive) (flood-change 1)))
  (define-key flood-mode-map (kbd "y") '(lambda () "" (interactive) (flood-change 2)))
  (define-key flood-mode-map (kbd "g") '(lambda () "" (interactive) (flood-change 3)))
  (define-key flood-mode-map (kbd "p") '(lambda () "" (interactive) (flood-change 4)))
  (define-key flood-mode-map (kbd "o") '(lambda () "" (interactive) (flood-change 5)))
  (define-key flood-mode-map (kbd "n") 'flood-init)
  (define-key flood-mode-map (kbd "<SPC>") 'flood-restart-game)
  (define-key flood-mode-map (kbd "+") 'flood-replay-move)
  (define-key flood-mode-map (kbd "u") 'flood-undo-move))

;;;###autoload
(defun flood-game () "Start playing Flood."
       (interactive)
       (switch-to-buffer "*flood*")
       (flood-mode)
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
(defface flood-face-button-undo '((t . (:background "white" :foreground "black" :weight bold))) "Face for the undo button" :group 'flood-faces)

(defvar flood-board nil
  "The matrix containing the values representing the colors.")

(defvar flood-board-backup nil
  "The backup of the board, for undoing purposes.")

(defvar flood-flooded-cells nil
  "A matrix representing which cells are already flooded.")

(defvar flood-move-count 0
  "The number of moves performed by the player.")

(defvar flood-max-moves 20
  "The number of moves allowed before losing the game.")

(defvar flood-rows 12
  "The board height.")

(defvar flood-columns 12
  "The board width.")

(defvar flood-recorded-moves nil
  "The list of moves performed in the current game.")

(defun flood-init (&optional size)
  "Initialize the game with board of size SIZE."
  (interactive)
  (when (number-or-marker-p size)
    (progn (setq flood-rows size)
           (setq flood-columns size)))
  (setq flood-board (make-vector (* flood-rows flood-columns) nil))
  (setq flood-flooded-cells (make-vector (* flood-rows flood-columns) nil))
  (flood-populate-board)
  (flood-init-flooded-cells)
  (setq flood-board-backup (copy-tree flood-board t))
  (setq flood-move-count 0)
  (setq flood-recorded-moves (make-vector flood-max-moves nil))
  (flood-draw-board))

(defun flood-populate-board ()
  "Populate the board with random colors."
  (dotimes (row flood-rows)
    (dotimes (col flood-columns)
      (flood-set-cell row col (random 6)))))

(defun flood-init-flooded-cells ()
  "Initialize already flooded cells at the beginning of the game."
  (aset flood-flooded-cells 0 t) ;; flood the first cell
  (flood-try-to-flood-neighbors 0 0 (flood-get-cell 0 0)))

(defun flood-restart-game ()
  "Restart current game."
  (interactive)
  (setq flood-flooded-cells (make-vector (* flood-rows flood-columns) nil))
  (setq flood-board (copy-tree flood-board-backup t))
  (flood-init-flooded-cells)
  (setq flood-move-count 0)
  (flood-draw-board))

(defun flood-undo-move (&optional n)
  "Undo N previous move."
  (interactive)
  (let* ((number (if (number-or-marker-p n) n 1))
         (nb-moves (- (1+ flood-move-count) number)))
    (flood-restart-game)
    (dotimes (i nb-moves)
      (flood-replay-move i))))

(defun flood-replay-move (&optional n)
  "Replay the Nth move."
  (interactive)
  (let* ((number (if (number-or-marker-p n) n 1))
         (move (elt flood-recorded-moves number)))
    ;; record the move in the right place

    ;; replay the move
    (if (and number (> number 0))
        (let* (color-to-replay)
          (setq color-to-replay (elt flood-recorded-moves number))
          (flood-change color-to-replay))
      (when (> flood-move-count 1)
        (flood-replay-move (1- flood-move-count))))))

(defun flood-set-cell (row col val)
  "Set the value in (ROW, COL) to VAL."
  (aset flood-board (flood-get-index row col) val))

(defun flood-get-face (value)
  "Get face for VALUE."
  (intern (concat "flood-face-" (number-to-string value))))

(defun get-color-name (value)
  "Get the color name for VALUE."
  (cond ((eq 0 value) "blue")
        ((eq 1 value) "red")
        ((eq 2 value) "yellow")
        ((eq 3 value) "green")
        ((eq 4 value) "purple")
        ((eq 5 value) "orange")))

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
      (insert "  ")
      ;; print the cells of each line
      (dotimes (col flood-columns)
        (let* ((val (flood-get-cell row col))
               (face (flood-get-face val)))
          (flood-insert-control-button "██" face 'flood-change val)))
      (insert "\n"))

    ;; display number of moves
    (insert (concat "\n  Moves: " (number-to-string flood-move-count) "/" (number-to-string flood-max-moves) "\n\n"))

    ;; check if grid successful
    (cond ((flood-finished-p)
           (progn
             (insert (concat "  Congratulations,\n you won in " (number-to-string flood-move-count) " moves!\n Now go back to work.\n"))
             (when (y-or-n-p "Try again? ")
               (flood-init))))
          ((>= flood-move-count flood-max-moves)
           (progn
             (insert "  Alas, you lost.")
             (when (y-or-n-p "Try again? ")
               (flood-init))))
          (t (progn
               (insert "  Controls:\n  ")
               (flood-insert-control-button " b " 'flood-face-button-0 'flood-change 0)
               (flood-insert-control-button " r " 'flood-face-button-1 'flood-change 1)
               (flood-insert-control-button " y " 'flood-face-button-2 'flood-change 2)
               (flood-insert-control-button " g " 'flood-face-button-3 'flood-change 3)
               (flood-insert-control-button " p " 'flood-face-button-4 'flood-change 4)
               (flood-insert-control-button " o " 'flood-face-button-5 'flood-change 5)
               (insert "\n\n  ")
               (flood-insert-control-button " u " 'flood-face-button-undo 'flood-undo-move)
               (insert " undo\t")
               (flood-insert-control-button " + " 'flood-face-button-undo 'flood-replay-move)
               (insert " redo\n\n  ")
               (flood-insert-control-button " <SPC> " 'flood-face-button-undo 'flood-restart-game)
               (insert " Restart game\n\n  Or click on a color to flood with it!"))))))

(defun flood-insert-control-button (label face action &optional action-params)
  "Insert button with LABEL, FACE and ACTION (with ACTION-PARAMS)."
  (insert-button label
                 'follow-link t
                 'help-echo nil
                 'face face
                 'mouse-face face
                 'action (lambda (button)
                           (if action-params
                               (funcall action action-params)
                             (funcall action)))))

(defun flood-get-cell (row col)
  "Get the value in (ROW, COL)."
  (when (and (>= row 0)
             (< row flood-rows)
             (>= col 0)
             (< col flood-columns))
    (elt flood-board (flood-get-index row col))))

(defun flood-try-to-flood-cell (row col color)
  "Try to recursively propagate flood to cell (ROW, COL) if same COLOR."
  (when (and (>= col 0)
             (>= row 0)
             (< col flood-columns)
             (< row flood-rows) ;; if cell exists
             (not (flood-cell-flooded-p row col)) ;; and it is not already flooded
             (eq color (flood-get-cell row col))) ;; and is the same color
    (progn
      (flood-do-flood-cell row col) ;; then flood it
      (flood-try-to-flood-neighbors row col color)))) ;; and recursively try to flood neighbors

(defun flood-try-to-flood-neighbors (row col color)
  "Try to propagate flood to cells adjacent to (ROW, COL) if they match COLOR."
  (flood-try-to-flood-cell row (1- col) color) ;; left
  (flood-try-to-flood-cell row (1+ col) color) ;; right
  (flood-try-to-flood-cell (1- row) col color) ;; up
  (flood-try-to-flood-cell (1+ row) col color)) ;; down

(defun flood-finished-p ()
  "Return t if only one color left on the board, nil otherwise."
  (not (cl-position nil flood-flooded-cells)))

(defun flood-change (color)
  "Play a move, changing the color of the first cell to COLOR."
  (unless (eq color (flood-get-cell 0 0))
    ;; change color of already flooded cells.
    (dotimes (row flood-rows)
      (dotimes (col flood-columns)
        (when (flood-cell-flooded-p row col)
          (flood-set-cell row col color))))

    ;; try to flood neighbors of flooded cells
    (dotimes (row flood-rows)
      (dotimes (col flood-columns)
        (when (flood-cell-flooded-p row col)
          (flood-try-to-flood-neighbors row col color))))

    ;; record move
    (aset flood-recorded-moves flood-move-count color)

    ;; increment move count
    (setq-local flood-move-count (1+ flood-move-count))

    ;; redraw the board
    (flood-draw-board)))

(provide 'flood)

;;; flood.el ends here
