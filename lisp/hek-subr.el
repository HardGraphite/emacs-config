;;; hek-subr.el --- hek's subroutines -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'pixel-scroll)


;;; Text manipulation.

(defun hek-center-line (&optional line-number)
  "Center the line by adding line prefix property."
  (save-excursion
    (when line-number
      (goto-char (point-min))
      (forward-line (1- line-number)))
    (let* ((beg-pos (line-beginning-position))
           (end-pos (line-end-position))
           (half-width (/ (car (window-text-pixel-size nil beg-pos end-pos))
                          (frame-char-width)
                          2))
           (padding (propertize " "
                                'display
                                `(space :align-to (- center ,half-width)))))
      (add-text-properties beg-pos end-pos
                           `(line-prefix ,padding indent-prefix ,padding)))))


;;; Text surrounding (wrapping).

(defvar hek-surround-pairs
  '((?\' . ?\')
    (?\" . ?\")
    (?\` . ?\`)
    (?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?<  . ?> )
    (?*  . ?* )
    (?_  . ?_ )
    (?$  . ?$ ))
  "Surrounding pairs.")

(defun hek-surround-region (region pair)
  "Add surrounding PAIR to the REGION. Both REGION and PAIR are cons. REGION is
the beginning and end points; PAIR is the left and write characters or strings.
When called interactively, REGION is the selected region; PAIR is the inferred
from `last-command-event' and `hek-surround-pairs'."
  (interactive
   (list (if (use-region-p)
             (cons (region-beginning) (region-end))
           (cons (point) (point)))
         (let ((key last-command-event))
           (unless (characterp key)
             (setq key (event-basic-type key)))
           (or (assq key hek-surround-pairs)
               (rassq key hek-surround-pairs)
               (let* ((left (read-string "left: " nil t))
                      (right (read-string "right: " left t)))
                 (cons left right))))))
  (save-excursion
    (goto-char (cdr region))
    (insert (cdr pair))
    (goto-char (car region))
    (insert (car pair))
    (- (point) (car region))))

(defun hek-unsurround-region (region pair)
  "Remove surrounding at the beginning and end of REGION. Both REGION and PAIR
are cons. REGION is the beginning and end points; PAIR is the number of
characters to delete at the beginning and end. When called interactively,
REGION is the selected region; PAIR is `(1 . 1)' if the first and last
characters are in `hek-surround-pairs'."
  (interactive
   (list (if (use-region-p)
             (cons (region-beginning) (region-end))
           (error "no region is active"))
         (if-let* ((the-pair (assq (char-after (region-beginning)) hek-surround-pairs))
                   (pair-matched (eq (cdr the-pair) (char-before (region-end)))))
             (cons 1 1)
           (cons (read-number "left count: " nil t)
                 (read-number "right count: " nil t)))))
  (unless (and (< (car region) (cdr region))
               (<= (+ (car pair) (cdr pair)) (- (cdr region) (car region))))
    (error "unexpected region and pair: `%S', `%S'" region pair))
  (save-excursion
    (goto-char (cdr region))
    (delete-char (- (cdr pair)))
    (goto-char (car region))
    (delete-char (car pair))
    (- (car region) (point))))


;;; Scrolling.

(defun hek-scroll-up (&optional lines)
  "Scroll upwards by LINES lines or one page."
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-up)))

(defun hek-scroll-down (&optional lines)
  "Scroll downwards by LINES lines or one page."
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-down))



(provide 'hek-subr)
;;; hek-subr.el ends here
