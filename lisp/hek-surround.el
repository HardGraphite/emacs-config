;;; hek-surround.el --- Edit brackets or quotes -*- lexical-binding: t -*-

;;; Commentary:

;; Commands to add or remove text surroundings like brackets and quotes.

;;; Code:

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

(provide 'hek-surround)
;;; hek-surround.el ends here
