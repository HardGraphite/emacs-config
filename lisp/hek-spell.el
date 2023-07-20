;;; hek-spell.el --- spell check -*- lexical-binding: t -*-

;;; Commentary:

;; This package provides a minor mode for a better spell check experience
;; based on builtin packages `flyspell' and `ispell'.

;;; Code:

(defconst hek-spell--flyspell-popup/operations
  '(("* Save word"        . save)
    ("* Accept (session)" . session)
    ("* Accept (buffer)"  . buffer)
    ("* Ignore"           . ignore)
    ("* Cancel"           . nil)))

(defun hek-spell--flyspell-popup/group (completion transform)
  (if transform
      completion
    (if (= ?* (string-to-char completion)) "Operation" "Correction")))

(defun hek-spell--flyspell-popup (event poss word)
  ;; This function overrides `flyspell-emacs-popup' and shall produce the first
  ;; argument for function `flyspell-do-correct'.
  (let* ((orig-word (car poss))
         (corrects (cl-copy-list (flyspell-sort (car (cdr (cdr poss))) word)))
         (collection (nconc corrects
                            (mapcar (lambda (x) (car x))
                                    hek-spell--flyspell-popup/operations)))
         (res (completing-read
               (concat orig-word " \u2192 ")
               (lambda (str pred act)
                 (if (eq act 'metadata)
                     `(metadata
                       (display-sort-function . identity)
                       (group-function . hek-spell--flyspell-popup/group))
                   (complete-with-action act collection str pred)))
               nil
               'confirm
               nil
               t)))
    (if (and res (= ?* (string-to-char res)))
        (cdr (assoc res hek-spell--flyspell-popup/operations))
      res)))

(define-minor-mode hek-spell-mode
  "A global minor mode for better spell check experience."
  :global t
  (if hek-spell-mode
      (progn
        (advice-add 'flyspell-emacs-popup :override #'hek-spell--flyspell-popup)
        )
  (advice-remove 'flyspell-emacs-popup #'hek-spell--flyspell-popup)
  ))

(provide 'hek-spell)
;;; hek-spell.el ends here
