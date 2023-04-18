;; --- LLVM coding style guidelines in emacs -*- lexical-binding: t -*-

(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

(defconst llvm-c-style
  '("gnu"
    (fill-column . 80)
    (c++-indent-level . 2)
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-offsets-alist
     . ((arglist-intro . ++)
        (innamespace . 0)
        (member-init-intro . ++)
        (statement-cont . llvm-lineup-statement)))))

(require 'cc-styles)
(c-add-style "llvm" llvm-c-style)

(provide 'llvm-c-style)
