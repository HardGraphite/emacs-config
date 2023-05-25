;;; hek-tempel-snippets.el --- snippets for TempEL. -*- lexical-binding: t -*-

;;; Commentary:

;; Load snippets from file for TempEl.

;;; Code:

(require 'tempel)

(defvar hek-tempel-snippets-dir
  (concat user-emacs-directory "snippets/")
  "Directory to find snippets.")

(defvar hek-tempel-snippets-file-affix
  '("tempel-" . ".eld")
  "(PREFIX . SUFFIX)")

(defvar hek-tempel-snippets--cache nil)

(defun hek-tempel-snippets--load (mode)
  (let ((file (concat hek-tempel-snippets-dir
                      (car hek-tempel-snippets-file-affix)
                      (string-remove-suffix "-mode" (symbol-name mode))
                      (cdr hek-tempel-snippets-file-affix))))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let (result
              object
              (buffer (current-buffer)))
          (while (setq object (ignore-error end-of-file (read buffer)))
            (when (listp object)
              (push object result)))
          result)))))

(defun hek-tempel-snippets ()
  "A TempEl templates loader."
  (let* ((this-mode major-mode))
    (if-let ((cached (assq this-mode hek-tempel-snippets--cache)))
        (cdr cached)
      (let ((tmpls (hek-tempel-snippets--load this-mode)))
        (push (cons this-mode tmpls) hek-tempel-snippets--cache)
        tmpls))))

(provide 'hek-tempel-snippets)
;;; hek-tempel-snippets.el ends here
