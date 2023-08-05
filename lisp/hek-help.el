;;; hek-help.el --- help functions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun hek-help-echo (items &optional separator)
  "Print ITEMS, a list of strings or `nil's, to echo area."
  (message "%s" (string-join
                 (remove nil items)
                 (or separator (propertize " | " 'face 'shadow)))))

;;;###autoload
(defun hek-describe-buffer-file ()
  (interactive)
  (hek-help-echo
   (list
    ;; file or buffer name
    (propertize (or buffer-file-truename (buffer-name)) 'face '(italic))
    ;; buffer size
    (concat
     (let ((sz (buffer-size)))
       (cond ((<= sz 999) (concat (number-to-string sz) " B"))
             ((<= sz 99999) (format "%.1f KiB" (/ sz 1000.0)))
             (t (format "%.1f MiB" (/ sz 1000000.0)))))
     ", "
     (number-to-string (count-lines (point-min) (point-max)))
     " ln"
     )
    ;; coding system and end-of-line type
    (concat
     (let ((cs (coding-system-plist buffer-file-coding-system)))
       (upcase
        (symbol-name
         (if (memq (plist-get cs :category)
                   '( coding-category-undecided coding-category-utf-8 ))
             'utf-8 (plist-get cs :name)))))
     " "
     (pcase (coding-system-eol-type buffer-file-coding-system)
       (0 "LF") (1 "CRLF") (2 "CR") (_ "LF?"))
     )
    ;; indentation
    (let (type size)
      (if indent-tabs-mode
          (setq type "TAB" size tab-width)
        (setq type "SPC" size 0)
        (when (boundp 'editorconfig-indentation-alist)
          (setq size
                (symbol-value
                 (seq-find
                  (lambda (var) (and var (boundp var) (symbol-value var)))
                  (cdr (assoc major-mode editorconfig-indentation-alist)) 'tab-width)))))
      (concat type " " (number-to-string size)))
    ;; version control
    (when (and vc-mode buffer-file-name)
      (concat
       (string-trim (substring-no-properties vc-mode))
       " "
       (capitalize
         (symbol-name
          (vc-state buffer-file-name (vc-backend buffer-file-name))))))
    )))

(provide 'hek-help)
;;; hek-help.el ends here
