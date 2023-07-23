;;; Elisp snippets.

;; Simplified comments and basic components for a single file package.
(pkgfile
";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " --- " p " -*- lexical-binding: t; -*-\n\n"
";;; Commentary:\n\n;; " p "\n\n"
";;; Code:\n\n" q "\n\n"
"(provide '"  (file-name-base (or (buffer-file-name) (buffer-name))) ")\n"
";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here\n"
)

;; Auto load.
(autoload
";;;###autoload"
)
