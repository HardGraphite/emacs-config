;;; hek-modeline.el --- A lightweight mode line. -*- lexical-binding: t -*-

;;; Commentary:

;; A lightweight Emacs mode line.

;;; Code:

(require 'all-the-icons)
(require 'cl-lib)

;;; === utilities ===

(defconst hek-modeline--thinspc
  (propertize " " 'display '((space :relative-width 0.5))))

(defconst hek-modeline--fatspc
  (propertize " " 'display '((space :relative-width 1.5))))

;;; === segment: indicators ===

(defvar hek-modeline--seg-indicators ;; list
  '(;; macro recording
    `(defining-kbd-macro ,(propertize " ● " 'face '(:inverse-video t)))
    "%n%e "))
;; meow state
(when (package-installed-p 'meow)
  (push '(meow-mode (:eval meow--indicator))
        hek-modeline--seg-indicators))

;;; === segment: buffer / file info ===

(defvar hek-modeline--seg-bufinfo ;; single
  '(:eval hek-modeline--bufinfo-cache))
(defvar-local hek-modeline--bufinfo-cache nil)
(defun hek-modeline--seg-bufinfo-init ()
  (setq hek-modeline--bufinfo-cache
        (propertize "%b " 'face '(all-the-icons-purple italic))))

(defvar hek-modeline--seg-fileinfo ;; single
  '(:eval (hek-modeline--seg-fileinfo)))
(defconst hek-modeline--seg-fileinfo-icons
  (vector (all-the-icons-material "lock" :face 'all-the-icons-red)
          (all-the-icons-material "save" :face 'all-the-icons-green)
          (all-the-icons-material "edit" :face 'all-the-icons-yellow)
          (all-the-icons-material "airplay")))
(defun hek-modeline--seg-fileinfo ()
  (cons (aref hek-modeline--seg-fileinfo-icons
              (if (buffer-modified-p) 2 (if buffer-read-only 0 1)))
        hek-modeline--bufinfo-cache))
(defun hek-modeline--seg-fileinfo-init ()
  (setq hek-modeline--bufinfo-cache ;; Reuse this variable.
        (list
         hek-modeline--thinspc
         ;; file icon
         (all-the-icons-icon-for-file buffer-file-name :v-adjust 0)
         hek-modeline--thinspc
         ;; remote icon
         (when (file-remote-p buffer-file-name)
           (aref hek-modeline--seg-fileinfo-icons 3))
         ;; in-project directory
         (when-let ((proj (project-current)))
           (concat
            (propertize
             (concat (project-name proj) "/")
             'face 'all-the-icons-lcyan)
            (propertize
             (substring
              (replace-regexp-in-string
               "/\\(.\\)[^/]+"
               "/\\1"
               (concat "/" (file-name-directory (file-relative-name buffer-file-name (project-root proj)))))
              1 nil)
             'face 'all-the-icons-dcyan)))
         ;; file name
         (propertize
          (concat (file-name-nondirectory buffer-file-name) " ")
          'face 'all-the-icons-cyan
          'help-echo buffer-file-truename)))
  (setq hek-modeline--bufinfo-cache
        (list (apply #'concat (remove nil hek-modeline--bufinfo-cache)))))

;;; === segment: cursor position ===

(defvar hek-modeline--seg-cursorpos ;; list
  '(;; location in buffer
    (-3 "%p")
    ;; line & column
    (9 " %l:%C")
    ;; marked region
    (mark-active
     (:eval (propertize
             (let ((p (point)) (m (marker-position (mark-marker))))
               (format " %dC %dL " (- p m) (count-lines p m)))
             'face 'region)))))

;;; === segment: minor modes ===

(defvar hek-modeline--seg-minormodes ;; list
  (list
   '(flymake-mode (:eval hek-modeline--flymake-cache))
   '(:eval hek-modeline--eglot-cache)))

(defconst hek-modeline--seg-minormodes-icons
  (vector (all-the-icons-material "check_circle" :face 'all-the-icons-dgreen)
          (all-the-icons-material "info_outline" :face 'all-the-icons-maroon)
          (all-the-icons-material "error_outline" :face 'all-the-icons-red)
          (all-the-icons-faicon "rocket" :face 'all-the-icons-purple :v-adjust 0)))
(defun hek-modeline--seg-minormodes-use-icon (icon-index text text-face)
  (concat (aref hek-modeline--seg-minormodes-icons icon-index)
          hek-modeline--thinspc
          (propertize text 'face text-face)
          hek-modeline--fatspc))

(defvar-local hek-modeline--flymake-cache nil)
(defun hek-modeline--flymake-update (&rest _)
  (let ((diag-cnt 0) ;; errors and warnings (excluding notes)
        (note-cnt 0)
        (note-lv  (warning-numeric-level :debug)))
    (cl-loop
     for state being the hash-values of flymake--state
     do (cl-loop
         for diag in (flymake--state-diags state)
         do (if (> (flymake--lookup-type-property
                    (flymake--diag-type diag)
                    'severity
                    note-lv)
                   note-lv)
              (cl-incf diag-cnt)
              (cl-incf note-cnt))))
    (setq hek-modeline--flymake-cache
          (cond
           ((> diag-cnt 0)
            (hek-modeline--seg-minormodes-use-icon
             2 (number-to-string diag-cnt) 'all-the-icons-red))
           ((> note-cnt 0)
            (hek-modeline--seg-minormodes-use-icon
             1 (number-to-string note-cnt) 'all-the-icons-maroon))
           (t
            (hek-modeline--seg-minormodes-use-icon 0 "" 'all-the-icons-green))))))
(advice-add #'flymake--handle-report :after #'hek-modeline--flymake-update)

(defvar-local hek-modeline--eglot-cache nil)
(defun hek-modeline--eglot-update ()
  (setq hek-modeline--eglot-cache
        (if (eglot-managed-p)
            (hek-modeline--seg-minormodes-use-icon
             3
             (let ((name (car (process-command (jsonrpc--process (eglot-current-server))))))
               (if-let ((end-pos (string-match "[-_/]" name)))
                   (substring name 0 end-pos)
                 name))
             'all-the-icons-purple)
          nil)))
(add-hook 'eglot-managed-mode-hook #'hek-modeline--eglot-update)

;;; === segment: major mode ===

(defvar hek-modeline--seg-majormode ;; signal
  '(:propertize mode-name face all-the-icons-lorange))

;;; === mode line format ===

(defvar hek-modeline--format-list
  `(;; *** real file associated ***
    (buffer-file-name
     ;; ** left
     (,@hek-modeline--seg-indicators
      ,hek-modeline--seg-fileinfo
      ,@hek-modeline--seg-cursorpos)
     ;; ** right
     (,@hek-modeline--seg-minormodes
      ,hek-modeline--seg-majormode)
     ;; ** init
     hek-modeline--seg-fileinfo-init
     )
    ;; *** other prog-mode or text-mode ***
    ((derived-mode-p 'prog-mode 'text-mode)
     ;; ** left
     (,@hek-modeline--seg-indicators
      ,hek-modeline--seg-bufinfo
      ,@hek-modeline--seg-cursorpos)
     ;; ** right
     (,@hek-modeline--seg-minormodes
      ,hek-modeline--seg-majormode)
     ;; ** init
     hek-modeline--seg-bufinfo-init)
    ;; *** all the others ***
    (t
     ;; ** left
     (,@hek-modeline--seg-indicators
      ,hek-modeline--seg-bufinfo)
     ;; ** right
     (,hek-modeline--seg-majormode)
     ;; ** init
     hek-modeline--seg-bufinfo-init))
  "A list of formats. Each entry is like: (COND LEFT RIGHT [INIT-FN...]) .")

(defvar-local hek-modeline--lfmt nil)
(defvar-local hek-modeline--rfmt nil)

(defun hek-modeline--init-local ()
  (catch 'found
    (dolist (entry hek-modeline--format-list)
      (when (eval (car entry))
        (setq hek-modeline--lfmt (cadr entry))
        (setq hek-modeline--rfmt (caddr entry))
        (dolist (fn (cdddr entry))
          (funcall fn))
        (throw 'found t)))
    (setq hek-modeline--lfmt nil)
    (setq hek-modeline--rfmt nil)
    nil))

(defun hek-modeline--render ()
  (let ((rstr (format-mode-line hek-modeline--rfmt)))
    (list hek-modeline--lfmt
          `(:propertize " " display (space :align-to (- right ,(string-width rstr))))
          rstr)))

(defconst hek-modeline--pub-format
  `((:eval (hek-modeline--render))))

(define-minor-mode hek-modeline-mode
  "A global minor mode for hek-modeline."
  :global t
  (if hek-modeline-mode
    (progn
      (add-hook 'after-change-major-mode-hook #'hek-modeline--init-local)
      (setq-default mode-line-format hek-modeline--pub-format))
    (progn
      (remove-hook 'after-change-major-mode-hook #'hek-modeline--init-local)
      (setq-default mode-line-format nil))))

(provide 'hek-modeline)

;;; hek-modeline.el ends here
