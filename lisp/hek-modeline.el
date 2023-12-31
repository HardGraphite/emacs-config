;;; hek-modeline.el --- A lightweight mode line. -*- lexical-binding: t -*-

;;; Commentary:

;; A lightweight Emacs mode line.

;;; Code:

(require 'nerd-icons)
(require 'cl-lib)

(defvar flymake--state)
(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function flymake--diag-type "flymake")
(declare-function flymake--handle-report "flymake")
(declare-function flymake--lookup-type-property "flymake")
(declare-function flymake--state-diags "flymake")
(declare-function jsonrpc--process "jsonrpc")
(declare-function package-installed-p "package")
(declare-function project-name "project")
(declare-function project-root "project")
(declare-function warning-numeric-level "warnings")

(defgroup hek-modeline nil
  "Hek's mode line."
  :group 'mode-line
  :prefix "hek-modeline-")

;;; === utilities ===

(defconst hek-modeline--thinspc
  (propertize " " 'display '((space :relative-width 0.5))))

(defconst hek-modeline--fatspc
  (propertize " " 'display '((space :relative-width 1.5))))

;;; === segment: indicators ===

(defvar hek-modeline--seg-indicators ;; list
  `(;; macro recording
    (defining-kbd-macro
     ,(concat hek-modeline--thinspc
              (nerd-icons-mdicon
               "nf-md-record_rec"
               :face 'nerd-icons-dred)))
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
        (propertize "%b " 'face '(nerd-icons-purple italic))))

(defvar hek-modeline--seg-fileinfo ;; single
  '(:eval (hek-modeline--seg-fileinfo)))
(defconst hek-modeline--seg-fileinfo-icons
  (vector (nerd-icons-mdicon "nf-md-lock" :face 'nerd-icons-lred)
          (nerd-icons-mdicon "nf-md-content_save" :face 'nerd-icons-green)
          (nerd-icons-mdicon "nf-md-pencil" :face 'nerd-icons-dyellow)
          (nerd-icons-mdicon "nf-md-server_network")))
(defun hek-modeline--seg-fileinfo ()
  (cons (aref hek-modeline--seg-fileinfo-icons
              (if (buffer-modified-p) 2 (if buffer-read-only 0 1)))
        hek-modeline--bufinfo-cache))
(defun hek-modeline--seg-fileinfo-init ()
  (setq hek-modeline--bufinfo-cache ;; Reuse this variable.
        (list
         hek-modeline--thinspc
         ;; file icon
         (nerd-icons-icon-for-file buffer-file-name :v-adjust 0)
         hek-modeline--thinspc
         ;; remote icon
         (when (file-remote-p buffer-file-name)
           (aref hek-modeline--seg-fileinfo-icons 3))
         ;; in-project directory
         (when-let ((proj (project-current)))
           (concat
            (propertize
             (concat (project-name proj) "/")
             'face 'nerd-icons-lcyan)
            (propertize
             (substring
              (replace-regexp-in-string
               "/\\(.\\)[^/]+"
               "/\\1"
               (concat "/" (file-name-directory (file-relative-name buffer-file-name (project-root proj)))))
              1 nil)
             'face 'nerd-icons-dcyan)))
         ;; file name
         (propertize
          (concat (file-name-nondirectory buffer-file-name) " ")
          'face 'nerd-icons-cyan
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
  (vector (nerd-icons-faicon "nf-fa-check_circle" :face 'nerd-icons-dgreen)
          (nerd-icons-faicon "nf-fa-info_circle" :face 'nerd-icons-maroon)
          (nerd-icons-faicon "nf-fa-warning" :face 'nerd-icons-red)
          (nerd-icons-faicon "nf-fa-rocket" :face 'nerd-icons-purple :v-adjust 0)))
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
             2 (number-to-string diag-cnt) 'nerd-icons-red))
           ((> note-cnt 0)
            (hek-modeline--seg-minormodes-use-icon
             1 (number-to-string note-cnt) 'nerd-icons-maroon))
           (t
            (hek-modeline--seg-minormodes-use-icon 0 "" 'nerd-icons-green))))))
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
             'nerd-icons-purple)
          nil)))
(add-hook 'eglot-managed-mode-hook #'hek-modeline--eglot-update)

;;; === segment: major mode ===

(defvar hek-modeline--seg-majormode ;; signal
  '(:propertize mode-name face nerd-icons-lorange))

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

(defvar-local hek-modeline--lfmt "%b")
(defvar-local hek-modeline--rfmt '(:eval mode-name))

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
