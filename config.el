;;; -*- lexical-binding: t; no-byte-compile: t; outline-regexp: ";;;;;\\*+"; -*-

;;; Configuring statements are grouped by outline heading lines like ";;;;;* ...".
;;; You may want to enable `outline-minor-mode' to navigate between the headings.


(require 'hek-subr)


;;;;;* COMPAT

(unless (>= emacs-major-version 30) ;; before Emacs 30
  )


;;;;;* USER CONF

;;;###batch-config-begin

;;;;;** Default values

;;; Location info
(setq calendar-latitude  30
      calendar-longitude 120)

;;; Package achieves (mirrors)
;; + default
;;(defconst *my-mirror-elpa* nil)                          ; `nil' or `(gnu . nongnu)'
;;(defconst *my-mirror-melpa* "https://melpa.org/packages/")
;; + China
(defconst *my-mirror-elpa* '("https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" .
                             "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
(defconst *my-mirror-melpa* "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")

;;; Fonts
(defconst *my-code-font-family*  "JetBrains Mono")         ; Mono font for coding.
(defconst *my-code-font-height*  150)
(defconst *my-mono-font-family*  "Iosevka Fixed Slab")     ; Mono font for UI.
(defconst *my-mono-font-height*  155)
(defconst *my-term-font-family*  "Ubuntu Mono")            ; Mono font for terminal.
(defconst *my-term-font-height*  165)
(defconst *my-text-font-family*  "Roboto")                 ; Sans font for other text.
(defconst *my-text-font-height*  160)
(defconst *my-nerd-font-family*  "Symbols Nerd Font")      ; Nerd font for icons
(defconst *my-cjkx-font-family*  "Sarasa Mono Slab SC")    ; CJK chars font

;;; Directories and paths
(pcase system-type
  ('gnu/linux
   (defconst *my-emacs-conf-dir*    "~/.config/emacs/")
   (defconst *my-emacs-data-dir*    "~/.local/share/emacs/")
   (defconst *my-emacs-cache-dir*   "~/.cache/emacs/"))
  (_
   (defconst *my-emacs-conf-dir*    "~/.emacs.d/")
   (defconst *my-emacs-data-dir*    "~/.emacs.d/data/")
   (defconst *my-emacs-cache-dir*   "~/.emacs.d/cache/")))

;;; Others
(defconst *my-shell*  "/usr/bin/fish")

;;; System specific
(pcase system-type
  ('windows-nt
   (setq *my-term-font-family*  "Consolas"
         *my-shell*             "pwsh.exe"
         default-directory      "~/Desktop/"))
  ('android
   (setq default-directory      "/sdcard"))
  )


;;;;;** Load custom files

(load (concat *my-emacs-conf-dir* "userconf") t t)

;;;###batch-config-end


;;;;;* SYSTEM

;;;;;** Emacs directories and files

;;;###batch-config-begin
(setq user-emacs-directory        *my-emacs-data-dir*
      custom-theme-directory      (concat *my-emacs-conf-dir* "themes/")
      package-user-dir            (concat *my-emacs-data-dir* "packages")
      package-quickstart-file     (concat *my-emacs-data-dir* "package-quickstart.el")
      auto-save-list-file-prefix  nil ;; (concat *my-emacs-data-dir* "auto-save-list/saves-")
      custom-file                 (concat *my-emacs-conf-dir* "custom.el"))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (concat *my-emacs-data-dir* "eln-cache/")))
;;;###batch-config-end


;;;;;** Package management

;;; Step up built-in package manager.
;;;###batch-config-begin
(require 'package)
(if *my-mirror-elpa*
    (setq package-archives
          `(("gnu"    . ,(car *my-mirror-elpa*))
            ("nongnu" . ,(cdr *my-mirror-elpa*))
            ("melpa"  . ,*my-mirror-melpa*)))
  (add-to-list 'package-archives
               (cons "melpa" *my-mirror-melpa*)
               t))
(setq package-quickstart t)
(package-initialize)
;;;###batch-config-end

;;; Prepare `hek-usepkg', which helps to setup packages.
(eval-when-compile
  (require 'hek-usepkg))
(hek-usepkg-gitpkg-initialize)

;;; Install packages.
(let ((package-install-switch "--install-packages"))
  (when (member package-install-switch command-line-args)
    (setq command-line-args (delete package-install-switch command-line-args)) ;; Remove the switch.
    (setq hek-usepkg-ensure t
          hek-usepkg-debug t)
    (setq package-native-compile t)
    (setq native-comp-async-query-on-exit t
          native-comp-verbose 2)
    (when package-quickstart
      (add-hook 'kill-emacs-hook #'package-quickstart-refresh))
    (add-hook 'kill-emacs-hook #'hek-usepkg-gitpkg-quickstart-refresh)
    (package-refresh-contents)))


;;;;;** Frame and basic UI

;; Title.
(setq frame-title-format '("%b — Emacs")
      icon-title-format frame-title-format)

;; Resize method.
(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

;; Window split.
(setq split-width-threshold  (* 2 72)
      split-height-threshold (* 2 25))

;; Minibuffer tweak.
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.5)

;; Disable tooltip.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Cursor.
(setq x-stretch-cursor t)
(setq-default cursor-in-non-selected-windows nil)
(blink-cursor-mode -1)


;;;;;** System misc

;;; Confirm before quit.
(setq confirm-kill-emacs 'y-or-n-p)

;;; Use UTF-8.
(set-default-coding-systems 'utf-8)

;;; Message buffer, scratch buffer.
(setq messages-buffer-max-lines 100
      initial-scratch-message   ";; This buffer is NOT part of GNU Emacs.\n\n"
      initial-major-mode        #'fundamental-mode)


;;;;;** Hacks

;;; Increase read chunk size. For faster file read and LSP performance.
(setq read-process-output-max #x100000) ; 1 MiB

;;; Inhibit fontification when inputing, helping a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;;; Disable bidi (Bidirectional Display) and decrease long line thresholds.
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;;; GCMH :: the Garbage Collector Magic Hack
;;; https://github.com/emacsmirror/gcmh
(hek-usepkg gcmh
  :from package
  :init
  (defun +gcmh-setup ()
    (setq gcmh-idle-delay 10
          gcmh-high-cons-threshold #x1000000) ;; 16 MiB
    (gcmh-mode 1))
  :hook
  (emacs-startup-hook . +gcmh-setup))


;;;;;** Specific system

(pcase system-type
  ('android
   ;; Work with Termux.
   (let ((termux-bin-dir "/data/data/com.termux/files/usr/bin"))
     (setenv "PATH" (concat termux-bin-dir ":" (getenv "PATH")))
     (push termux-bin-dir exec-path))
   ;; Convenient bars and key bindings.
   (require 'hek-touchscreen)
   (add-to-list 'default-frame-alist '(tool-bar-position . bottom))
   (menu-bar-mode)
   (tool-bar-mode)
   (let ((img-dir (concat *my-emacs-conf-dir* "images/")))
     (hek-touchscreen-bar-add 'esc "Esc key" #'hek-touchscreen-esc (concat img-dir "esc.xpm"))
     (hek-touchscreen-bar-add 'tab "Tab key" #'hek-touchscreen-tab (concat img-dir "tab.xpm"))
     (hek-touchscreen-bar-mode))
   (setq touch-screen-display-keyboard t)
   (global-set-key (kbd "<volume-up>") #'hek-touchscreen-toggle-disp-kbd)
   (global-set-key (kbd "<volume-down>") #'hek-touchscreen-C-g)
   ;; Disable text conversion by default. Will be handled later by pakcage `hek-exim'.
   (setq overriding-text-conversion-style nil)
   ))


;;;;;* THEME

;;;;;** Fonts

;;; Global default font.
(custom-set-faces
 `(default ((t :family ,*my-code-font-family* :height ,*my-code-font-height*))))
(let ((spec (font-spec :family *my-cjkx-font-family*)))
  (set-fontset-font t 'han spec)
  (set-fontset-font t 'cjk-misc spec))

;;; Minibuffer default font.
(defconst +my-minibuffer-font-remapping-alist
  `((default :family ,*my-mono-font-family* :height ,*my-mono-font-height*)))
(defun +my-minibuffer-font-setup ()
  (set (make-local-variable 'face-remapping-alist)
       +my-minibuffer-font-remapping-alist))
(add-hook 'minibuffer-setup-hook #'+my-minibuffer-font-setup)

;;; Mode line defualt font.
(custom-set-faces
  `(mode-line-active ((t :family ,*my-mono-font-family* :height ,(- *my-mono-font-height* 10))))
  `(mode-line-inactive ((t :family ,*my-mono-font-family* :height ,(- *my-mono-font-height* 10)))))

;;; Ligatures.
(hek-usepkg hek-ligature
  :from local
  :init
  (setq hek-ligature-table
        '((verilog-mode
           (?= . ".=")
           (?! . ".=")
           (?+ . ".[+=]")
           (?- . ".[-=]")
           (?* . ".[*=/]")
           (?/ . ".[/=*]")
           (?@ . ".("))
          (prog-mode
           (?= . ".\\(?:=+\\|>\\)")
           (?< . ".\\(?:=>\\|[<=-]\\)")
           (?> . ".[>=]")
           (?+ . ".[+=]")
           (?- . ".\\(?:[=>]\\|-+\\)")
           (?* . ".[*=/]")
           (?/ . ".\\(?:/+\\|[=*]\\)")
           (?% . ".[%=]")
           (?& . ".[&=]")
           (?| . ".[|=]")
           (?^ . ".[\\^=]")
           (?~ . ".\\(?:~+\\|=\\)")
           (?! . ".\\(?:==\\|[!=]\\)")
           (?: . ".[:=]")
           (?. . ".\\.+")
           (?# . ".\\(?:#+}\\|[([{]\\)")
           (?_ . "._+"))))
  :hook
  (prog-mode-hook . hek-ligature-mode))

;;; nerd-icons :: use Nerd Font icons, an alternative to `all-the-icons'
;;; https://github.com/rainstormstudio/nerd-icons.el
(hek-usepkg nerd-icons
  :from package
  :init
  (setq nerd-icons-font-family *my-nerd-font-family*))


;;;;;** Color theme

;;; Dark/light themes.
(hek-usepkg hek-yinyang
  :from local
  :init
  (setq hek-yinyang-dark-theme  'hek-one-dark
        hek-yinyang-light-theme 'hek-one-light
        hek-yinyang-auto-sunrise-sunset t)
  :config
  (hek-yinyang-mode t))

;;; Solaire mode :: different background darkness between special / file buffers
;;; https://github.com/hlissner/emacs-solaire-mode
(hek-usepkg solaire-mode
  :from package
  :init
  (defvar +solaire-mode-exclude-mode-list ()
    "List of extra modes to enable solaire-mode.")
  :config
  (setq solaire-mode-real-buffer-fn
        (lambda ()
          (or (buffer-file-name (buffer-base-buffer))
              (derived-mode-p 'prog-mode 'text-mode)
              (memq major-mode +solaire-mode-exclude-mode-list))))
  ;; Use a different font.
  (custom-set-faces
   `(solaire-default-face
     ((t :family ,*my-mono-font-family* :height ,*my-mono-font-height*))))
  (solaire-global-mode 1))


;;;;;** Effects

;;; Pulse effects from package `pulse.el'.
(setq pulse-delay 0.04
      pulse-iterations 12)


;;;;;* EDITOR

;;;;;** Appearance & behavior

;;;;;*** Mode line

(hek-usepkg hek-modeline
  :from local
  :config
  (hek-modeline-mode 1))

;;;;;*** Scrolling

(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 5)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command #'hek-scroll-down)
(defalias 'scroll-down-command #'hek-scroll-up)

;;;;;*** Line & column

;;; Line number, current line, unused lines.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (when (or buffer-file-name
             (derived-mode-p 'prog-mode 'text-mode))
     (display-line-numbers-mode 1)
     (hl-line-mode 1)
     (setq indicate-empty-lines t))))
;; -- line number
(setq display-line-numbers-width-start 500)
(custom-set-faces
 `(line-number
   ((t :family ,*my-mono-font-family*
       :height ,(- *my-mono-font-height* 10)
       :slant italic)))
 '(line-number-current-line
   ((t :inherit line-number
       :slant normal
       :foreground "olive drab"))))
;; -- current line
(setq hl-line-sticky-flag nil) ;; Don't show highlights in all windows.

;;; Column indicator.
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

;;; Indentation indicator.
(add-hook
 'prog-mode-hook
 (lambda ()
   (unless indent-tabs-mode
     (setq hek-hl-indent-width (hek-indent-width))
     (hek-hl-indent-mode))))

;;;;;*** Brackets and pairs

;;; Auto insert pairs.
(electric-pair-mode 1)
(defun +electric-pair-inhibit (char)
  (or (eq char (char-after))
      (eq (char-syntax (following-char)) ?w)))
(setq electric-pair-inhibit-predicate '+electric-pair-inhibit)

;;; Show parenthesis.
(show-paren-mode 1)
(setq show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(custom-set-faces
 '(show-paren-match
   ((t :foreground unspecified :background unspecified
       :weight ultra-bold :underline t))))

;;; rainbow-delimiters :: highlights delimiters such as parentheses according to the depth
;;; https://github.com/Fanael/rainbow-delimiters
(hek-usepkg rainbow-delimiters
  :from package
  :hook
  (emacs-lisp-mode-hook . rainbow-delimiters-mode))

;;;;;*** Whitespace and Coding style

;;; Whitespace visualization.
(setq whitespace-line-column nil
      whitespace-style '(face tabs tab-mark trailing missing-newline-at-eof)
      whitespace-display-mappings '((tab-mark ?\t [?» ?\t])
                                    (newline-mark ?\n [?↵ ?\n])
                                    (space-mark ?\  [?·] [?.])))
(custom-set-faces
 '(trailing-whitespace ((t (:strike-through "#ff6c6b" :background unspecified)))))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)

;;; Delete trailing whitespace on save.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode 'text-mode)
              (delete-trailing-whitespace))))

;;; hungry-delete, https://github.com/nflath/hungry-delete
(hek-usepkg hungry-delete
  :from package
  :init
  (global-hungry-delete-mode)
  :config
  (setq hungry-delete-join-reluctantly nil
        hungry-delete-chars-to-skip " \t"))

;;; Tab.
(setq-default
  indent-tabs-mode  nil
  tab-width         8)

;;; editorconfig, https://github.com/editorconfig/editorconfig-emacs.
(hek-usepkg editorconfig
  :from package
  :init (editorconfig-mode 1))

;;;;;*** Special highlights

;;; hl-todo :: Highlight TODO keywords
;;; https://github.com/tarsius/hl-todo
(hek-usepkg hl-todo
  :from package
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"    warning bold italic underline)
          ("XXX"     warning bold italic underline)
          ("HACK"    font-lock-constant-face bold italic underline)
          ("FIXME"   error   bold italic underline)
          ("NOTE"    success bold italic underline)))
  :hook
  (prog-mode-hook . hl-todo-mode))


;;;;;** Tabs, windows, and buffers

;;; ace-window :: select window by number
;;; https://github.com/abo-abo/ace-window
(hek-usepkg ace-window
  :from package
  :config
  (setq aw-minibuffer-flag t
        aw-ignore-current  t)
  :bind
  (("C-x o" . ace-window)))

;;; winner (builtin) :: records of the changes in window layout configuration
(hek-usepkg winner
  :from builtin
  :init
  (setq winner-ring-size 16
        winner-dont-bind-my-keys t)
  :hook
  (emacs-startup-hook . winner-mode))

;;; Tab bar (builtin)
(hek-usepkg tab-bar
  :from builtin
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show t
        tab-bar-auto-width nil
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
        tab-bar-separator "  "
        tab-bar-new-tab-choice "*scratch*")
  (custom-set-faces
   `(tab-bar-tab ((t :family ,*my-mono-font-family* :height ,(- *my-mono-font-height* 5) :overline t)))
   '(tab-bar-tab-inactive ((t :slant italic :overline nil)))))


;;;;;** File

;;; File auto save, backup and lock.
(setq load-prefer-newer  t
      make-backup-files  nil
      auto-save-default  nil
      create-lockfiles   nil)

;;; File auto revert.
(setq auto-revert-verbose t
      auto-revert-interval 30)
(global-auto-revert-mode 1)

;;; Recent files.
(setq recentf-auto-cleanup 'never)
(add-hook 'emacs-startup-hook #'recentf-mode)


;;;;;** Project

(hek-usepkg project
  :from builtin
  :config
  ;; --- HACK: add a project find function
  (defvar +project-root-marker-file-list
    '(".git" "Makefile" "CMakeLists.txt")
    "Files that may exist in project root.")
  (defun +project-root-marker-file-exists-p (dir)
    (catch 'found
      (dolist (file +project-root-marker-file-list)
        (when (file-exists-p (concat dir file))
          (throw 'found t)))))
  (defun +project-root-marker-file-find-root (path)
    (when-let ((root-path (locate-dominating-file path #'+project-root-marker-file-exists-p)))
      (cons 'transient root-path)))
  (add-to-list 'project-find-functions #'+project-root-marker-file-find-root)
  ;; ---
  )


;;;;;** Compilation

(defun +compile-auto-hide (buf msg)
  (unless (cl-search "abnormally" msg)
    (quit-window nil (get-buffer-window buf))))

(add-hook 'compilation-finish-functions #'+compile-auto-hide)


;;;;;** Search and navigation

;;; consult :: Consulting completing-read
;;; https://github.com/minad/consult
(hek-usepkg consult
  :from package
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-preview-key '(:debounce 0.25 any)
        consult-fontify-max-size 65536
        consult-preview-raw-size 65536)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")
  :bind
  (;; Replace default commands.
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ))

;;; rg.el :: search tool based on ripgrep
;;; https://github.com/dajva/rg.el
(hek-usepkg rg
  :from package
  :config
  ;; HACK: use package.el
  (defun rg-project-root (file)
    (project-root (project-current)))
  :bind
  (("C-x p g" . rg-project))
  :bind~
  (rg-mode-map
   ("?" . rg-menu)))

;;; avy :: Jump to things in Emacs tree-style
;;; https://github.com/abo-abo/avy
(hek-usepkg avy
  :from package
  :config
  (setq avy-style 'at-full
        avy-timeout-seconds 0.4
        avy-highlight-first t)
  ;; :bind
  ;; key bindings are defined in `init-keymaps.el'.
  )

;;; Xref search
(setq xref-search-program 'ripgrep)


;;;;;** Completion

;; TODO: embark, embark-consult, wgrep

;;;;;*** Completion algorithm and category

;;; orderless, https://github.com/oantolin/orderless
(hek-usepkg orderless
  :from package
  :init
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;      orderless-component-separator #'orderless-escapable-split-on-space)
  )

(dolist (name '(emacs21 emacs22 substring initials))
  (setq completion-styles-alist (assq-delete-all name completion-styles-alist)))
(setq completion-styles '(flex basic) ;; normal buffer
      completion-category-defaults nil
      completion-category-overrides
      '((consult-location (styles orderless basic))
        (file (styles partial-completion))))
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local completion-styles '(orderless basic)))) ;; minibuffer

;;;;;*** Minibuffer completion

;;; vertico :: VERTical Interactive COmpletion
;;; https://github.com/minad/vertico
(hek-usepkg vertico
  :from package
  :defer 0 ;; load after init
  :config
  (setq vertico-scroll-margin 2
        vertico-count 10
        vertico-cycle t)
  (vertico-mode 1)
  :bind~
  (vertico-map
   ("M-j" . vertico-next)
   ("M-k" . vertico-previous)))
(hek-usepkg vertico-directory
  :after vertico
  :bind~
  (vertico-map
   ("RET" . vertico-directory-enter)))

;;; vertico-posframe :: an extension for `vertico' to show contents in a child frame
;;; https://github.com/tumashu/vertico-posframe
(hek-usepkg vertico-posframe
  :when (display-graphic-p)
  :from package
  :after vertico
  :config
  (defun +vertico-posframe-size (buffer)
    (let ((h (1+ vertico-count))
          (w (min 120 (- (frame-width) 2))))
      (list :height h :width w :min-height h :min-width w)))
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center
        vertico-posframe-size-function #'+vertico-posframe-size
        vertico-posframe-border-width 3
        vertico-posframe-parameters
        '(;; (alpha-background . 72) ;; XXX: `alpha-background' may not work on some window systems
          (left-fringe . 8)
          (right-fringe . 8)))
  (vertico-posframe-mode 1))

;;; marginalia :: Marginalia in the minibuffer
;;; https://github.com/minad/marginalia
(hek-usepkg marginalia
  :from package
  :init
  (marginalia-mode 1))

;;;;;*** Code (in-buffer) completion

;;; corfu, https://github.com/minad/corfu
(hek-usepkg corfu
  :from package
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-preselect 'valid
        corfu-echo-documentation t)
  (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  (require 'hek-corfu-nerd-icons)
  (add-to-list 'corfu-margin-formatters
               #'hek-corfu-nerd-icons-margin-formatter)
  :bind
  (corfu-map
   ("M-<escape>" . corfu-quit)
   ("<escape>" . (lambda () (interactive) (meow-insert-exit) (corfu-quit))) ;; for meow-mode
   ("M-j" . corfu-next)
   ("M-k" . corfu-previous)
   ("M-v" . corfu-scroll-up)
   ("M-V" . corfu-scroll-down)))

;;; cape, https://github.com/minad/cape
(hek-usepkg cape
  :from package
  :init
  (dolist (capf
           '(cape-dabbrev
             ;; cape-file
             ;; cape-elisp-block
             ;; cape-history
             ;; cape-keyword
             ;; cape-tex
             ;; cape-sgml
             ;; cape-rfc1345
             ;; cape-abbrev
             ;; cape-dict
             ;; cape-symbol
             ;; cape-line
             ))
  (add-to-list 'completion-at-point-functions capf)))

;; Hacks for a better Eglot experience.
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;; (defun +eglot-setup-capfs ()
;;   (setq-local
;;    completion-at-point-functions
;;    (list (cape-super-capf
;;           #'eglot-completion-at-point
;;           #'cape-dabbrev))))
;; (add-hook 'eglot-managed-mode-hook #'+eglot-setup-capfs)

;;;;;*** Code templates

;;; TempEl :: code templates (snippets)
;;; https://github.com/minad/tempel
(hek-usepkg tempel
  :from package
  :config
  (setq hek-tempel-snippets-dir (concat *my-emacs-conf-dir* "snippets/"))
  (require 'hek-tempel-snippets)
  (push #'hek-tempel-snippets tempel-template-sources)
  :bind
  (("M-=" . tempel-insert))
  :bind~
  (tempel-map
   ("S-TAB" . tempel-previous)
   ("TAB" . tempel-next)
   ("M-<escape>" . tempel-done)))

;;; lsp-snippet :: bridge between TempEl and Eglot
;;; https://github.com/svaante/lsp-snippet
;; (hek-usepkg lsp-snippet-tempel
;;   ;; :from package-vc "https://github.com/svaante/lsp-snippet"
;;   :from local
;;   :after eglot
;;   :config
;;   (lsp-snippet-tempel-eglot-init))

;; FIXME: Since `lsp-snippet' is not so stable until now, while `eglot' itself
;; only supports `yasnippet' as the template engine, `yasnippet' is used for
;; `eglot' completion only at present, until there is a better solution.

;;; YASnippet :: A template system
;;; https://github.com/joaotavora/yasnippet
(hek-usepkg yasnippet
  :from package
  :defer t
  :init
  (setq yas-snippet-dirs nil
        yas-verbosity 0)
  (fset 'yas--message #'ignore)
  )


;;;;;** Spell / grammar check

;;; Spell check
(defun +flyspell-setup ()
  (require 'ispell)
  (if (not (executable-find ispell-program-name))
      (unload-feature 'ispell)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode) ;; TODO: Check spells in identifiers.
    (add-hook 'text-mode-hook #'flyspell-mode)
    (require 'flyspell)
    (require 'hek-spell)
    (hek-spell-mode 1))
  (fmakunbound '+flyspell-setup))
(add-hook 'emacs-startup-hook #'+flyspell-setup)

;; TODO: better spell checks.


;;;;;* LANGUAGES

;;;;;** Common programming support

;;; Eglot :: LSP (language server protocol) support
(hek-usepkg eglot
  :from builtin
  :init
  ;; eglot options
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  ;; LSP servers
  (setq eglot-server-programs
        '(((c-mode c++-mode c-ts-mode c++-ts-mode)
           "clangd" "--function-arg-placeholders=0" "--header-insertion=never")
          ((cmake-mode cmake-ts-mode) "cmake-language-server")
          (python-base-mode "pyright-langserver" "--stdio")
          (bash-ts-mode "bash-language-server" "start")
          (lua-mode "lua-language-server")
          ((tex-mode bibtex-mode) "texlab")))
  ;; mode hooks
  (dolist (config eglot-server-programs)
    (let ((mode-list (car config)))
      (unless (listp mode-list)
        (setq mode-list (list mode-list)))
      (dolist (mode mode-list)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  #'eglot-ensure))))
  :hook ;; DO NOT delete this line.
  )

;;; ElDoc box :: displays ElDoc documentations in a childframe
;;; https://github.com/casouri/eldoc-box/tree/master
(hek-usepkg eldoc-box
  :from package
  :config
  (custom-set-faces
   `(eldoc-box-body ((t :family ,*my-mono-font-family* :height ,*my-mono-font-height*))))
  :hook
  (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
  )

;;; treesit :: Tree-sitter support, providing syntax highlights and querying.
(hek-usepkg treesit
  :from builtin
  :when-ensure
  (setq treesit-language-source-alist
        '(;; (bash "https://github.com/tree-sitter/tree-sitter-bash")
          ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (dolist (x treesit-language-source-alist)
    (let ((lang (car x)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang))))
  :init
  (setq major-mode-remap-alist
        '((js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode . bash-ts-mode)))
  )


;;;;;** C-like languages

(with-eval-after-load "cc-mode"

  (defconst +my-c-style
    '((fill-column . 80)
      (indent-tabs-mode . nil)
      (c-basic-offset . 4)
      (c++-indent-level . 4)
      (c-comment-only-line-offset . (0 . 0))
      (c-block-comment-prefix . "")
      (c-hanging-braces-alist
       . ((defun-open after)
          (substatement-open after)
          (brace-list-open after)))
      (c-offsets-alist
       . ((arglist-intro . +) ;; c-lineup-arglist-intro-after-paren
          (arglist-cont-nonempty . 0)
          (arglist-cont . 0)
          (arglist-close . 0) ;; c-lineup-arglist
          (statement-block-intro . +)
          (statement-case-open . +)
          (statement-cont . +)
          (substatement-open . 0)
          (substatement-label . 0)
          (label . 0)
          (brace-list-open . 0)
          (brace-list-intro . +)
          (topmost-intro . 0)
          (topmost-intro-cont . 0)
          (inline-open . 0)
          (member-init-intro . +)
          (innamespace . 0)
          (template-args-cont . +)
          ))
      (c-special-indent-hook
       . ())
      ))

  (c-add-style "my" +my-c-style)
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode  . "awk")
          (other     . "my")))
  )

;;;;;** CMake

;;; CMake mode
;;; https://gitlab.kitware.com/cmake/cmake/-/blob/master/Auxiliary/cmake-mode.el
(hek-usepkg cmake-mode
  :from package
  :defer t
  :config
  (setq cmake-tab-width 4)
  :bind~
  (cmake-mode-map
   ("C-c C-h" . cmake-help)))

;;;;;** Lua

;;; lua mode
;;; https://github.com/immerrr/lua-mode

(hek-usepkg lua-mode
  :from package
  :defer t
  )

;;;;;** Markdown

;;; Markdown mode
;;; https://github.com/jrblevin/markdown-mode
(hek-usepkg markdown-mode
  :from package
  :defer t
  :config
  (setq markdown-split-window-direction 'right
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-mouse-follow-link nil
        markdown-command
        '("pandoc" "--from=markdown" "--to=html5" "--no-highlight")
        markdown-fontify-code-blocks-natively t
        markdown-code-lang-modes
        '(("c" . c-mode)
          ("c++" . c++-mode)
          ("cpp" . c++-mode)
          ("python" . python-mode)
          ("sh" . sh-mode)
          ("shell" . sh-mode))))

;;;;;** Verilog HDL / SystemVerilog

(hek-usepkg verilog-mode
  :from builtin
  :defer t
  :config
  (setq verilog-align-ifelse t
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-auto-inst-vector nil
        verilog-auto-lineup '(all)
        verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-highlight-grouping-keywords t
        verilog-indent-level 4
        verilog-indent-level-behavioral 4
        verilog-indent-level-declaration 4
        verilog-indent-level-directive 4
        verilog-indent-level-module 4
        verilog-indent-lists nil
        verilog-case-indent 4
        verilog-cexp-indent 4
        verilog-indent-begin-after-if nil
        )
  ;; (setq verilog-linter "verilator -–lint-only -Wall")
  (require 'hek-flymake-verilator)
  (when (ignore-errors (hek-flymake-verilator-setup) t)
    (flymake-mode 1)))


;;;;;* APPLICATIONS

;;;;;** Dashboard

;; Do not use dashboard now. Instead, put contents in scratch buffer at startup,
;; and erase buffer after first key stroke. Make sure `inhibit-startup-screen'
;; is non-nil so that the contents in scratch buffer is visible at startup.

(defconst +my-scratch-hello-message initial-scratch-message)
(setq initial-scratch-message nil)

(defun +my-scratch-hello-clear ()
  (remove-hook 'pre-command-hook #'+my-scratch-hello-clear)
  (with-current-buffer "*scratch*"
    (let ((inhibit-redisplay t))
      (erase-buffer)
      (lisp-interaction-mode)
      (insert +my-scratch-hello-message ?\n)
      (backward-char))))

(defun +my-scratch-hello-setup ()
  (remove-hook 'emacs-startup-hook #'+my-scratch-hello-setup)
  (with-current-buffer "*scratch*"
    (require 'hek-subr)
    (add-hook 'pre-command-hook #'+my-scratch-hello-clear)
    (insert ?\n)

    ;; Emacs logo.
    (insert-image (create-image (concat *my-emacs-conf-dir* "misc/gnu_emacs.png")))
    (hek-center-line)
    (insert ?\n)

    ;; Emacs version.
    (insert (propertize
             (concat "GNU Emacs "
                     emacs-version
                     (when (daemonp) " (client)"))
             'face '(variable-pitch
                     (:slant italic :height 120))))
    (hek-center-line)
    (insert ?\n ?\n)

    (goto-char (point-min))))

(if (and command-line-args (cdr command-line-args)) ;; has extra arguments
    (+my-scratch-hello-clear)
  (setq initial-major-mode #'fundamental-mode)
  (add-hook 'emacs-startup-hook #'+my-scratch-hello-setup))


;;;;;** Input methods

(hek-usepkg hek-exim
  :from local
  :config
  (pcase system-type
    ('gnu/linux
     (hek-exim-fcitx5-dbus-init-state)
     (setq hek-exim-get-source-function #'hek-exim-fcitx5-dbus-get-state
           hek-exim-set-source-function #'hek-exim-fcitx5-dbus-set-state))
    ('windows-nt
     (setq hek-exim-get-source-function #'w32-get-ime-open-status
           hek-exim-set-source-function #'w32-set-ime-open-status))
    ('android
     (hek-exim-textconv-init-state)
     (setq hek-exim-get-source-function #'hek-exim-textconv-get-state
           hek-exim-set-source-function #'hek-exim-textconv-set-state))
    (_
     (user-error "input method is not available"))
    )
  (when (featurep 'meow)
    (setq hek-exim-automodal-hooks
          '(meow-insert-enter-hook . meow-insert-exit-hook))
    (hek-exim-automodal-mode))
  (hek-exim-inlinetext-mode)
  (setq hek-exim-verbose t) ;; For debug.
  :bind
  (("M-SPC" . hek-exim-toggle)
   ("M-S-SPC" . hek-exim-inlinetext-create)))


;;;;;** Directory view

;;; dirvish :: an improved version of `dired'
;;; https://github.com/alexluigit/dirvish
(hek-usepkg dirvish
  :from package
  :config
  (setq dirvish-use-header-line 'global
        dirvish-attributes '(vc-state subtree-state nerd-icons file-time file-size)
        dirvish-subtree-state-style 'nerd
        dirvish-subtree-always-show-state t)
  :bind~
  (dirvish-mode-map
   ("TAB" . dirvish-subtree-toggle)
   ("`" . dirvish-layout-toggle)
   ("/" . dired-isearch-filenames))
  :hook
  (after-init-hook . dirvish-override-dired-mode))


;;;;;** Terminal

;; Emacs-libvterm, https://github.com/akermu/emacs-libvterm
(hek-usepkg vterm
  :from package
  :init
  (defun +shell-vterm (&optional buffer-name)
    (if (stringp buffer-name)
      (if-let (buffer (get-buffer buffer-name))
        (switch-to-buffer buffer)
        (vterm buffer-name))
      (vterm)))
  (advice-add 'shell :override #'+shell-vterm)
  :config
  (setq vterm-shell *my-shell*
        vterm-kill-buffer-on-exit t
        vterm-keymap-exceptions ())
  ;; Disable solaire-mode.
  (when (boundp '+solaire-mode-exclude-mode-list) ;; See init-theme.el
    (add-to-list '+solaire-mode-exclude-mode-list 'vterm-mode))
  ;; Key map for modal editing normal state.
  (defvar +vterm-modal-normal-state-map (make-sparse-keymap))
  (dolist (x '((yank . vterm-yank)
               (undo . vterm-undo)
               (xterm-paste . vterm-xterm-paste)
               (yank-pop . vterm-yank-pop)
               (mouse-yank-primary . vterm-yank-primary)
               (self-insert-command . vterm--self-insert)
               (beginning-of-defun . vterm-previous-prompt)
               (end-of-defun . vterm-next-prompt)))
    (define-key +vterm-modal-normal-state-map
                (vector 'remap (car x)) (cdr x)))
  ;; Buffer local configuration.
  (defun +vterm-local-init ()
    ;; Use a different font.
    (buffer-face-set
     (list :family *my-term-font-family*
           :height *my-term-font-height*))
    ;; Meow integration.
    (add-hook 'meow-insert-enter-hook
              (lambda ()
                (use-local-map vterm-mode-map)
                (vterm-reset-cursor-point))
              nil t)
    (add-hook 'meow-insert-exit-hook
              (lambda ()
                (use-local-map +vterm-modal-normal-state-map))
              nil t)
    )
  ;; HACK: keep cursor-type unchanged.
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
  (advice-add
   'vterm--redraw :around
   (lambda (fn &rest args)
     (let ((cursor-type cursor-type))
       (apply fn args))))
  :bind
  (("<f12>" . vterm-other-window))
  :bind~
  (vterm-mode-map
   ("C-<escape>" . vterm-send-next-key)
   ("C-x" . vterm--self-insert)
   ("C-c" . vterm--self-insert))
  :hook
  (vterm-mode-hook . +vterm-local-init))


;;;;;** Version control systems

;; Magit, https://github.com/magit/magit
(hek-usepkg magit
  :from package
  :defer t
  :config
  (setq magit-save-repository-buffers nil))

;; diff-hl, https://github.com/dgutov/diff-hl
(hek-usepkg diff-hl
  :from package
  :hook
  ;; (dired-mode-hook . diff-hl-dired-mode) ;; use `dirvish-vc' instead
  ((prog-mode-hook tex-mode-hook markdown-mode) . diff-hl-mode)
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

;;;;;* KEY BINDINGS

;;;;;** Customized keymaps

(defvar-keymap +my-app-prefix-map
  "d"  #'dirvish  ;; dired
  "g"  #'magit
  "t"  #'vterm  ;; shell
  )
(fset '+my-app-prefix-map +my-app-prefix-map)

(defvar-keymap +my-goto-prefix-map
  "d"  #'xref-find-definitions
  "r"  #'xref-find-references
  "o"  #'xref-pop-marker-stack
  "O"  #'xref-pop-to-location
  "h"  #'eldoc-doc-buffer
  "D"  #'eglot-find-declaration
  "I"  #'eglot-find-implementation
  "T"  #'eglot-find-typeDefinition
  "y"  #'consult-imenu ;; imenu
  "l"  #'consult-goto-line
  "m"  #'consult-mark
  )
(fset '+my-goto-prefix-map +my-goto-prefix-map)

(defvar-keymap +my-diagnose-prefix-map
  "e"  (lambda (prev) (interactive "P")
         (if prev (flymake-goto-prev-error) (flymake-goto-next-error)))
  "f"  #'eglot-code-action-quickfix
  "s"  #'flyspell-goto-next-error
  "S"  #'flyspell-correct-word-before-point
  )
(fset '+my-diagnose-prefix-map +my-diagnose-prefix-map)

(defvar-keymap +my-rectedit-prefix-map
  "d"  #'kill-rectangle
  "y"  #'copy-rectangle-as-kill
  "D"  #'delete-rectangle
  "p"  #'yank-rectangle
  "C"  #'clear-rectangle
  "o"  #'open-rectangle
  "c"  #'string-rectangle
  "i"  #'string-insert-rectangle
  "SPC" #'rectangle-mark-mode
  )
(fset '+my-rectedit-prefix-map +my-rectedit-prefix-map)

(defvar-keymap +my-editing-prefix-map
  "l"  #'downcase-dwim
  "u"  #'upcase-dwim
  "U"  #'capitalize-dwim
  "j"  #'join-line
  "s"  #'isearch-forward
  "S"  #'isearch-forward-regexp
  "r"  #'query-replace
  "R"  #'query-replace-regexp
  "p"  #'consult-yank-from-kill-ring
  "P"  #'clipboard-yank
  "Y"  #'clipboard-kill-ring-save
  "/"  #'comment-line
  "?"  #'comment-or-uncomment-region
  "e"  +my-rectedit-prefix-map
  )
(fset '+my-editing-prefix-map +my-editing-prefix-map)

(defvar-keymap +my-search-prefix-map
  "s"  #'rg-literal
  "S"  #'rg
  )
(fset '+my-search-prefix-map +my-search-prefix-map)

(defvar-keymap +my-tabbar-prefix-map
   "T"  #'tab-bar-mode
   "t"  #'tab-switch
   "D"  #'dired-other-tab
   "F"  #'find-file-other-tab
   "b"  #'switch-to-buffer-other-tab
   "SPC" #'other-tab-prefix
   "+"  #'tab-new
   "*"  #'tab-duplicate
   "n"  #'tab-rename
   "x"  #'tab-close
   "."  #'tab-close-other
   "u"  #'tab-undo
   "m"  #'tab-move
   "["  #'tab-previous
   "]"  #'tab-next
   "l"  #'tab-list
   )
(fset '+my-tabbar-prefix-map +my-tabbar-prefix-map)

(defvar-keymap +my-window-prefix-map
   "0"  #'delete-window
   "1"  #'delete-other-windows
   "."  #'delete-other-windows
   "2"  #'split-window-below
   "-"  #'split-window-below
   "_"  (lambda () (interactive) (select-window (split-window-below)))
   "3"  #'split-window-right
   "\\" #'split-window-right
   "|"  (lambda () (interactive) (select-window (split-window-right)))
   "4"  #'ctl-x-4-map
   "5"  #'ctl-x-5-map
   "o"  #'other-window-prefix
   "w"  #'ace-select-window
   "h"  #'windmove-left
   "j"  #'windmove-down
   "k"  #'windmove-up
   "l"  #'windmove-right
   "R"  #'ace-swap-window
   "x"  #'delete-window
   "H"  #'windmove-delete-left
   "J"  #'windmove-delete-down
   "K"  #'windmove-delete-up
   "L"  #'windmove-delete-right
   "^"  #'enlarge-window
   ">"  #'enlarge-window-horizontally
   "v"  #'shrink-window
   "<"  #'shrink-window-horizontally
   "="  #'balance-windows
   "r"  #'window-configuration-to-register
   "u"  #'winner-undo
   "U"  #'winner-redo
   )
(fset '+my-window-prefix-map +my-window-prefix-map)

(defvar-keymap +my-buffer-prefix-map
   "b"  #'consult-buffer
   "B"  #'consult-buffer-other-window
   "l"  #'list-buffers
   "o"  #'mode-line-other-buffer
   "j"  #'previous-buffer
   "k"  #'next-buffer
   "x"  #'kill-buffer
   "s"  #'save-buffer
   "S"  #'save-some-buffers
   "R"  #'read-only-mode
   "n"  #'narrow-to-region
   "N"  #'widen
   )
(fset '+my-buffer-prefix-map +my-buffer-prefix-map)

(defvar-keymap +my-open-prefix-map
   "f"  #'find-file
   "F"  #'find-file-other-window
   "r"  #'consult-recent-file
   "s"  #'consult-find
   "h"  #'hexl-find-file
   "d"  #'dired
   "D"  #'dired-other-window
   )
(fset '+my-open-prefix-map +my-open-prefix-map)

(fset '+my-project-prefix-map project-prefix-map)

(defvar-keymap +my-register-prefix-map
  "r"  #'consult-register
  "l"  #'consult-register-load
  "s"  #'consult-register-store
  "."  #'point-to-register
  "w"  #'window-configuration-to-register
  "k"  #'kmacro-to-register
  "j"  #'jump-to-register
  "y"  #'copy-to-register
  "x"  #'copy-rectangle-to-register
  "n"  #'number-to-register
  "p"  #'insert-register
  "A"  #'append-to-register
  "I"  #'prepend-to-register
  "+"  #'increment-register
  )
(fset '+my-register-prefix-map +my-register-prefix-map)

(defvar-keymap +my-kmacro-prefix-map
   "p"  #'consult-kmacro
   "q"  #'kbd-macro-query
   "n"  #'kmacro-name-last-macro
   "d"  #'kmacro-delete-ring-head
   "j"  #'kmacro-cycle-ring-next
   "k"  #'kmacro-cycle-ring-previous
   "e"  #'kmacro-edit-macro
   "E"  #'kmacro-edit-lossage
   "c"  #'kmacro-insert-counter
   "C"  #'kmacro-set-counter
   "+"  #'kmacro-add-counter
   "F"  #'kmacro-set-format
   "r"  #'kmacro-to-register
   )
(fset '+my-kmacro-prefix-map +my-kmacro-prefix-map)

(defvar-keymap +my-pairedit-prefix-map
  "'"  #'hek-surround-region
  "\"" #'hek-surround-region
  "`"  #'hek-surround-region
  "("  #'hek-surround-region
  "["  #'hek-surround-region
  "{"  #'hek-surround-region
  "<"  #'hek-surround-region
  "s"  #'hek-surround-region
  "x"  #'hek-unsurround-region
  "d"  #'hek-unsurround-region
  )
(fset '+my-pairedit-prefix-map +my-pairedit-prefix-map)


;;;;;** Modify pre-defined keymap

(let ((map global-map)
      (defs `((,(kbd "C-M-v") . scroll-other-window)
              (,(kbd "C-M-S-v") . scroll-other-window-down)
              ("\C-xm" . nil)
              ("\C-x4m" . nil)
              ("\C-x5m" . nil))))
  (dolist (def defs)
    (define-key map (car def) (cdr def))))


;;;;;** Modal editing

;;; Meow, https://github.com/meow-edit/meow
(hek-usepkg meow
  :from package
  :init
  ;; --- pre-start configs ---
  (setq meow-use-cursor-position-hack t
        meow-use-enhanced-selection-effect t)
  ;; --- start meow ! ---
  (meow-global-mode 1)
  :config
  ;; --- functions ---
  (defun +meow-reverse-or-negarg ()
    (interactive)
    (call-interactively (if mark-active #'meow-reverse #'negative-argument)))
  (defun +meow-delete-region-or-char ()
    (interactive)
    (call-interactively (if mark-active 'delete-region 'delete-char)))
  ;; --- key bindings ---
  (meow-normal-define-key
    ;; -- numbers --
    '("0" . meow-expand-0)
    '("9" . meow-expand-9)
    '("8" . meow-expand-8)
    '("7" . meow-expand-7)
    '("6" . meow-expand-6)
    '("5" . meow-expand-5)
    '("4" . meow-expand-4)
    '("3" . meow-expand-3)
    '("2" . meow-expand-2)
    '("1" . meow-expand-1)
    '("-" . +meow-reverse-or-negarg)
    ;; -- basic cursor moving --
    '("h" . meow-left)
    '("H" . meow-left-expand)
    '("j" . meow-next)
    '("J" . meow-next-expand)
    '("k" . meow-prev)
    '("K" . meow-prev-expand)
    '("l" . meow-right)
    '("L" . meow-right-expand)
    ;; -- word-based selections --
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    ;; -- object-based selections --
    '("x" . meow-line)
    '("o" . meow-block)
    '("O" . meow-to-block)
    '("m" . meow-join)
    ;; -- thing-based selections --
    '("," . meow-inner-of-thing)
    '("." . meow-bounds-of-thing)
    '("[" . meow-beginning-of-thing)
    '("]" . meow-end-of-thing)
    ;; -- find-based selections --
    '("f" . meow-find)
    '("t" . meow-till)
    ;; -- selection manipulating --
    ;;'("-" . meow-reverse)
    '("g" . meow-cancel-selection)
    '("z" . meow-pop-selection)
    ;; -- kill, delete, copy & paste --
    '("d" . meow-kill)
    '("D" . +meow-delete-region-or-char)
    '("y" . meow-save)
    '("p" . meow-yank)
    '("r" . meow-replace)
    ;; -- search/jump --
    '("/" . consult-line)
    '("?" . avy-goto-line)
    '(";" . avy-goto-word-or-subword-1)
    '(":" . avy-goto-char-timer)
    '("n" . meow-search)
    '("v" . scroll-up-command)
    '("V" . scroll-down-command)
    '("M" . recenter-top-bottom)
    ;; -- undo/redo --
    '("u" . meow-undo)
    '("U" . undo-redo) ;;'("U" . meow-undo-in-selection)
    ;; -- enter insert mode --
    '("a" . meow-append)
    '("A" . meow-open-below)
    '("i" . meow-insert)
    '("I" . meow-open-above)
    '("c" . meow-change)
    ;; -- grab (beacon mode) --
    '("G" . meow-grab)
    '("Y" . meow-sync-grab)
    '("R" . meow-swap-grab)
    '("Z" . meow-pop-grab)
    ;; -- others --
    '("q" . meow-quit)
    '("Q" . kill-current-buffer)
    '("'" . repeat)
    '("\"" . +my-pairedit-prefix-map)
    '("<escape>" . ignore)
    )
  (setq meow-char-thing-table
    '((?\( . round)
      (?\) . round)
      (?\[ . square)
      (?\] . square)
      (?\{ . curly)
      (?\} . curly)
      (?'  . string)
      (?\" . string)
      (?y . symbol)
      (?w . window)
      (?b . buffer)
      (?p . paragraph)
      (?l . line)
      (?f . defun)
      (?. . sentence)))
  (meow-motion-overwrite-define-key
    ;; '("j" . meow-next)
    ;; '("k" . meow-prev)
    '("M-h" . backward-char)
    '("M-j" . next-line)
    '("M-k" . previous-line)
    '("M-l" . forward-char)
    '("<escape>" . ignore)
    )
  (meow-leader-define-key
    '("1" . meow-digit-argument)
    '("2" . meow-digit-argument)
    '("3" . meow-digit-argument)
    '("4" . meow-digit-argument)
    '("5" . meow-digit-argument)
    '("6" . meow-digit-argument)
    '("7" . meow-digit-argument)
    '("8" . meow-digit-argument)
    '("9" . meow-digit-argument)
    '("0" . meow-digit-argument)
    '("g" . ignore)
    '("\\" . +my-app-prefix-map)
    '("j" . +my-goto-prefix-map)
    '("d" . +my-diagnose-prefix-map)
    '("e" . +my-editing-prefix-map)
    '("s" . +my-search-prefix-map)
    '("t" . +my-tabbar-prefix-map)
    '("w" . +my-window-prefix-map)
    '("b" . +my-buffer-prefix-map)
    '("o" . +my-open-prefix-map)
    '("r" . +my-register-prefix-map)
    '("k" . +my-kmacro-prefix-map)
    '("p" . +my-project-prefix-map)
    '("TAB" . other-window)
    '("RET" . execute-extended-command)
    `("SPC" . ,(lambda () (interactive)
                 (pulse-momentary-highlight-one-line nil 'cursor)
                 (hek-describe-buffer-file)))
    '("`" . mode-line-other-buffer)
    '("/" . meow-keypad-describe-key)
    '("?" . meow-cheatsheet)
    )
  (meow-define-keys 'insert
    '("M-h" . backward-char)
    '("M-j" . next-line)
    '("M-k" . previous-line)
    '("M-l" . forward-char)
    '("M-b" . forward-word)
    '("M-e" . backword-word)
    )
  (setf (alist-get 'meow-kill meow-selection-command-fallback) #'delete-char)
  ;; --- behaviors ---
  (setq meow-keypad-message nil
        meow-display-thing-help t ;; See the hack on `meow-thing-prompt' below.
        meow-keypad-describe-delay 1
        meow-keypad-self-insert-undefined nil
        meow-keypad-start-keys '((?c . ?c) (?x . ?x))
        meow-keypad-meta-prefix ?M
        meow-keypad-ctrl-meta-prefix ?m
        meow-select-on-change nil)
  (setq meow-mode-state-list
        '((vterm-mode . insert)))
  (setq meow-grab-fill-commands
        '(query-replace query-replace-regexp))
  ;; HACK: delay before xxx-of-thing help.
  (defun meow-thing-prompt (prompt-text)
    (if meow-display-thing-help
        (or (read-char prompt-text nil meow-keypad-describe-delay)
            (read-char (concat (meow--render-char-thing-table) "\n" prompt-text)))
      (read-char prompt-text)))
  ;; --- visual elements ---
  (setq meow-replace-state-name-list
    '((normal . "N")
      (motion . "M")
      (keypad . "K")
      (insert . "I")
      (beacon . "B")))
  (setq meow-cursor-type-insert '(bar . 3)
        meow-cursor-type-region-cursor '(bar . 1)
        meow-cursor-type-motion 'hollow)
  ;;(meow-setup-indicator) ;; Rendered by doom-modeline.
  ;;(meow-setup-line-number)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  )
