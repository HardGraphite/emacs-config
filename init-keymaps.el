;; --- Key bindings and modal editing -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; Customized keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-keymap +my-app-prefix-map
  "d"  #'dirvish  ;; dired
  "g"  #'magit
  "t"  #'vterm  ;; shell
  )

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

(defvar-keymap +my-diagnose-prefix-map
  "e"  (lambda (prev) (interactive "P")
         (if prev (flymake-goto-prev-error) (flymake-goto-next-error)))
  "f"  #'eglot-code-action-quickfix
  "s"  #'flyspell-goto-next-error
  "S"  #'flyspell-correct-word-before-point
  )

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
   "r"  #'ace-swap-window
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
   )

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

(defvar-keymap +my-file-prefix-map
   "f"  #'find-file
   "F"  #'find-file-other-window
   "R"  #'find-file-read-only
   "r"  #'consult-recent-file
   "s"  #'consult-find
   "h"  #'hexl-find-file
   "H"  #'hexl-mode
   "n"  #'rename-file
   "p"  #'copy-file
   "d"  #'dired
   "D"  #'dired-other-window
   )

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

(defvar-keymap +my-pair-edit-prefix-map
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


;;;;; Modify pre-defined keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((map global-map)
      (defs `((,(kbd "C-M-v") . scroll-other-window)
              (,(kbd "C-M-S-v") . scroll-other-window-down)
              ("\C-xm" . nil)
              ("\C-x4m" . nil)
              ("\C-x5m" . nil))))
  (dolist (def defs)
    (define-key map (car def) (cdr def))))


;;;;; Modal editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    `("\"" . ,+my-pair-edit-prefix-map)
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
    `("\\" . ,+my-app-prefix-map)
    `("j" . ,+my-goto-prefix-map)
    `("d" . ,+my-diagnose-prefix-map)
    `("e" . ,+my-editing-prefix-map)
    `("t" . ,+my-tabbar-prefix-map)
    `("w" . ,+my-window-prefix-map)
    `("b" . ,+my-buffer-prefix-map)
    `("f" . ,+my-file-prefix-map)
    `("r" . ,+my-register-prefix-map)
    `("k" . ,+my-kmacro-prefix-map)
    `("p" . ,project-prefix-map)
    '("TAB" . other-window)
    '("RET" . execute-extended-command)
    '("SPC" . hek-describe-buffer-file)
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
