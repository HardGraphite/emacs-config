;; --- Key bindings and modal editing -*- lexical-binding: t -*-

;;;;; Customized keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +my-define-keys (map key-def-pairs)
  (dolist (pair key-def-pairs)
    (define-key map
      (let ((key (car pair)))
        (if (stringp key) (kbd key) key))
      (cdr pair)))
  map)

(defvar +my-goto-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("d" . xref-find-definitions)
     ("r" . xref-find-references)
     ("o" . xref-pop-marker-stack)
     ("O" . xref-pop-to-location)
     ("E" . flymake-goto-next-error)
     ("e" . flymake-goto-next-error)
     ("H" . eldoc-doc-buffer)
     ("D" . eglot-find-declaration)
     ("i" . eglot-find-implementation)
     ("T" . eglot-find-typeDefinition)
     ("y" . consult-imenu) ;; imenu
     ("l" . consult-goto-line)
     ("m" . consult-mark))))

(defvar +my-rectangle-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("s" . kill-rectangle)
     ("y" . copy-rectangle-as-kill)
     ("d" . delete-rectangle)
     ("p" . yank-rectangle)
     ("c" . clear-rectangle)
     ("o" . open-rectangle)
     ("t" . string-rectangle)
     ("i" . string-insert-rectangle)
     ("SPC" . rectangle-mark-mode))))

(defvar +my-text-manip-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   `(("l" . downcase-dwim)
     ("u" . upcase-dwim)
     ("r" . query-replace)
     ("R" . query-replace-regexp)
     ("p" . consult-yank-from-kill-ring)
     ("P" . consult-yank-pop)
     ("t" . ,+my-rectangle-prefix-map))))

(defvar +my-window-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("0" . delete-window)
     ("1" . delete-other-windows)
     ("." . delete-other-windows)
     ("2" . split-window-below)
     ("-" . split-window-below)
     ("3" . split-window-right)
     ("\\" . split-window-right)
     ("4" . ctl-x-4-map)
     ("5" . ctl-x-5-map)
     ("o" . other-window)
     ("w" . ace-select-window)
     ("h" . windmove-left)
     ("j" . windmove-down)
     ("k" . windmove-up)
     ("l" . windmove-right)
     ("r" . ace-swap-window)
     ("x" . delete-window)
     ("H" . windmove-delete-left)
     ("J" . windmove-delete-down)
     ("K" . windmove-delete-up)
     ("L" . windmove-delete-right)
     ("^" . enlarge-window)
     (">" . enlarge-window-horizontally)
     ("v" . shrink-window)
     ("<" . shrink-window-horizontally)
     ("=" . balance-windows))))

(defvar +my-buffer-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("b" . consult-buffer)
     ("B" . list-buffers)
     ("o" . mode-line-other-buffer)
     ("j" . previous-buffer)
     ("k" . next-buffer)
     ("x" . kill-buffer)
     ("s" . save-buffer)
     ("n" . narrow-to-region)
     ("N" . widen))))

(defvar +my-file-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("f" . find-file)
     ("r" . consult-recent-file)
     ("s" . consult-find))))

(defvar +my-register-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("r" . consult-register)
     ("l" . consult-register-load)
     ("s" . consult-register-store)
     ("." . point-to-register)
     ("w" . window-configuration-to-register)
     ("k" . kmacro-to-register)
     ("j" . jump-to-register)
     ("y" . copy-to-register)
     ("x" . copy-rectangle-to-register)
     ("n" . number-to-register)
     ("p" . insert-register)
     ("A" . append-to-register)
     ("I" . prepend-to-register)
     ("+" . increment-register))))

(defvar +my-kmacro-prefix-map
  (+my-define-keys
   (make-sparse-keymap)
   '(("p" . consult-kmacro)
     ("q" . kbd-macro-query)
     ("d" . kmacro-delete-ring-head)
     ("j" . kmacro-cycle-ring-next)
     ("k" . kmacro-cycle-ring-previous)
     ("e" . kmacro-edit-macro)
     ("E" . kmacro-edit-lossage)
     ("c" . kmacro-insert-counter)
     ("C" . kmacro-set-counter)
     ("+" . kmacro-add-counter)
     ("F" . kmacro-set-format)
     ("r" . kmacro-to-register))))

(fmakunbound #'+my-define-keys)

;;;;; Modal editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; evil, https://github.com/emacs-evil/evil
;; (use-package evil
;;   :preface
;;   (setq evil-undo-system 'undo-redo)
;;   :init
;;   (evil-mode 1))

;;; Meow, https://github.com/meow-edit/meow
(use-package meow
  :init
  ;; --- pre-start configs ---
  (setq meow-use-cursor-position-hack t
        meow-use-enhanced-selection-effect t)
  ;; --- start meow ! ---
  (meow-global-mode 1)
  :config
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
    '("-" . negative-argument)
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
    '(";" . meow-reverse)
    '("g" . meow-cancel-selection)
    '("z" . meow-pop-selection)
    ;; -- kill, delete, copy & paste --
    '("d" . (lambda () (interactive)
              (call-interactively (if mark-active 'delete-region 'delete-char))))
    '("y" . meow-save)
    '("s" . meow-kill)
    '("p" . meow-yank)
    '("r" . meow-replace)
    ;; -- search/jump --
    '("/" . consult-line) ;; '("/" . meow-visit)
    '("?" . avy-goto-char-2)
    '("n" . meow-search)
    '("X" . meow-goto-line)
    '("v" . scroll-up-command)
    '("V" . scroll-down-command)
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
    ;; -- others --
    '("Q" . meow-quit)
    '("'" . repeat)
    '("<escape>" . ignore)
    )
  (setq meow-char-thing-table
    '((?\( . round)
      (?\) . round)
      (?\[ . square)
      (?\] . square)
      (?\{ . curly)
      (?\} . curly)
      (?' . string)
      (?y . symbol)
      (?w . window)
      (?b . buffer)
      (?p . paragraph)
      (?l . line)
      (?d . defun)
      (?. . sentence)))
  (meow-motion-overwrite-define-key
    '("j" . meow-next)
    '("k" . meow-prev)
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
    `("g" . ,+my-goto-prefix-map)
    `("t" . ,+my-text-manip-prefix-map)
    `("w" . ,+my-window-prefix-map)
    `("b" . ,+my-buffer-prefix-map)
    `("f" . ,+my-file-prefix-map)
    `("r" . ,+my-register-prefix-map)
    `("k" . ,+my-kmacro-prefix-map)
    `("p" . ,project-prefix-map)
    '("TAB" . other-window)
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
  ;; --- behaviors ---
  (setq meow-keypad-message nil
        meow-keypad-self-insert-undefined nil
        meow-keypad-start-keys '((?c . ?c) (?x . ?x))
        meow-keypad-meta-prefix ?X
        meow-keypad-ctrl-meta-prefix ?C)
  (setq meow-mode-state-list
        '((vterm-mode . +shell)))
  ;; --- visual elements ---
  (setq meow-replace-state-name-list
    '((normal . "N")
      (motion . "M")
      (keypad . "K")
      (insert . "I")
      (beacon . "B")))
  (dolist (pair '((meow-normal-indicator . "#9BA3EB")
                  (meow-motion-indicator . "#A9907E")
                  (meow-keypad-indicator . "#D6D5A8")
                  (meow-insert-indicator . "#7AA874")
                  (meow-beacon-indicator . "#C060A1")))
    (set-face-attribute (car pair) nil
       :weight 'bold :foreground (cdr pair) :inverse-video t))
  (dolist (pair `((meow-normal-cursor . ,(face-attribute 'cursor :background))
                  (meow-insert-cursor . "#7AA874")
                  (meow-beacon-cursor . "#C060A1")))
    (set-face-attribute (car pair) nil :inherit 'unspecified) ;; Unset inherit attr first.
    (set-face-background (car pair) (cdr pair)))
  (setq meow-cursor-type-insert '(bar . 3)
        meow-cursor-type-region-cursor '(bar . 1))
  ;;(meow-setup-indicator) ;; Rendered by doom-modeline.
  ;;(meow-setup-line-number)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;; --- shell state ---
  (setq +meow-+shell-keymap (make-sparse-keymap))
  (meow-define-state +shell
    "meow state for shells"
    :keymap +meow-+shell-keymap)
  (meow-define-keys '+shell
    '("C-SPC" . meow-keypad))
  (setq meow-replace-state-name-list
        (assq-delete-all '+shell meow-replace-state-name-list))
  (add-to-list 'meow-replace-state-name-list '(+shell . "S") t)
  (add-to-list 'meow-indicator-face-alist '(+shell . meow-insert-indicator) t))

;;;;; Keymap tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; which-key, https://github.com/justbur/emacs-which-key
(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right))
