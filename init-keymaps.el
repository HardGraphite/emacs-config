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
     ("s" . consult-imenu) ;; imenu
     ("l" . consult-goto-line)
     )))

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
    ;; -- word selections --
    '("b" . meow-back-word)
    '("B" . meow-back-symbol)
    '("e" . meow-next-word)
    '("E" . meow-next-symbol)
    '("w" . meow-mark-word)
    '("W" . meow-mark-symbol)
    ;; -- line/block/join selections
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
    ;; -- deletions --
    '("d" . meow-delete)
    '("D" . meow-backward-delete)
    ;; -- copying and pasting --
    '("y" . meow-save)
    '("s" . meow-kill)
    '("p" . meow-yank)
    '("r" . meow-replace)
    ;; -- search/jump --
    '("/" . consult-line) ;; '("/" . meow-visit)
    '("n" . meow-search)
    '("X" . meow-goto-line)
    '("v" . scroll-up-command)
    '("V" . scroll-down-command)
    ;; -- undo/redo --
    '("u" . meow-undo)
    '("U" . undo-redo) ;;'("U" . meow-undo-in-selection)
    '("C-r" . undo-redo)
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
    '("M-u" . scroll-up-command)
    '("M-d" . scroll-down-command)
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
    `("p" . ,project-prefix-map)
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
        meow-keypad-meta-prefix ?m
        meow-keypad-ctrl-meta-prefix ?M)
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
  (which-key-mode 1))
