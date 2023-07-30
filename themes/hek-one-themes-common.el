;;; hek-one-themes-common.el --- hek-one-xxx themes common code -*- lexical-binding: t -*-

;;; Commentary:

;; Common code for hek's One xxx themes.
;;
;; Hek's One xxx themes are inspired by Atom One Light/Dark themes and are
;; adapted from Doom Emacs themes. This file provides common definitions and
;; utilities to define themes. The themes themselves are defined in
;; hek-one-xxx-themes.el files.

;;; Code:

(require 'cl-macs) ;; cl-loop

(defvar hek-one-brighter-modeline nil)

(defvar hek-one-brighter-comments nil)

(defvar hek-one-modeline-padding nil
  "Padding added to the mode line. Nil or an integer.")

(defun hek-one-color-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

(defun hek-one-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (cl-loop for it    in (hek-one-color-name-to-rgb color1)
                  for other in (hek-one-color-name-to-rgb color2)
                  collect (+ (* alpha it) (* other (- 1 alpha))))))

(defun hek-one-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (hek-one-blend color "#000000" (- 1 alpha)))

(defun hek-one-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (hek-one-blend color "#FFFFFF" (- 1 alpha)))

(defconst hek-one--base-faces
 '((bold :weight 'bold)
   (bold-italic :inherit '(bold italic))
   (italic :slant  'italic)
   (escape-glyph :foreground cyan)
   (default :background bg :foreground fg)
   (fringe :inherit 'default :foreground base4)
   (region :background region :distant-foreground (hek-one-darken fg 0.2) :extend t)
   (highlight :background highlight :foreground base0 :distant-foreground base8)
   (cursor :background highlight)
   (shadow :foreground base5)
   (minibuffer-prompt :foreground highlight)
   (tooltip :background bg-alt :foreground fg)
   (secondary-selection :background grey :extend t)
   (lazy-highlight
    (&dark  :background (hek-one-darken highlight 0.3)   :foreground base8 :distant-foreground base0 :weight 'bold)
    (&light :background (hek-one-blend bg highlight 0.7) :foreground base0 :distant-foreground base8))
   (match :foreground green      :background base0 :weight 'bold)
   (trailing-whitespace :background red)
   (nobreak-space :inherit 'escape-glyph :underline t)
   (vertical-border :background vertical-bar :foreground vertical-bar)
   (link :foreground highlight :underline t :weight 'bold)
   (error :foreground error)
   (warning :foreground warning)
   (success :foreground success)
   ;;;; font-lock-* faces
   (font-lock-builtin-face :foreground builtin)
   (font-lock-comment-face :foreground comments)
   (font-lock-comment-delimiter-face :inherit 'font-lock-comment-face)
   (font-lock-doc-face :inherit 'font-lock-comment-face :foreground doc-comments)
   (font-lock-constant-face :foreground constants)
   (font-lock-function-name-face :foreground functions)
   (font-lock-keyword-face :foreground keywords)
   (font-lock-string-face :foreground strings)
   (font-lock-type-face :foreground type)
   (font-lock-variable-name-face :foreground variables)
   (font-lock-warning-face :inherit 'warning)
   (font-lock-negation-char-face :inherit 'bold :foreground operators)
   (font-lock-preprocessor-face :inherit 'bold :foreground operators)
   (font-lock-preprocessor-char-face :inherit 'bold :foreground operators)
   (font-lock-regexp-grouping-backslash :inherit 'bold :foreground operators)
   (font-lock-regexp-grouping-construct :inherit 'bold :foreground operators)
   ;;;; mode-line / header-line
   (mode-line           :background bg     :foreground fg     :distant-foreground bg)
   (mode-line-active    :inherit 'mode-line)
   (mode-line-inactive  :background bg-alt :foreground fg-alt :distant-foreground bg-alt)
   (mode-line-emphasis  :foreground highlight :distant-foreground bg)
   (mode-line-highlight :inherit 'highlight :distant-foreground bg)
   (mode-line-buffer-id :weight 'bold)
   (header-line :inherit 'mode-line)
   (header-line-highlight :inherit 'mode-line-highlight)
   ;;;; tab-line/tab-bar (Emacs 27+)
   (tab-line :background bg-alt :foreground bg-alt)
   (tab-line-tab :background bg :foreground fg)
   (tab-line-tab-inactive :inherit 'tab-line-tab :background bg-alt :foreground fg-alt)
   (tab-line-tab-inactive-alternate :inherit 'tab-line-tab-inactive)
   (tab-line-tab-current :background bg :foreground fg)
   ;; (tab-line-special )
   (tab-line-highlight :inherit 'tab-line-tab)
   (tab-line-close-highlight :foreground highlight)
   (tab-bar :inherit 'tab-line)
   (tab-bar-tab :inherit 'tab-line-tab)
   (tab-bar-tab-inactive :inherit 'tab-line-tab-inactive)
   ;;;; Line numbers
   ;; 1. Line number faces must explicitly disable its text style attributes
   ;;    because nearby faces may "bleed" into the line numbers otherwise.
   ;; 2. All other line number plugin faces should inherit from these.
   (line-number
    :inherit 'default
    :foreground base5 :distant-foreground 'unspecified
    :weight 'normal :italic 'unspecified
    :underline 'unspecified :strike-through 'unspecified)
   (line-number-current-line
    :inherit '(hl-line default)
    :foreground fg :distant-foreground 'unspecified
    :weight 'normal :italic 'unspecified
    :underline 'unspecified :strike-through 'unspecified)

   ;;;; --- Package faces ----------------------
   ;;;; auctex <modes:latex-mode>
   (font-latex-bold-face   :inherit 'bold)
   (font-latex-italic-face :inherit 'italic)
   (font-latex-math-face   :foreground blue)
   (font-latex-sectioning-0-face :foreground blue    :weight 'ultra-bold)
   (font-latex-sectioning-1-face :foreground magenta :weight 'semi-bold)
   (font-latex-sectioning-2-face :foreground violet  :weight 'semi-bold)
   (font-latex-sectioning-3-face :foreground (hek-one-lighten blue 0.3)    :weight 'semi-bold)
   (font-latex-sectioning-4-face :foreground (hek-one-lighten magenta 0.3) :weight 'semi-bold)
   (font-latex-sectioning-5-face :foreground (hek-one-lighten violet 0.3)  :weight 'semi-bold)
   (font-latex-script-char-face  :foreground dark-blue)
   (font-latex-string-face   :inherit 'font-lock-string-face)
   (font-latex-warning-face  :inherit 'font-lock-warning-face)
   (font-latex-verbatim-face :inherit 'fixed-pitch :foreground violet :slant 'italic)
   (TeX-error-description-error    :inherit 'error   :weight 'bold)
   (TeX-error-description-warning  :inherit 'warning :weight 'bold)
   (TeX-error-description-tex-said :inherit 'success :weight 'bold)
   ;;;; alert
   (alert-high-face :inherit 'bold :foreground warning)
   (alert-low-face :foreground grey)
   (alert-moderate-face :inherit 'bold :foreground fg-alt)
   (alert-trivial-face :foreground doc-comments)
   (alert-urgent-face :inherit 'bold :foreground error)
   ;;;; annotate
   (annotate-annotation           :background (hek-one-blend highlight bg 0.1) :foreground doc-comments)
   (annotate-annotation-secondary :background (hek-one-blend green bg 0.1)     :foreground doc-comments)
   (annotate-highlight            :background (hek-one-blend highlight bg 0.1) :underline highlight)
   (annotate-highlight-secondary  :background (hek-one-blend green bg 0.1)     :underline green)
   ;;;; ansi-color <built-in>
   (ansi-color-black          :foreground bg      :background bg)
   (ansi-color-red            :foreground red     :background red)
   (ansi-color-green          :foreground green   :background green)
   (ansi-color-yellow         :foreground yellow  :background yellow)
   (ansi-color-blue           :foreground blue    :background blue)
   (ansi-color-magenta        :foreground magenta :background magenta)
   (ansi-color-cyan           :foreground cyan    :background cyan)
   (ansi-color-white          :foreground fg      :background fg)
   (ansi-color-bright-black   :foreground base0   :background base2)
   (ansi-color-bright-red     :foreground (hek-one-lighten red 0.15)     :background (hek-one-lighten red 0.15))
   (ansi-color-bright-green   :foreground (hek-one-lighten green 0.15)   :background (hek-one-lighten green 0.15))
   (ansi-color-bright-yellow  :foreground (hek-one-lighten yellow 0.15)  :background (hek-one-lighten yellow 0.15))
   (ansi-color-bright-blue    :foreground (hek-one-lighten blue 0.15)    :background (hek-one-lighten blue 0.15))
   (ansi-color-bright-magenta :foreground (hek-one-lighten magenta 0.15) :background (hek-one-lighten magenta 0.15))
   (ansi-color-bright-cyan    :foreground (hek-one-lighten cyan 0.15)    :background (hek-one-lighten cyan 0.15))
   (ansi-color-bright-white   :foreground base8   :background base8)
   ;;;; avy
   (avy-background-face :foreground comments)
   (avy-lead-face :background highlight :foreground bg :distant-foreground fg :weight 'bold)
   (avy-lead-face-0
    (&dark  :inherit 'avy-lead-face :background (hek-one-lighten highlight 0.3))
    (&light :inherit 'avy-lead-face :background (hek-one-darken highlight 0.3)))
   (avy-lead-face-1
    (&dark  :inherit 'avy-lead-face :background (hek-one-lighten highlight 0.6))
    (&light :inherit 'avy-lead-face :background (hek-one-darken highlight 0.6)))
   (avy-lead-face-2
    (&dark  :inherit 'avy-lead-face :background (hek-one-lighten highlight 0.9))
    (&light :inherit 'avy-lead-face :background (hek-one-darken highlight 0.9)))
   ;;;; bookmark
   (bookmark-face :background (hek-one-blend highlight bg 0.1) :extend t)
   ;;;; corfu
   (corfu-default :inherit 'tooltip)
   (corfu-current :background bg :foreground fg)
    ;;;; circe
   (circe-fool :foreground doc-comments)
   (circe-highlight-nick-face :weight 'bold :foreground constants)
   (circe-prompt-face :weight 'bold :foreground highlight)
   (circe-server-face :foreground comments)
   (circe-my-message-face :weight 'bold)
   ;;;; cperl <built-in>
   (cperl-array-face :weight 'bold :inherit 'font-lock-variable-name-face)
   (cperl-hash-face :weight 'bold :slant 'italic :inherit 'font-lock-variable-name-face)
   (cperl-nonoverridable-face :inherit 'font-lock-builtin-face)
   ;;;; compilation <built-in>
   (compilation-column-number :inherit 'font-lock-comment-face)
   (compilation-line-number   :foreground highlight)
   (compilation-error   :inherit 'error   :weight 'bold)
   (compilation-warning :inherit 'warning :slant 'italic)
   (compilation-info    :inherit 'success)
   (compilation-mode-line-exit :inherit 'compilation-info)
   (compilation-mode-line-fail :inherit 'compilation-error)
   ;;;; custom <built-in>
   (custom-button            :foreground blue   :background bg     :box '(:line-width 1 :style none))
   (custom-button-unraised   :foreground violet :background bg     :box '(:line-width 1 :style none))
   (custom-button-pressed-unraised :foreground bg :background violet :box '(:line-width 1 :style none))
   (custom-button-pressed    :foreground bg     :background blue   :box '(:line-width 1 :style none))
   (custom-button-mouse      :foreground bg     :background blue   :box '(:line-width 1 :style none))
   (custom-variable-button   :foreground green  :underline t)
   (custom-saved             :foreground green  :background (hek-one-blend green bg 0.2) :weight 'bold)
   (custom-comment           :foreground fg     :background region)
   (custom-comment-tag       :foreground grey)
   (custom-modified          :foreground blue   :background (hek-one-blend blue bg 0.2))
   (custom-variable-tag      :foreground magenta)
   (custom-visibility        :foreground blue   :underline 'unspecified)
   (custom-group-subtitle    :foreground red)
   (custom-group-tag         :foreground violet)
   (custom-group-tag-1       :foreground blue)
   (custom-set               :foreground yellow :background bg)
   (custom-themed            :foreground yellow :background bg)
   (custom-invalid           :foreground red    :background (hek-one-blend red bg 0.2))
   (custom-variable-obsolete :foreground grey   :background bg)
   (custom-state             :foreground green  :background (hek-one-blend green bg 0.2))
   (custom-changed           :foreground blue   :background bg)
   ;;;; diff-hl
   (diff-hl-change :foreground vc-modified :background vc-modified)
   (diff-hl-delete :foreground vc-deleted :background vc-deleted)
   (diff-hl-insert :foreground vc-added :background vc-added)
   ;;;; diff-mode <built-in>
   (diff-added   :inherit 'hl-line :foreground green)
   (diff-changed :foreground violet)
   (diff-context
    (&dark  :foreground (hek-one-darken fg 0.12))
    (&light :foreground (hek-one-lighten fg 0.12)))
   (diff-removed :foreground red :background base3)
   (diff-header  :foreground cyan)
   (diff-file-header :foreground blue)
   (diff-hunk-header :foreground violet)
   (diff-refine-added   :inherit 'diff-added :inverse-video t)
   (diff-refine-changed :inherit 'diff-changed :inverse-video t)
   (diff-refine-removed :inherit 'diff-removed :inverse-video t)
   ;;;; dired <built-in>
   (dired-directory  :foreground builtin)
   (dired-ignored    :foreground comments)
   (dired-flagged    :foreground red)
   (dired-header     :foreground blue :weight 'bold)
   (dired-mark       :foreground orange :weight 'bold)
   (dired-marked     :foreground magenta :weight 'bold :inverse-video t)
   (dired-perm-write :foreground fg :underline t)
   (dired-symlink    :foreground cyan :weight 'bold)
   (dired-warning    :foreground warning)
   ;;;; disk-usage
   (disk-usage-children :foreground yellow)
   (disk-usage-percent  :foreground violet)
   (disk-usage-size     :foreground blue)
   (disk-usage-symlink  :foreground cyan :weight 'bold)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (hek-one-blend selection bg 0.7) :weight 'bold :extend t)
   (ediff-fine-diff-B    :inherit 'ediff-fine-diff-A)
   (ediff-fine-diff-C    :inherit 'ediff-fine-diff-A)
   (ediff-current-diff-A :background (hek-one-blend selection bg 0.3) :extend t)
   (ediff-current-diff-B :inherit 'ediff-current-diff-A)
   (ediff-current-diff-C :inherit 'ediff-current-diff-A)
   (ediff-even-diff-A    :inherit 'hl-line)
   (ediff-even-diff-B    :inherit 'ediff-even-diff-A)
   (ediff-even-diff-C    :inherit 'ediff-even-diff-A)
   (ediff-odd-diff-A     :inherit 'ediff-even-diff-A)
   (ediff-odd-diff-B     :inherit 'ediff-odd-diff-A)
   (ediff-odd-diff-C     :inherit 'ediff-odd-diff-A)
   ;;;; embark
   (embark-target :inherit 'vertico-current)
   ;;;; eshell <built-in>
   (eshell-prompt        :foreground highlight :weight 'bold)
   (eshell-ls-archive    :foreground magenta)
   (eshell-ls-backup     :foreground yellow)
   (eshell-ls-clutter    :foreground red)
   (eshell-ls-directory  :foreground blue)
   (eshell-ls-executable :foreground green)
   (eshell-ls-missing    :foreground red)
   (eshell-ls-product    :foreground orange)
   (eshell-ls-readonly   :foreground orange)
   (eshell-ls-special    :foreground violet)
   (eshell-ls-symlink    :foreground cyan)
   (eshell-ls-unreadable :foreground base5)
   (evil-goggles-default-face :inherit 'region :background (hek-one-blend region bg 0.5))
   ;;;; eww
   (eww-form-checkbox :inherit 'eww-form-file)
   (eww-form-file   :inherit 'eww-form-submit :background bg-alt)
   (eww-form-select :inherit 'eww-form-submit :background bg-alt)
   (eww-form-submit :inherit 'eww-form-text :box `(:line-width 2 :style released-button) :background base2)
   (eww-form-text :box `(:line-width 1 :color ,base3) :background bg :foreground fg :distant-foreground bg)
   (eww-form-textarea :inherit 'eww-form-text)
   (eww-invalid-certificate :foreground error)
   (eww-valid-certificate :foreground highlight)
   ;;;; flymake
   (flymake-error   :underline `(:style wave :color ,red))
   (flymake-note    :underline `(:style wave :color ,green))
   (flymake-warning :underline `(:style wave :color ,orange))
   ;;;; flyspell <built-in>
   (flyspell-incorrect :underline `(:style wave :color ,error) :inherit 'unspecified)
   (flyspell-duplicate :underline `(:style wave :color ,warning) :inherit 'unspecified)
   ;;;; hi-lock <built-in>
   (hi-yellow   :background yellow)
   (hi-pink     :background magenta)
   (hi-red-b    :foreground red :weight 'bold)
   (hi-green    :background green)
   (hi-green-b  :foreground green :weight 'bold)
   (hi-blue     :background blue)
   (hi-blue-b   :foreground blue :weight 'bold)
   ;; (hi-black-b  :weight 'bold)
   ;; (hi-black-hb :inherit 'variable-pitch :weight 'bold :height 1.67)
   ;;;; hideshow <built-in>
   (+fold-hideshow-folded-face :inherit 'font-lock-comment-face
                               :weight 'light
                               :background (hek-one-darken bg 0.125))
   ;;;; hl-fill-column-face
   (hl-fill-column-face :inherit '(hl-line shadow))
   ;;;; hl-line (built-in)
   (hl-line :background bg-alt :extend t)
   ;;;; hl-todo
   (hl-todo :foreground red :weight 'bold)
   ;;;; isearch <built-in>
   (isearch :inherit 'lazy-highlight :weight 'bold)
   (isearch-fail :background error :foreground base0 :weight 'bold)
   ;;;; vertico
   (vertico-current :background region :extend t)
   ;;;; js2-mode <modes:js2-mode,js2-jsx-mode>
   (js2-function-param    :foreground variables)
   (js2-function-call     :foreground functions)
   (js2-object-property   :foreground violet)
   (js2-jsdoc-tag         :foreground doc-comments)
   (js2-external-variable :foreground operators)
   ;;;; magit
   (magit-bisect-bad        :foreground red)
   (magit-bisect-good       :foreground green)
   (magit-bisect-skip       :foreground orange)
   (magit-blame-hash        :foreground cyan)
   (magit-blame-date        :foreground red)
   (magit-blame-heading     :foreground orange :background base3 :extend t)
   (magit-branch-current    :foreground blue)
   (magit-branch-local      :foreground cyan)
   (magit-branch-remote     :foreground green)
   (magit-cherry-equivalent :foreground violet)
   (magit-cherry-unmatched  :foreground cyan)
   (magit-diff-added             :foreground (hek-one-darken vc-added 0.2)  :background (hek-one-blend vc-added bg 0.1) :extend t)
   (magit-diff-added-highlight   :foreground vc-added                    :background (hek-one-blend vc-added bg 0.2) :weight 'bold :extend t)
   (magit-diff-base              :foreground (hek-one-darken orange 0.2) :background (hek-one-blend orange bg 0.1) :extend t)
   (magit-diff-base-highlight    :foreground orange                   :background (hek-one-blend orange bg 0.2) :weight 'bold :extend t)
   (magit-diff-context           :foreground (hek-one-darken fg 0.4) :background bg :extend t)
   (magit-diff-context-highlight :foreground fg                   :background bg-alt :extend t)
   (magit-diff-file-heading           :foreground fg :weight 'bold :extend t)
   (magit-diff-file-heading-selection :foreground magenta               :background dark-blue :weight 'bold :extend t)
   (magit-diff-hunk-heading           :foreground bg                    :background (hek-one-blend violet bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground bg                    :background violet :weight 'bold :extend t)
   (magit-diff-lines-heading          :foreground yellow :background red :extend t :extend t)
   (magit-diff-removed                :foreground (hek-one-darken vc-deleted 0.2) :background (hek-one-blend vc-deleted base3 0.1) :extend t)
   (magit-diff-removed-highlight      :foreground vc-deleted                   :background (hek-one-blend vc-deleted base3 0.2) :weight 'bold :extend t)
   (magit-diffstat-added              :foreground vc-added)
   (magit-diffstat-removed            :foreground vc-deleted)
   (magit-dimmed :foreground comments)
   (magit-hash :foreground comments)
   (magit-header-line :background dark-blue :foreground base8 :weight 'bold
                      :box `(:line-width 3 :color ,dark-blue))
   (magit-filename :foreground violet)
   (magit-log-author :foreground orange)
   (magit-log-date :foreground blue)
   (magit-log-graph :foreground comments)
   (magit-process-ng :inherit 'error)
   (magit-process-ok :inherit 'success)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-reflog-commit :foreground green)
   (magit-reflog-merge :foreground green)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :inherit 'error)
   (magit-refname :foreground comments)
   (magit-section-heading :foreground blue :weight 'bold :extend t)
   (magit-section-heading-selection :foreground orange :weight 'bold :extend t)
   (magit-section-highlight :inherit 'hl-line)
   (magit-section-secondary-heading :foreground violet :weight 'bold :extend t)
   (magit-sequence-drop :foreground red)
   (magit-sequence-head :foreground blue)
   (magit-sequence-part :foreground orange)
   (magit-sequence-stop :foreground green)
   (magit-signature-bad :inherit 'error)
   (magit-signature-error :inherit 'error)
   (magit-signature-expired :foreground orange)
   (magit-signature-good :inherit 'success)
   (magit-signature-revoked :foreground magenta)
   (magit-signature-untrusted :foreground yellow)
   (magit-tag :foreground yellow)
   ;;;; make-mode <built-in> <modes:makefile-mode,makefile-automake-mode,makefile-makepp-mode,makefile-gmake-mode,makefile-imake-mode,makefile-bsdmake-mode>
   (makefile-targets :foreground blue)
   ;;;; man <built-in> <mode:Man-mode>
   (Man-overstrike :inherit 'bold :foreground operators)
   (Man-underline :inherit 'underline :foreground keywords)
   ;;;; markdown-mode <modes:markdown-mode,gfm-mode>
   (markdown-header-face           :inherit 'bold :foreground highlight)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground red)
   (markdown-list-face             :foreground red)
   (markdown-link-face             :foreground highlight)
   (markdown-url-face              :foreground magenta :weight 'normal)
   (markdown-italic-face           :inherit 'italic :foreground violet)
   (markdown-bold-face             :inherit 'bold   :foreground orange)
   (markdown-markup-face           :foreground operators)
   (markdown-blockquote-face       :inherit 'italic :foreground doc-comments)
   (markdown-pre-face              :foreground strings)
   (markdown-code-face             :background base3 :extend t)
   (markdown-reference-face        :foreground doc-comments)
   (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face) :extend nil)
   (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
   (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
   (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
   (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
   (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)
   ;;;; marginalia
   (marginalia-documentation   :inherit 'font-lock-doc-face)
   (marginalia-file-priv-dir   :foreground blue)
   (marginalia-file-priv-exec  :foreground green)
   (marginalia-file-priv-link  :foreground violet)
   (marginalia-file-priv-other :foreground magenta)
   (marginalia-file-priv-rare  :foreground fg)
   (marginalia-file-priv-read  :foreground yellow)
   (marginalia-file-priv-write :foreground red)
   (marginalia-number          :foreground numbers)
   (marginalia-size            :foreground violet)
   (marginalia-lighter         :foreground violet)
   ;;;; message <built-in>
   (message-header-name       :foreground green)
   (message-header-subject    :foreground highlight :weight 'bold)
   (message-header-to         :foreground highlight :weight 'bold)
   (message-header-cc         :inherit 'message-header-to :foreground (hek-one-darken highlight 0.15))
   (message-header-other      :foreground violet)
   (message-header-newsgroups :foreground yellow)
   (message-header-xheader    :foreground doc-comments)
   (message-separator         :foreground comments)
   (message-mml               :foreground comments :slant 'italic)
   (message-cited-text   :inherit 'gnus-cite-1)
   (message-cited-text-1 :inherit 'gnus-cite-2)
   (message-cited-text-2 :inherit 'gnus-cite-3)
   (message-cited-text-3 :inherit 'gnus-cite-4)
   (message-cited-text-4 :inherit 'gnus-cite-5)
   ;;;; orderless
   (orderless-match-face-0 :weight 'bold :foreground (hek-one-blend blue    fg 0.6) :background (hek-one-blend blue    bg 0.1))
   (orderless-match-face-1 :weight 'bold :foreground (hek-one-blend magenta fg 0.6) :background (hek-one-blend magenta bg 0.1))
   (orderless-match-face-2 :weight 'bold :foreground (hek-one-blend green   fg 0.6) :background (hek-one-blend green   bg 0.1))
   (orderless-match-face-3 :weight 'bold :foreground (hek-one-blend yellow  fg 0.6) :background (hek-one-blend yellow  bg 0.1))
   ;;;; outline <built-in>
   (outline-1 :foreground blue                        :weight 'bold :extend t)
   (outline-2 :foreground magenta                     :weight 'bold :extend t)
   (outline-3 :foreground violet                      :weight 'bold :extend t)
   (outline-4 :foreground (hek-one-lighten blue 0.25)    :weight 'bold :extend t)
   (outline-5 :foreground (hek-one-lighten magenta 0.25) :weight 'bold :extend t)
   (outline-6 :foreground (hek-one-lighten blue 0.5)     :weight 'bold :extend t)
   (outline-7 :foreground (hek-one-lighten magenta 0.5)  :weight 'bold :extend t)
   (outline-8 :foreground (hek-one-lighten blue 0.8)     :weight 'bold :extend t)
   ;;;; pkgbuild-mode <modes:pkgbuild-mode>
   (pkgbuild-error-face :underline `(:style wave :color ,red))
   ;;;; popup
   (popup-face           :inherit 'tooltip)
   (popup-tip-face       :inherit 'popup-face :foreground violet :background base0)
   (popup-selection-face :background selection)
    ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face (&light :foreground "#a8007f") (&dark :foreground "#ff62d4"))
   (rainbow-delimiters-depth-2-face (&light :foreground "#005f88") (&dark :foreground "#3fdfd0"))
   (rainbow-delimiters-depth-3-face (&light :foreground "#904200") (&dark :foreground "#fba849"))
   (rainbow-delimiters-depth-4-face (&light :foreground "#7f10d0") (&dark :foreground "#9f80ff"))
   (rainbow-delimiters-depth-5-face (&light :foreground "#006800") (&dark :foreground "#4fe42f"))
   (rainbow-delimiters-depth-6-face (&light :foreground "#b60000") (&dark :foreground "#fe6060"))
   (rainbow-delimiters-depth-7-face (&light :foreground "#1f1fce") (&dark :foreground "#4fafff"))
   (rainbow-delimiters-depth-8-face (&light :foreground "#605b00") (&dark :foreground "#f0dd60"))
   (rainbow-delimiters-depth-9-face (&light :foreground "#000000") (&dark :foreground "#ffffff"))
   (rainbow-delimiters-base-error-face :inherit 'rainbow-delimiters-base-face :foreground error)
   (rainbow-delimiters-base-face :inherit 'default)
   (rainbow-delimiters-unmatched-face  :foreground red :weight 'bold :inverse-video t)
   (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
   ;;;; re-builder <built-in>
   (reb-match-0 :foreground orange  :inverse-video t)
   (reb-match-1 :foreground magenta :inverse-video t)
   (reb-match-2 :foreground green   :inverse-video t)
   (reb-match-3 :foreground yellow  :inverse-video t)
   ;;;; rst <built-in> <modes:rst-mode>
   (rst-block :inherit 'font-lock-constant-face)
   (rst-level-1 :inherit 'rst-adornment :weight 'bold)
   (rst-level-2 :inherit 'rst-adornment :weight 'bold)
   (rst-level-3 :inherit 'rst-adornment :weight 'bold)
   (rst-level-4 :inherit 'rst-adornment :weight 'bold)
   (rst-level-5 :inherit 'rst-adornment :weight 'bold)
   (rst-level-6 :inherit 'rst-adornment :weight 'bold)
   ;;;; sh-script <built-in> <modes:sh-mode,shell-script-mode>
   (sh-heredoc :inherit 'font-lock-string-face :weight 'normal)
   (sh-quoted-exec :inherit 'font-lock-preprocessor-face)
   ;;;; show-paren <built-in>
   (show-paren-match :inherit 'paren-face-match)
   (show-paren-mismatch :inherit 'paren-face-mismatch)
   ;;;; smerge-tool
   (smerge-lower :background (hek-one-blend green bg 0.2))
   (smerge-upper :background (hek-one-blend red base3 0.2))
   (smerge-base  :background (hek-one-blend blue bg 0.2))
   (smerge-markers :background comments :foreground bg :distant-foreground fg :weight 'bold)
   (smerge-refined-added   :inherit 'diff-added :inverse-video t)
   (smerge-refined-removed :inherit 'diff-removed :inverse-video t)
   ;;;; solaire-mode
   (solaire-default-face  :inherit 'default :background bg-alt)
   (solaire-hl-line-face  :inherit 'hl-line :background bg :extend t)
   (solaire-org-hide      :inherit 'org-hide :foreground bg-alt)
   ;;;; tabbar
   (tabbar-default             :foreground bg :background bg :height 1.0)
   (tabbar-highlight           :foreground fg :background selection :distant-foreground bg)
   (tabbar-button              :foreground fg :background bg)
   (tabbar-button-highlight    :inherit 'tabbar-button :inverse-video t)
   (tabbar-modified            :inherit 'tabbar-default :foreground red :weight 'bold)
   (tabbar-unselected          :inherit 'tabbar-default :foreground base5)
   (tabbar-unselected-modified :inherit 'tabbar-modified)
   (tabbar-selected
    :inherit 'tabbar-default :weight 'bold
    :foreground fg :background bg-alt)
   (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)
   ;;;; term <built-in>
   (term               :foreground fg)
   (term-bold          :weight 'bold)
   (term-color-black   :background base0   :foreground base0)
   (term-color-red     :background red     :foreground red)
   (term-color-green   :background green   :foreground green)
   (term-color-yellow  :background yellow  :foreground yellow)
   (term-color-blue    :background blue    :foreground blue)
   (term-color-magenta :background magenta :foreground magenta)
   (term-color-cyan    :background cyan    :foreground cyan)
   (term-color-white   :background base8   :foreground base8)
   ;;;; typescript-mode <modes:typescript-mode,typescript-tsx-mode>
   (typescript-jsdoc-tag :foreground doc-comments)
   (typescript-jsdoc-type :foreground (hek-one-darken doc-comments 0.15))
   (typescript-jsdoc-value :foreground (hek-one-lighten doc-comments 0.15))
   ;;;; vterm
   (vterm-color-black   :background (hek-one-lighten base0 0.25)   :foreground base0)
   (vterm-color-red     :background (hek-one-lighten red 0.25)     :foreground red)
   (vterm-color-green   :background (hek-one-lighten green 0.25)   :foreground green)
   (vterm-color-yellow  :background (hek-one-lighten yellow 0.25)  :foreground yellow)
   (vterm-color-blue    :background (hek-one-lighten blue 0.25)    :foreground blue)
   (vterm-color-magenta :background (hek-one-lighten magenta 0.25) :foreground magenta)
   (vterm-color-cyan    :background (hek-one-lighten cyan 0.25)    :foreground cyan)
   (vterm-color-white   :background (hek-one-lighten base8 0.25)   :foreground base8)
   ;;;; web-mode <modes:web-mode>
   (web-mode-block-control-face     :foreground builtin)
   (web-mode-block-delimiter-face   :foreground builtin)
   (web-mode-css-property-name-face :foreground type)
   (web-mode-doctype-face           :foreground comments)
   (web-mode-html-tag-face          :foreground methods)
   (web-mode-html-tag-bracket-face  :foreground methods)
   (web-mode-html-attr-name-face    :foreground type)
   (web-mode-html-attr-value-face   :foreground strings)
   (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
   (web-mode-block-control-face     :foreground orange)
   (web-mode-html-tag-bracket-face  :foreground operators)
   (web-mode-json-key-face          :foreground strings)
   (web-mode-json-context-face      :foreground strings)
   (web-mode-keyword-face           :foreground keywords)
   (web-mode-string-face            :foreground strings)
   (web-mode-type-face              :foreground type)
   ;;;; wgrep <built-in>
   (wgrep-face :weight 'bold :foreground green :background base5)
   (wgrep-delete-face :foreground base3 :background red)
   (wgrep-done-face   :foreground blue)
   (wgrep-file-face   :foreground comments)
   (wgrep-reject-face :foreground red :weight 'bold)
   ;;;; which-key
   (which-key-key-face                   :foreground green)
   (which-key-group-description-face     :foreground violet)
   (which-key-command-description-face   :foreground blue)
   (which-key-local-map-description-face :foreground magenta)
   ;;;; whitespace <built-in>
   (whitespace-empty    :background base3)
   (whitespace-space    :foreground base4)
   (whitespace-newline  :foreground base4)
   (whitespace-tab
    :foreground base4
    :background (if (default-value 'indent-tabs-mode) 'unspecified base3))
   (whitespace-indentation
    :foreground base4
    :background (if (default-value 'indent-tabs-mode) base3 'unspecified))
   (whitespace-trailing :inherit 'trailing-whitespace)
   (whitespace-line     :background base0 :foreground red :weight 'bold)
   ;;;; widget
   (widget-button-pressed :foreground red)
   (widget-documentation  :foreground green)
   (widget-single-line-field :background base3 :distant-foreground bg)
   (widget-field :background base3 :distant-foreground bg
                 :box `(:line-width -1 :color ,grey) :extend t)
   ;;;; woman <built-in>
   (woman-bold :inherit 'Man-overstrike)
   (woman-italic :inherit 'Man-underline)
   ;;;; yasnippet
   (yas-field-highlight-face :inherit 'match)
   ;;;; xref <built-in>
   (xref-file-header :inherit 'compilation-info)
   (xref-line-number :inherit 'compilation-line-number)
   (xref-match :inherit 'match)
   ;;;; --- END Package faces ------------------
   ))

(defconst hek-one--base-vars
 '(
   ;;;; --- Package variables ------------------

   ;;;; ansi-color <built-in> DEPRECATED
   (ansi-color-names-vector (vector bg red green yellow blue magenta cyan fg))
   ;;;; rustic <modes:rustic-mode>
   (rustic-ansi-faces (vector bg red green yellow blue magenta cyan fg))
   ;;;; pdf-tools
   (pdf-view-midnight-colors (cons fg bg))
   ;;;; vc <built-in>
   (vc-annotate-color-map
    (list
     (cons 20  green)
     (cons 40  (hek-one-blend yellow green (/ 1.0 3)))
     (cons 60  (hek-one-blend yellow green (/ 2.0 3)))
     (cons 80  yellow)
     (cons 100 (hek-one-blend orange yellow (/ 1.0 3)))
     (cons 120 (hek-one-blend orange yellow (/ 2.0 3)))
     (cons 140 orange)
     (cons 160 (hek-one-blend magenta orange (/ 1.0 3)))
     (cons 180 (hek-one-blend magenta orange (/ 2.0 3)))
     (cons 200 magenta)
     (cons 220 (hek-one-blend red magenta (/ 1.0 3)))
     (cons 240 (hek-one-blend red magenta (/ 2.0 3)))
     (cons 260 red)
     (cons 280 (hek-one-blend grey red (/ 1.0 4)))
     (cons 300 (hek-one-blend grey red (/ 2.0 4)))
     (cons 320 (hek-one-blend grey red (/ 3.0 4)))
     (cons 340 base5)
     (cons 360 base5)))
   (vc-annotate-very-old-color nil)
   (vc-annotate-background bg)
   ;;;; --- END Package variables --------------
   ))

(defun hek-one--make-face-form (name-and-attr)
  ;; `(name :k1 v1 :k2 v2 ...)' => `(name ((t :k1 v1 ...)))'
  ;; `(name (&dark :k1 v1 ...) (&light :k1 v1 ...))' => `(name (((background dark) ...) ...))'
  (let ((name (car name-and-attr))
        (attr (cdr name-and-attr)))
    (if (symbolp (car attr))
        `(list ',name (list (list t ,(cons 'list attr))))
      (let ((spec '()))
        (dolist (form attr)
          (push (list
                 'list
                 (list
                  'quote
                  (list
                   (cond
                    ((eq (car form) '&dark)
                     '(background dark))
                    ((eq (car form) '&light)
                     '(background light))
                    (t
                     (error "bad type `%S' in `%S'" (car form) name-and-attr)))))
                 (cons 'list (cdr form)))
                spec))
        (push 'list spec)
        `(list ',name ,spec)))))

(defun hek-one--make-var-form (name-and-exp)
  ;; `(name exp)' => `(name exp)'
  (let ((name (car name-and-exp))
        (exp  (cdr name-and-exp)))
    `(list ',name ,@exp)))

(defun hek-one--merge-faces-make-forms (new base)
  ;; Faces in NEW that marked `&override' (like `(name &override attr...)') will
  ;; only override mentioned attributes while keep other attributes from BASE.
  ;; Other faces re-write the face.
  (cl-loop
   for base-def in base
   for (base-def-name . base-def-attr) = base-def
   for new-def = (assq base-def-name new)
   when new-def
   do (setq new (assq-delete-all base-def-name new))
   collect (hek-one--make-face-form
            (if (null new-def)
                base-def
              (let ((new-def-attr (cdr new-def)))
                (when (eq (car new-def-attr) '&override)
                  (setq new-def-attr (cdr new-def-attr))
                  (unless (keywordp (car new-def-attr))
                    (error "cannot merge complex face def: `%S'" new-def-attr))
                  (cl-loop
                   for (key base-val) on base-def-attr by #'cddr
                   unless (plist-get new-def-attr key)
                   do (push base-val new-def-attr) (push key new-def-attr)))
                (cons base-def-name new-def-attr))))
   into forms
   finally (dolist (new-def new)
             (push (hek-one--make-face-form new-def) forms))
   finally return forms))

(defun hek-one--merge-vars-make-forms (new base)
  (cl-loop
   for base-def in base
   for (base-def-name _base-def-exp) = base-def
   for new-def = (assq base-def-name new)
   when new-def
   do (setq new (assq-delete-all base-def-name new))
   collect (hek-one--make-var-form
            (if new-def (cons base-def-name (cdr new-def)) base-def))
   into forms
   finally (dolist (new-def new)
             (push (hek-one--make-var-form new-def) forms))
   finally return forms))

(defmacro hek-one-def-theme (name colors colors2 faces vars)
  "Define a hek one theme."
  (setq name (intern (concat "hek-one-" (symbol-name name))))
  `(progn
     (deftheme ,name)
     (let (,@colors)
       (let ((custom--inhibit-theme-enable nil)
             ,@colors2)
         (custom-theme-set-faces
          ',name ,@(hek-one--merge-faces-make-forms faces hek-one--base-faces))
         (custom-theme-set-variables
          ',name ,@(hek-one--merge-vars-make-forms vars hek-one--base-vars))))
     (provide-theme ',name)))

(defun hek-one--clean-up ()
  (mapc #'makunbound
        '(hek-one--base-faces hek-one--base-vars))
  (mapc #'fmakunbound
        '( hek-one--make-face-form hek-one--make-var-form
           hek-one--merge-faces-make-forms hek-one--merge-vars-make-forms
           hek-one-def-theme hek-one--clean-up )))

(provide 'hek-one-themes-common)
;;; hek-one-themes-common.el ends here
