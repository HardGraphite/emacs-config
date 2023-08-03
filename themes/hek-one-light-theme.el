;;; hek-one-light-theme.el --- Hek's One Light theme -*- lexical-binding: t; no-byte-compile: t; -*-

;; See hek-one-themes-common.el for more info.

(let ((load-path (cons (file-name-directory load-file-name) load-path)))
  (require 'hek-one-themes-common))

(hek-one-def-theme
 ;;;;; ===== name =====
 light

 ;;;;; ===== colors =====
 ((bg      "#fafafa")
  (fg      "#383a42")
  (bg-alt  "#f0f0f0")
  (fg-alt  "#c6c7c7")

  (base0  "#f0f0f0")
  (base1  "#e7e7e7")
  (base2  "#dfdfdf")
  (base3  "#c6c7c7")
  (base4  "#9ca0a4")
  (base5  "#383a42")
  (base6  "#202328")
  (base7  "#1c1f24")
  (base8  "#1b2229")

  (grey       "#9ca0a4") ;; base 4
  (red        "#e45649")
  (orange     "#da8548")
  (green      "#50a14f")
  (teal       "#4db5bd")
  (yellow     "#986801")
  (blue       "#4078f2")
  (dark-blue  "#a0bcf8")
  (magenta    "#a626a4")
  (violet     "#b751b6")
  (cyan       "#0184bc")
  (dark-cyan  "#005478"))

 ;;;;; ===== colors2 =====
 ((highlight      blue)
  (vertical-bar   (hek-one-darken base2 0.1))
  (selection      dark-blue)
  (builtin        magenta)
  (comments       (if hek-one-brighter-comments cyan base4))
  (doc-comments   (hek-one-darken (if hek-one-brighter-comments cyan base4) 0.15))
  (constants      violet)
  (functions      magenta)
  (keywords       red)
  (methods        cyan)
  (operators      blue)
  (type           yellow)
  (strings        green)
  (variables      (hek-one-darken magenta 0.36))
  (numbers        orange)
  (region         (hek-one-darken bg-alt 0.1))
  (error          red)
  (warning        yellow)
  (success        green)
  (vc-modified    orange)
  (vc-added       green)
  (vc-deleted     red)

  (modeline-fg              fg)
  (modeline-fg-alt          (hek-one-blend violet base4 (if hek-one-brighter-modeline 0.5 0.2)))
  (modeline-bg              (if hek-one-brighter-modeline (hek-one-darken base2 0.05) base1))
  (modeline-bg-alt          (if hek-one-brighter-modeline (hek-one-darken base2 0.1) base2))
  (modeline-bg-inactive     (hek-one-darken bg 0.1))
  (modeline-bg-alt-inactive (hek-one-darken bg-alt 0.05))
  (-modeline-pad            hek-one-modeline-padding))

 ;;;;; ===== faces =====
 ( ;;;; Base theme face overrides
  (font-lock-comment-face &override :background (if hek-one-brighter-comments base0 'unspecified))
  (font-lock-doc-face &override :slant 'italic)
  (line-number &override :foreground (hek-one-lighten base4 0.15))
  (line-number-current-line &override :foreground base8)
  (mode-line
   :background modeline-bg :foreground modeline-fg
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
  (mode-line-inactive
   :background modeline-bg-inactive :foreground modeline-fg-alt
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
  (mode-line-emphasis
   :foreground (if hek-one-brighter-modeline base8 highlight))
  (shadow :foreground base4)
  (tooltip :background base1 :foreground fg)

   ;;;; css-mode <built-in> / scss-mode
  (css-proprietary-property :foreground orange)
  (css-property             :foreground green)
  (css-selector             :foreground blue)
   ;;;; ediff <built-in>
  (ediff-current-diff-A        :foreground red   :background (hek-one-lighten red 0.8))
  (ediff-current-diff-B        :foreground green :background (hek-one-lighten green 0.8))
  (ediff-current-diff-C        :foreground blue  :background (hek-one-lighten blue 0.8))
  (ediff-current-diff-Ancestor :foreground teal  :background (hek-one-lighten teal 0.8))
   ;;;; magit
  (magit-blame-heading :foreground orange :background bg-alt)
  (magit-diff-removed  :foreground (hek-one-darken red 0.2) :background (hek-one-blend red bg 0.1))
  (magit-diff-removed-highlight :foreground red :background (hek-one-blend red bg 0.2) :weight 'bold)
   ;;;; markdown-mode
  (markdown-markup-face :foreground base5)
  (markdown-header-face :inherit 'bold :foreground red)
  (markdown-code-face &override :background base1)
  (mmm-default-submode-face :background base1)
   ;;;; outline <built-in>
  (outline-1 &override :foreground red)
  (outline-2 &override :foreground orange)
   ;;;; vertico
  (vertico-current :background base2)
   ;;;; solaire-mode
  (solaire-mode-line-face
   :inherit 'mode-line
   :background modeline-bg-alt
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
  (solaire-mode-line-inactive-face
   :inherit 'mode-line-inactive
   :background modeline-bg-alt-inactive
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt-inactive)))
   ;;;; web-mode
  (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
  (wgrep-face :background base1)
   ;;;; whitespace
  (whitespace-tab &override :background (if (default-value 'indent-tabs-mode) 'unspecified base0))
  (whitespace-indentation &override :background (if (default-value 'indent-tabs-mode) base0 'unspecified)))

 ;;;;; ===== vars =====
 ()
 )
