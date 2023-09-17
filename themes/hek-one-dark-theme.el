;;; hek-one-dark-theme.el --- Hek's One Dark theme -*- lexical-binding: t; no-byte-compile: t; -*-

;; See hek-one-themes-common.el for more info.

(let ((load-path (cons (file-name-directory load-file-name) load-path)))
  (require 'hek-one-themes-common))

(hek-one-def-theme
 ;;;;; ===== name =====
 dark

 ;;;;; ===== colors =====
 ((bg      "#282c34")
  (fg      "#bbc2cf")
  (bg-alt  "#21242b")
  (fg-alt  "#5B6268")

  (base0  "#1B2229")
  (base1  "#1c1f24")
  (base2  "#202328")
  (base3  "#23272e")
  (base4  "#3f444a")
  (base5  "#5B6268")
  (base6  "#73797e")
  (base7  "#9ca0a4")
  (base8  "#DFDFDF")

  (grey       "#3f444a") ;; base4
  (red        "#ff6c6b")
  (orange     "#da8548")
  (green      "#98be65")
  (teal       "#4db5bd")
  (yellow     "#ECBE7B")
  (blue       "#51afef")
  (dark-blue  "#2257A0")
  (magenta    "#c678dd")
  (violet     "#a9a1e1")
  (cyan       "#46D9FF")
  (dark-cyan  "#5699AF"))

 ;;;;; ===== colors2 =====
 ((highlight      blue)
  (vertical-bar   (hek-one-darken base1 0.1))
  (selection      dark-blue)
  (builtin        magenta)
  (comments       (if hek-one-brighter-comments dark-cyan base5))
  (doc-comments   (hek-one-lighten (if hek-one-brighter-comments dark-cyan base5) 0.25))
  (constants      violet)
  (functions      magenta)
  (keywords       blue)
  (methods        cyan)
  (operators      blue)
  (type           yellow)
  (strings        green)
  (variables      (hek-one-lighten magenta 0.4))
  (numbers        orange)
  (region         (hek-one-lighten bg-alt 0.18))
  (error          red)
  (warning        yellow)
  (success        green)
  (vc-modified    orange)
  (vc-added       green)
  (vc-deleted     red)

  (modeline-fg              fg)
  (modeline-fg-alt          base5)
  (modeline-bg              (if hek-one-brighter-modeline (hek-one-darken blue 0.45) (hek-one-darken bg-alt 0.1)))
  (modeline-bg-alt          (if hek-one-brighter-modeline (hek-one-darken blue 0.475) (hek-one-darken bg-alt 0.15)))
  (modeline-bg-inactive     bg-alt)
  (modeline-bg-inactive-alt (hek-one-darken bg-alt 0.1))
  (modeline-box             "gray26")
  (-modeline-pad            hek-one-modeline-padding))

 ;;;;; ===== faces =====
 (;;;; Base theme face overrides
  (line-number &override :foreground base4)
  (line-number-current-line &override :foreground fg)
  (font-lock-comment-face &override :background (if hek-one-brighter-comments (hek-one-lighten bg 0.05) 'unspecified))
  (mode-line :background modeline-bg :foreground modeline-fg)
  (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt)
  (mode-line-emphasis :foreground (if hek-one-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
  (css-proprietary-property :foreground orange)
  (css-property             :foreground green)
  (css-selector             :foreground blue)
   ;;;; ivy
  (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
  (font-latex-math-face :foreground green)
   ;;;; markdown-mode
  (markdown-markup-face :foreground base5)
  (markdown-header-face :inherit 'bold :foreground red)
  (markdown-code-face &override :background (hek-one-lighten base3 0.05))
   ;;;; solaire-mode
  (solaire-mode-line-face
   :inherit 'mode-line
   :background modeline-bg-alt
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
  (solaire-mode-line-inactive-face
   :inherit 'mode-line-inactive
   :background modeline-bg-inactive-alt
   :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

 ;;;;; ===== vars =====
 ()
 )
