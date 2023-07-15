;; --- Programming language supports -*- lexical-binding: t -*-

;;;;; LSP (language server protocol) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Eglot, https://github.com/joaotavora/eglot
(hek-usepkg eglot
  :from package ;; FIXME: builtin after Emacs 29
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  :config
  (setq eglot-server-programs
    '(((c-mode c++-mode) "clangd")
      (python-mode "pyright-langserver" "--stdio")
      (lua-mode "lua-language-server")
      ((tex-mode bibtex-mode) "texlab")))
  :hook
  ((c-mode-hook c++-mode-hook
    python-mode-hook
    lua-mode-hook
    tex-mode-hook bibtex-mode-hook)
   . eglot-ensure))

;;;;; Tree-sitter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ELisp Tree-sitter, https://github.com/emacs-tree-sitter/elisp-tree-sitter
(hek-usepkg tree-sitter
  :from package
  :hook
  ((sh-mode-hook
    ;; c-mode-hook c++-mode-hook ;; <-- clangd provides highlight info
    ;; csharp-mode-hook
    css-mode-hook
    ;; go-mode-hook
    ;; haskell-mode-hook
    html-mode-hook
    java-mode-hook
    javascript-mode-hook
    ;; json-mode-hook
    ;; julia-mode-hook
    ;; lua-mode-hook
    ;; php-mode-hook
    python-mode-hook
    rust-mode-hook
    ;; toml-mode-hook
    ;; yaml-mode-hook
    verilog-mode-hook
    ;;markdown-mode-hook
    )
   . tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))
(hek-usepkg tree-sitter-langs
  :from package
  :after tree-sitter)
;; See `https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries'
;; for query definitions from Nvim's tree sitter plugin.

;;;;; Specific languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --- Markdown ---

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
        '("pandoc" "--from=markdown" "--to=html5" "--standalone" "--mathjax"
          "--metadata" "title=PREVIEW")
        markdown-fontify-code-blocks-natively t
        markdown-code-lang-modes
        '(("c" . c-mode)
          ("c++" . c++-mode)
          ("cpp" . c++-mode)
          ("python" . python-mode)
          ("sh" . sh-mode)
          ("shell" . sh-mode))))

;;; --- Verilog HDL / SystemVerilog ---

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
  (hek-flymake-verilator-setup)
  (flymake-mode 1))
