;; --- Programming language supports -*- lexical-binding: t -*-

;;;;; LSP (language server protocol) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Eglot, https://github.com/joaotavora/eglot
(use-package eglot
  ;; :ensure nil ;; built-in
  :defer t
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
  ((c-mode c++-mode
    python-mode
    lua-mode
    tex-mode bibtex-mode)
   . eglot-ensure))

;;;;; Tree-sitter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ELisp Tree-sitter, https://github.com/emacs-tree-sitter/elisp-tree-sitter
(use-package tree-sitter
  ;; :ensure nil ;; built-in
  :defer t
  :hook
  ((sh-mode
    ;; c-mode c++-mode ;; <-- clangd provides highlight info
    ;; csharp-mode
    css-mode
    ;; go-mode
    ;; haskell-mode
    html-mode
    java-mode
    javascript-mode
    ;; json-mode
    ;; julia-mode
    ;; lua-mode
    ;; php-mode
    python-mode
    rust-mode
    ;; toml-mode
    ;; yaml-mode
    verilog-mode
    markdown-mode)
   . tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)
;; See `https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries'
;; for query definitions from Nvim's tree sitter plugin.

;;;;; Specific languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Markdown ---
;; Markdown mode, https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :defer t
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5" "--standalone" "--mathjax")))

;; --- Verilog HDL / SystemVerilog ---
(use-package verilog-mode
  :ensure nil ;; built-in
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
        verilog-indent-level-module 0
        verilog-indent-lists nil
        verilog-case-indent 4
        verilog-cexp-indent 4
        verilog-indent-begin-after-if nil
        )
  ;; (setq verilog-linter "verilator -–lint-only -Wall")
  (require 'hek-flymake-verilator)
  (defun +verilog-mode-setup ()
    (hek-flymake-verilator-setup)
    (flymake-mode 1))
  :hook
  ((verilog-mode . +verilog-mode-setup)))
