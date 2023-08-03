;; --- Programming language supports -*- lexical-binding: t; no-byte-compile: t -*-

;;;;; LSP (language server protocol) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Eglot
(hek-usepkg eglot
  :from builtin
  :init
  ;; eglot options
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  ;; LSP servers
  (setq eglot-server-programs
        '(((c-mode c++-mode c-ts-mode c++-ts-mode) "clangd")
          ((cmake-mode cmake-ts-mode) "cmake-language-server")
          (python-base-mode "pyright-langserver" "--stdio")
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

;;;;; Tree-sitter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tree-sitter
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
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist))
  :init
  (setq major-mode-remap-alist
        '((js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode)))
  )

;;;;; Specific languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --- C-like languages ---

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

;;; --- CMake ---

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
  (when (ignore-errors (hek-flymake-verilator-setup) t)
    (flymake-mode 1)))
