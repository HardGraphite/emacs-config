;; hek-flymake-verilator.el --- A Verilog Flymake backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Provide a flymake back-end for Verilog HDL/SystemVerilog linting using verilator.

;;; Code:

(require 'flymake)
(require 'project)

(defvar hek-flymake-verilator-path "verilator"
  "Path to verilator executable.")

(defun hek-flymake-verilator-setup ()
  "Setup the Verilog Flymake backend for current buffer."
  (unless (executable-find hek-flymake-verilator-path)
    (error (concat "linter not found:" hek-flymake-verilator-path)))
  (hek-flymake-verilator--make-cmd)
  (setq-local flymake-no-changes-timeout nil
              flymake-start-on-save-buffer t
              flymake-diagnostic-functions '(hek-flymake-verilator--impl)
              flymake-proc-allowed-file-name-masks nil))

(defvar-local hek-flymake-verilator--cmd nil)
(defvar-local hek-flymake-verilator--proc nil)

(defun hek-flymake-verilator--make-cmd ()
  (unless buffer-file-name
    (error "Not buffer file name."))
  (let ((cmd (list
              "--lint-only" "-Wall" "--quiet-exit"
              "--timing"
              buffer-file-name))
        (proj-root (when-let ((proj (project-current)))
                     (project-root proj))))
    (when proj-root
      (let ((file-dir (file-name-directory buffer-file-name)))
        (unless (file-equal-p proj-root file-dir)
          (push (concat "-I" (directory-file-name proj-root)) cmd))))
    (push hek-flymake-verilator-path cmd)
    (setq-local hek-flymake-verilator--cmd cmd)))

(defun hek-flymake-verilator--impl (report-fn &rest _args)
  (when (process-live-p hek-flymake-verilator--proc)
    (kill-process hek-flymake-verilator--proc))
  (let ((source (current-buffer)))
    (setq hek-flymake-verilator--proc
          (make-process
           :name "flymake/verilator"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer "*flymake-verilator*")
           :command (hek-flymake-verilator--make-cmd)
           :sentinel
           (lambda (proc _ev)
             (hek-flymake-verilator--impl-sentinel
              proc report-fn source buffer-file-name))))))

(defun hek-flymake-verilator--impl-diag-other-file
    (fname line col type msg source-buf)
  ;; (flymake-make-diagnostic fname (cons line col) nil type msg)
  (flymake-make-diagnostic
   source-buf 1 2 type
   (concat (file-name-nondirectory fname) (format ":%d:%d: " line col) msg)))

(defun hek-flymake-verilator--impl-sentinel
    (proc report-fn source-buf source-fname)
  (when (memq (process-status proc) '(exit signal))
    (setq source-fname (file-name-nondirectory source-fname))
    (unwind-protect
        (if (with-current-buffer source-buf
              (eq proc hek-flymake-verilator--proc))
            (with-current-buffer (process-buffer proc)
              (goto-char (point-min))
              (cl-loop
               while (search-forward-regexp
                      "^%\\(.+\\): \\(.+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                      ;;  1:TYPE    2:FILE     3:LINE       4:COL       5:MSG
                      nil t)
               for type = (if (string-prefix-p "Error" (match-string 1))
                              :error :warning)
               for fname = (match-string 2)
               for line = (string-to-number (match-string 3))
               for col = (string-to-number (match-string 4))
               for msg = (match-string 5)
               collect (if (string-suffix-p source-fname fname)
                         (let ((region (flymake-diag-region source-buf line col)))
                           (flymake-make-diagnostic
                            source-buf (car region) (cdr region) type msg))
                         (hek-flymake-verilator--impl-diag-other-file
                          fname line col type msg source-buf))
                       into diags
               finally (funcall report-fn diags)))
          (flymake-log :warning "Canceling obsolete check %s" proc))
      (kill-buffer (process-buffer proc)))))

(provide 'hek-flymake-verilator)
;; hek-flymake-verilator.el ends here
