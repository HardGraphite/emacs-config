;;; hek-ligature.el --- Enable font ligatures. -*- lexical-binding: t -*-

;;; Commentary:

;; A minor mode to support font ligatures.

;;; Code:

(defgroup hek-ligature nil
  "Font ligatures."
  :group 'display
  :prefix "hek-ligature-")

(defvar hek-ligature-table nil
  "A list of ligature definitions for a mode. Each definition is like:

     (MODE (FIRST-CHAR . REGEXP) (FIRST-CHAR . REGEXP) ...)

   where MODE is a mode; FIRST-CHAR is the first char of a ligature sequence;
   REGEXP is a regular expression string representing the sequence. Only the
   first matched case will be applied.")

(defvar hek-ligature-cft--cache nil
  "A list of composition-function-table cache. Each element is like:

    (MODE . TABLE)")

(defun hek-ligature--make-cft ()
  "Generate composition-function-table based on current major mode."
  (let ((cft (default-value 'composition-function-table))
        (made nil))
    (dolist (lig-def hek-ligature-table)
      (when (and (not made) (derived-mode-p (car lig-def)))
        (setq made t)
        (dolist (rule (cdr lig-def))
          (set-char-table-range
            cft (car rule) `([,(cdr rule) 0 font-shape-gstring])))))
    (if made cft nil)))

(defun hek-ligature--find-cft ()
  "Find a cached composition-function-table based on current major mode.
   If it does not exist, create one. If it is not defined, return nil."
  (let ((lig-mode nil))
    (dolist (lig-def hek-ligature-table)
      (when (and (not lig-mode) (derived-mode-p (car lig-def)))
        (setq lig-mode (car lig-def))))
    (when lig-mode
      (let ((cache-pair (assoc lig-mode hek-ligature-cft--cache)))
        (if cache-pair
          (cdr cache-pair)
          (let ((cft (hek-ligature--make-cft)))
            (push (cons lig-mode cft) hek-ligature-cft--cache)
            cft))))))

;;;###autoload
(define-minor-mode hek-ligature-mode
  "Font ligature."
  :global  nil
  :group   'hek-ligature
  (if hek-ligature-mode
    (unless (local-variable-if-set-p 'composition-function-table)
      (when-let ((cft (hek-ligature--find-cft)))
        (setq-local composition-function-table cft)))
    (when (local-variable-if-set-p 'composition-function-table)
      (kill-local-variable 'composition-function-table))))

(provide 'hek-ligature)

;;; hek-ligature.el ends here
