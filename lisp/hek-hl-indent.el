;;; hek-hl-indent.el --- indentation visualization -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a minor mode that highlights indentations.

;;; Code:

(defgroup hek-hl-indent nil
  "Indentation highlighting."
  :group 'editing
  :prefix "hek-hl-indent-")

(defvar-local hek-hl-indent-width 4
  "Indentation width.")

(defface hek-hl-indent-indicator
  `((t ( :inherit fill-column-indicator
         :stipple (4 6 ,(string 1 0 0 4 0 0)) )))
  "Indentation highlighting.")

(defun hek-hl-indent-redraw (start end)
  "Clear and re-draw indentation highlights in the region."
  (save-excursion
    (with-silent-modifications
      (remove-text-properties start end
                              '(font-lock-face nil rear-nonsticky nil))
      (goto-char start)
      (let ((c (current-column)))
        (when (> c 0)
          (goto-char (- (point) c))))
      (while (< (point) end)
        (let* ((pt (point))
               (pt-end (+ pt (current-indentation))))
          (while (< pt pt-end)
            (add-text-properties
             pt (1+ pt)
             '(font-lock-face hek-hl-indent-indicator
                              rear-nonsticky t))
            (setq pt (+ pt hek-hl-indent-width))))
        (forward-line)))))

(defun hek-hl-indent-clear (start end)
  "Delete indentation highlights in the region."
  (remove-text-properties start end
                          '(font-lock-face nil rear-nonsticky nil)))

;;;###autoload
(define-minor-mode hek-hl-indent-mode
  "Visualize indentation in the current buffer."
  :global nil
  (if hek-hl-indent-mode
      (jit-lock-register #'hek-hl-indent-redraw)
    (jit-lock-unregister #'hek-hl-indent-redraw)
    (hek-hl-indent-clear (point-min) (point-max))))

(provide 'hek-hl-indent)
;;; hek-hl-indent.el ends here
