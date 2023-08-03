;;; hek-subr.el --- hek's subroutines -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'pixel-scroll)

(defun hek-center-line (&optional line-number)
  "Center the line by adding line prefix property."
  (save-excursion
    (when line-number
      (goto-char (point-min))
      (forward-line (1- line-number)))
    (let* ((beg-pos (line-beginning-position))
           (end-pos (line-end-position))
           (half-width (/ (car (window-text-pixel-size nil beg-pos end-pos))
                          (frame-char-width)
                          2))
           (padding (propertize " "
                                'display
                                `(space :align-to (- center ,half-width)))))
      (add-text-properties beg-pos end-pos
                           `(line-prefix ,padding indent-prefix ,padding)))))

(defun hek-scroll-up (&optional lines)
  "Scroll upwards by LINES lines or one page."
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-up)))

(defun hek-scroll-down (&optional lines)
  "Scroll downwards by LINES lines or one page."
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-down))

(provide 'hek-subr)
;;; hek-subr.el ends here
