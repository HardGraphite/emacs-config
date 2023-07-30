;;; hek-subr.el --- hek's subroutines -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

(provide 'hek-subr)
;;; hek-subr.el ends here
