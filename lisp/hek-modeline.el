;;; hek-modeline.el --- A lightweight mode line. -*- lexical-binding: t -*-

;;; Commentary:

;; A lightweight Emacs mode line.

;;; Code:

(defvar hek-modeline-left
  '((meow-mode (:eval (meow-indicator)))
    " %* %b "
    (-3 "%p")
    (9 " %l:%C")
    )
  "Left half of mode line.")

(defvar hek-modeline-right
  '("%m"
    )
  "Right half of mode line.")

(defconst hek-modeline-format
  '((:eval (hek-modeline--draw)))
  "An implementation of `mode-line-format'.")

(defun hek-modeline-setup (&optional global)
  "Setup mode line."
  (if global
      (setq-default mode-line-format hek-modeline-format)
      (setq mode-line-format hek-modeline-format)))

(defun hek-modeline--draw ()
  (let ((right-str  (format-mode-line hek-modeline-right)))
    (list "%e"
          hek-modeline-left
          `(:propertize " " display (space :align-to (- right ,(length right-str))))
          right-str)))

(provide 'hek-modeline)

;;; hek-modeline.el ends here
