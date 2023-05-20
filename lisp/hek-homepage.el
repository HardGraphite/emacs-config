;;; hek-homepage.el --- A welcome page. -*- lexical-binding: t -*-

;;; Commentary:

;; Provides a start welcome page to replace a dashboard.

;;; Code:

(defvar hek-homepage-logo nil)

(defvar hek-homepage-banner nil)

(defvar hek-homepage-links
  nil
  "A list containing links to create buttons.

(((LABEL1 KEY1 . TARGET1) (LABEL2 KEY2 . TARGET2) ...) ;; Line 1
 ((LABEL3 KEY3 . TARGET3) (LABEL4 KEY4 . TARGET4) ...) ;; Line 2
 ...)

LABEL is a string; KEY is a key sequence; TARGET is a function or a command.")

(defface hek-homepage-button
  '((t :background "#383c44" :box t))
  "")

(defun hek-homepage--center-line ()
  (let* ((line-beg-pos (line-beginning-position))
         (line-end-pos (line-end-position))
         (content-half-width (/ (car (window-text-pixel-size
                                      nil line-beg-pos line-end-pos))
                                (frame-char-width)
                                2))
         (prefix (propertize " "
                             'display
                             `(space :align-to (- center ,content-half-width)))))
    (add-text-properties line-beg-pos line-end-pos
                         `(line-prefix ,prefix indent-prefix ,prefix))))

(defun hek-homepage--invoke-button-link (button)
  (let ((button-label (button-label button)))
    (when-let ((key-def (lookup-key (current-local-map)
                                    (kbd (and (string-match "\\[\\(.+\\)\\]" button-label)
                                              (match-string 1 button-label))))))
      (if (commandp key-def)
          (call-interactively key-def)
        (funcall key-def)))))

(defun hek-homepage-setup (&optional buf)
  "Setup home page in the BUF buffer if specified."
  ;; prepare buffer
  (when buf
    (switch-to-buffer buf t))
  (major-mode-suspend)
  (fundamental-mode)
  (erase-buffer)
  (when (bound-and-true-p whitespace-mode)
    (whitespace-mode -1))
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (goto-char (point-min))
  (insert "\n")
  ;; insert logo and banner
  (when-let ((img hek-homepage-logo))
    (insert-image (create-image img))
    (hek-homepage--center-line)
    (insert "\n"))
  (when-let ((txt hek-homepage-banner))
    (insert txt)
    (hek-homepage--center-line)
    (insert "\n"))
  ;; insert link buttons
  (insert "\n\n")
  (let ((keymap (make-sparse-keymap)))
    (dolist (link-line hek-homepage-links)
      (dolist (link-entry link-line)
        (let ((label (car link-entry))
              (keyseq (cadr link-entry))
              (target (cddr link-entry)))
          (define-key keymap (kbd keyseq) target)
          (insert-button (format " %s [%s] " label keyseq)
                         'face 'hek-homepage-button
                         'action #'hek-homepage--invoke-button-link
                         'follow-link t)
          (insert "  ")))
      (hek-homepage--center-line)
      (insert "\n\n"))
    (use-local-map keymap))
  ;; version info
  (insert "\n\n")
  (insert (propertize (concat "GNU Emacs "
                              emacs-version
                              (when (daemonp) " (client)"))
                      'face '(variable-pitch
                              (:slant italic :height 120))))
  (hek-homepage--center-line)
  ;; finally
  (setq buffer-read-only t
        truncate-lines t)
  (goto-char (point-min)))

(defun hek-homepage-cleanup (&optional new-major-mode)
  "Erase buffer and use a new major mode."
  (setq buffer-read-only nil
        truncate-lines nil)
  (erase-buffer)
  (use-local-map nil)
  (if new-major-mode
      (funcall new-major-mode)
    (major-mode-restore)))

(provide 'hek-homepage)

;;; hek-homepage.el ends here
