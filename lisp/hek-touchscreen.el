;;; hek-touchscreen.el --- Touch screen tools -*- lexical-binding: t; -*-

;;; Code:

(require 'tool-bar)

(defun hek-touchscreen-toggle-disp-kbd ()
  (interactive)
  (setq touch-screen-display-keyboard
        (not touch-screen-display-keyboard)))

(defun hek-touchscreen-C-g (&rest _args)
  (interactive)
  (execute-kbd-macro (kbd "C-g")))

(defun hek-touchscreen-esc (&rest _args)
  (interactive)
  (execute-kbd-macro (kbd "<escape>")))

(defun hek-touchscreen-tab (&rest _args)
  (interactive)
  (execute-kbd-macro (kbd "<tab>")))

(defvar hek-touchscreen-bar--items
  `(keymap
    ( control menu-item "Control Key"
      event-apply-control-modifier
      :help "Add Control modifier to the following event"
      :image ,(tool-bar--image-expression "ctrl")
      :enable (modifier-bar-available-p 'control))
    ( shift menu-item "Shift Key"
      event-apply-shift-modifier
      :help "Add Shift modifier to the following event"
      :image ,(tool-bar--image-expression "shift")
      :enable (modifier-bar-available-p 'shift))
    ( meta menu-item "Meta Key"
      event-apply-meta-modifier
      :help "Add Meta modifier to the following event"
      :image ,(tool-bar--image-expression "meta")
      :enable (modifier-bar-available-p 'meta))
    ( super menu-item "Super Key"
      event-apply-super-modifier
      :help "Add Super modifier to the following event"
      :image ,(tool-bar--image-expression "super")
      :enable (modifier-bar-available-p 'super))
    ))

(defvar hek-touchscreen-bar--bindings
  '((control . tool-bar-event-apply-control-modifier)
    (shift   . tool-bar-event-apply-shift-modifier)
    (meta    . tool-bar-event-apply-meta-modifier)
    (super   . tool-bar-event-apply-super-modifier)))

(defun hek-touchscreen-bar-add (sym name func &rest create-image-args)
  (nconc hek-touchscreen-bar--items
         (list (list sym 'menu-item name #'ignore
                     :image (apply #'create-image create-image-args))))
  (nconc hek-touchscreen-bar--bindings
         (list (cons sym func))))

(define-minor-mode hek-touchscreen-bar-mode
  "A tool bar that provides keys for touch screen.

Conflicts with `modifier-bar-mode'."
  :init-value nil
  :global t
  :group 'tool-bar
  (if (not hek-touchscreen-bar-mode)
      (setq secondary-tool-bar-map nil)
    (when modifier-bar-mode
      (modifier-bar-mode -1))
    (setq secondary-tool-bar-map hek-touchscreen-bar--items)
    (dolist (x hek-touchscreen-bar--bindings)
      (define-key input-decode-map (vector 'tool-bar (car x)) (cdr x)))))

(provide 'hek-touchscreen)
;;; hek-touchscreen.el ends here
