;;; hek-exim.el --- external input method controlling -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides tools to query and control external input methods states.

;;; Code:

(defgroup hek-exim nil
  "External input method controlling."
  :group 'text
  :group 'external
  :prefix "hek-exim-")

;;;
;;;;; Basic input method control
;;;

(defvar hek-exim-verbose nil
  "Print messages when switching and querying IM source.")

(defvar hek-exim-get-source-function nil
  "Function to query current IM source.

It shall take no argument and return nil for disable or a non-nil value
representing IM enabled.
Consider function `w32-get-ime-open-status' on MS Windows, `mac-input-source'
on Darwin (macOS), `hek-exim-fcitx5-dbus-get-state' for Fcitx 5.")

(defvar hek-exim-set-source-function
  nil
  "Function to change IM source.

It shall take an argument STATE (nil or non-nil) and switch to the state.
Consider function `w32-set-ime-open-status' on MS Windows,
`mac-select-input-source' on Darwin (macOS), `hek-exim-fcitx5-dbus-set-state'
for Fcitx 5.")

(defvar-local hek-exim--buffer-source nil) ;; Cached buffer-local IM source state.
(defvar hek-exim--current-source nil) ;; Cached global IM source state.

(defun hek-exim-query ()
  "Get current input method source."
  (let ((state (funcall hek-exim-get-source-function)))
    (when hek-exim-verbose
      (message "input method is %s" (if state "on" "off")))
    (setq hek-exim--buffer-source state
          hek-exim--current-source state)))

(defun hek-exim-switch (state)
  "Switch input method to STATE."
  (when hek-exim-verbose
    (message "switch %s input method" (if state "on" "off")))
  (funcall hek-exim-set-source-function state)
  (setq hek-exim--buffer-source state
        hek-exim--current-source state))

;;;###autoload
(defun hek-exim-toggle ()
  "Toggle input method."
  (interactive)
  (hek-exim-switch (not (hek-exim-query))))

;;;
;;;;; Automatic switching
;;;

(defvar hek-exim-automodal-hooks
  nil
  "Pair of hooks for modal editing. Nil to disable.

  (ENTER-INSERT-STATE-HOOK . LEAVE-INSERT-STATE-HOOK)")

(defvar-local hek-exim--automodal-insert-source nil) ;; Source before leaving insert mode.

(defun hek-exim--automodal-enter-insert ()
  (unless (eq hek-exim--buffer-source hek-exim--automodal-insert-source)
    (hek-exim-switch hek-exim--automodal-insert-source)))

(defun hek-exim--automodal-leave-insert ()
  (when (setq hek-exim--automodal-insert-source hek-exim--buffer-source)
    (hek-exim-switch nil)))

(defun hek-exim--automodal-pre-command ()
  (unless (eq hek-exim--buffer-source hek-exim--current-source)
    ;; Current source is not the expected buffer local source.
    ;; Rare if use modal editing. Probably switching window/buffer with a mouse?
    (hek-exim-switch hek-exim--buffer-source)))

;;;###autoload
(define-minor-mode hek-exim-automodal-mode
  "Global minor mode that automatically switch input methods."
  :global t
  (if hek-exim-automodal-mode
      (progn
        ;; (setq-default hek-exim--buffer-source nil
        ;;               hek-exim--automodal-insert-source nil)
        (add-hook 'pre-command-hook #'hek-exim--automodal-pre-command)
        (when hek-exim-automodal-hooks
          (add-hook (car hek-exim-automodal-hooks) #'hek-exim--automodal-enter-insert)
          (add-hook (cdr hek-exim-automodal-hooks) #'hek-exim--automodal-leave-insert)))
    (remove-hook 'pre-command-hook #'hek-exim--automodal-pre-command)
    (when hek-exim-automodal-hooks
      (remove-hook (car hek-exim-automodal-hooks) #'hek-exim--automodal-enter-insert)
      (remove-hook (cdr hek-exim-automodal-hooks) #'hek-exim--automodal-leave-insert))))

;;;
;;;;; Inline text
;;;

(defvar hek-exim-inlinetext-triggers '(?\  ?\\)
  "List of characters that start inline text regions.")

(defface hek-exim-inlinetext
  '((t :inherit region :underline t))
  "Inline text region face.")

(defvar-local hek-exim--inlinetext-overlay nil) ;; Overlay or nil

(defun hek-exim-inlinetext-commit ()
  "Try to commit and delete an inline text region. Return `t' if success."
  (interactive)
  (when hek-exim--inlinetext-overlay
    (hek-exim-switch t)
    (delete-overlay hek-exim--inlinetext-overlay)
    (setq hek-exim--inlinetext-overlay nil)
    t))

(defvar hek-exim--inlinetext-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'hek-exim-inlinetext-commit)
    keymap))

(defun hek-exim-inlinetext-create (&optional beginning end)
  "Create (start) an inline text region."
  (interactive)
  (unless (and beginning end)
    (setq beginning (point)
          end beginning))
  (hek-exim-switch nil)
  (setq hek-exim--inlinetext-overlay (make-overlay beginning end nil t t))
  (overlay-put hek-exim--inlinetext-overlay 'face 'hek-exim-inlinetext)
  (overlay-put hek-exim--inlinetext-overlay 'keymap hek-exim--inlinetext-map))

(defun hek-exim--inlinetext-post-self-insert ()
  (let ((p (point)))
    ;; Inactivate the region if point is out of the region.
    (when (and hek-exim--inlinetext-overlay
               (let ((p0 (overlay-start hek-exim--inlinetext-overlay))
                     (p1 (overlay-end hek-exim--inlinetext-overlay)))
                 (or (< p p0) (< p p1) (= p0 p1))))
      (hek-exim-inlinetext-commit))
    ;; Activate region if triggered.
    (when (and (not hek-exim--inlinetext-overlay)
               hek-exim--buffer-source ;; IM enabled
               (memq last-command-event hek-exim-inlinetext-triggers))
      (hek-exim-inlinetext-create (1- p) p))))

;;;###autoload
(define-minor-mode hek-exim-inlinetext-mode
  "Global minor mode to type key sequences with IM disabled."
  :global t
  (if hek-exim-inlinetext-mode
      (add-hook 'post-self-insert-hook #'hek-exim--inlinetext-post-self-insert)
    (remove-hook 'post-self-insert-hook #'hek-exim--inlinetext-post-self-insert)
    (hek-exim-inlinetext-commit)))

;;;
;;;;; Utilities
;;;

(when (eval-when-compile (eq system-type 'gnu/linux))

  ;; Fcitx 5 via D-Bus.
  ;;
  ;; States are: 0 = down, 1 = inactive, 2 = active.
  ;; Require `dbus' before using the following functions.
  ;;
  ;; See: https://github.com/fcitx/fcitx5/blob/master/src/tools/remote.cpp

  (declare-function dbus-call-method "dbus")

  (defun hek-exim-fcitx5-dbus-get-state ()
    (= (dbus-call-method
        :session "org.fcitx.Fcitx5"
        "/controller" "org.fcitx.Fcitx.Controller1"
        "State")
       2))

  (defun hek-exim-fcitx5-dbus-set-state (state)
    (dbus-call-method
     :session "org.fcitx.Fcitx5"
     "/controller" "org.fcitx.Fcitx.Controller1"
     (if state "Activate" "Deactivate")))

  )

(provide 'hek-exim)
;;; hek-exim.el ends here
