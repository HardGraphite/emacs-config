;;; hek-exim.el --- external input method controlling -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides tools to query and control external input methods states.

;;; Code:

(defgroup hek-exim nil
  "External input method controlling."
  :group 'text
  :group 'external
  :prefix "hek-exim-")

(defvar hek-exim-sources
  nil
  "Alist of IM sources.

Each element in the list shall has the form:

  (NAME . STATE)

where STATE is a internal state value used by `hek-exim-get-source-function' and
`hek-exim-set-source-function'; NAME is a symbol used by other functions.")

(defvar hek-exim-get-source-function
  nil
  "Function to query current IM source.

It shall take no argument and return current IM source. Consider function
`w32-get-ime-open-status' on MS Windows, `mac-input-source' on Darwin (macOS),
`hek-exim-fcitx5-dbus-get-state' for Fcitx 5.")

(defvar hek-exim-set-source-function
  nil
  "Function to change IM source.

It shall take an argument SOURCE and switch to the source. Consider function
`w32-set-ime-open-status' on MS Windows, `mac-select-input-source' on Darwin
(macOS), `hek-exim-fcitx5-dbus-set-state' for Fcitx 5.")

(defvar-local hek-exim--buffer-source nil) ;; Cached buffer-local IM source.

(defun hek-exim-query ()
  "Get current input method source."
  (let ((state (funcall hek-exim-get-source-function)))
    (setq hek-exim--buffer-source
          (car (or (rassoc state hek-exim-sources)
                   (error "unknown IM state `%S'" state))))))

(defun hek-exim-switch (source)
  "Switch to input method SOURCE."
  (funcall hek-exim-set-source-function
           (cdr (or (assq source hek-exim-sources)
                    (error "unknown IM source `%S'" source))))
  (setq hek-exim--buffer-source source))

(defun hek-exim-toggle ()
  "Toggle input method."
  (interactive)
  (when (cdr hek-exim-sources) ;; Has at least two sources.
    (hek-exim-switch
     (if (eq hek-exim--buffer-source (caadr hek-exim-sources))
         (caar hek-exim-sources)
       (caadr hek-exim-sources)))))

(when (eq system-type 'gnu/linux)

  ;; Fcitx 5 via D-Bus.
  ;;
  ;; States are: 0 = down, 1 = inactive, 2 = active.
  ;; Require `dbus' before using the following functions.
  ;;
  ;; See: https://github.com/fcitx/fcitx5/blob/master/src/tools/remote.cpp

  (declare-function dbus-call-method "dbus")

  (defun hek-exim-fcitx5-dbus-get-state ()
    (dbus-call-method
     :session "org.fcitx.Fcitx5"
     "/controller" "org.fcitx.Fcitx.Controller1"
     "State"))

  (defun hek-exim-fcitx5-dbus-set-state (state)
    (dbus-call-method
     :session "org.fcitx.Fcitx5"
     "/controller" "org.fcitx.Fcitx.Controller1"
     (if (= state 2) "Activate" "Deactivate")))

  )

(provide 'hek-exim)
;;; hek-exim.el ends here
