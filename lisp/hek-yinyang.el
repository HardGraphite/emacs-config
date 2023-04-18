;;; hek-yinyang.el --- Auto dark/light theme switcher -*- lexical-binding: t; -*-

;;; Commentary:

;; A minor mode is provided to switch between dark theme and light theme
;; automatically according to local time.

;;; Code:

(defgroup hek-yinyang nil
  "Auto dark/light theme switcher."
  :prefix "hek-yinyang-")

(defcustom hek-yinyang-dark-theme
  nil
  "Dark theme."
  :type 'symbol)

(defcustom hek-yinyang-light-theme
  nil
  "Light theme."
  :type 'symbol)

(defcustom hek-yinyang-sunset
  '(17 . 00)
  "Time of sunset, after which dark theme will be used."
  :type '(cons natnum natnum))

(defcustom hek-yinyang-sunrise
  '(07 . 00)
  "Time of sunset, after which light theme will be used."
  :type '(cons natnum natnum))

(defvar hek-yinyang-update-hook
  nil
  "Hooks that will be called after theme reloaded.")

(defvar hek-yinyang--state nil) ;; t for light, nil for dark.
(defvar hek-yinyang--timer nil)

(defun hek-yinyang--time-of-day (&optional hr-and-min-pair)
  "Represent time of a day with an integer (min + hr * 60)."
  (if hr-and-min-pair
    (+ (cdr hr-and-min-pair)
       (* (car hr-and-min-pair) 60))
    (let ((time (decode-time)))
      (+ (cadr time)
         (* (caddr time) 60)))))

(defun hek-yinyang--reload-theme ()
  (load-theme
    (if hek-yinyang--state hek-yinyang-light-theme hek-yinyang-dark-theme)
    t)
  (run-hooks 'hek-yinyang-update-hook))

(defun hek-yinyang-update (force-reload)
  "Switch to the expected theme according to current time.
  Return time (in sec) to next expected update."
  (let ((current-time (hek-yinyang--time-of-day))
        (sunset-time  (hek-yinyang--time-of-day hek-yinyang-sunset))
        (sunrise-time (hek-yinyang--time-of-day hek-yinyang-sunrise)))
    (unless (eq hek-yinyang--state
                (and (< sunrise-time current-time)
                     (<= current-time sunset-time)))
      (setq hek-yinyang--state (not hek-yinyang--state))
      (setq force-reload t))
    (when force-reload
      (hek-yinyang--reload-theme))
    (let ((next-update
            (if hek-yinyang--state
              (- sunset-time current-time)
              (- sunrise-time current-time))))
      (if (> next-update 0)
        next-update
        (+ next-update (hek-yinyang--time-of-day '(24 . 00)))))))

(defun hek-yinyang-toggle ()
  "Toggle between dark and light themes."
  (interactive)
  (setq hek-yinyang--state (not hek-yinyang--state))
  (hek-yinyang--reload-theme))

(defun hek-yinyang--auto-update ()
  (setq hek-yinyang--timer
    (run-with-timer
      (+ (hek-yinyang-update t) 5) nil
      #'hek-yinyang--auto-update)))

(define-minor-mode hek-yinyang-mode
  "Auto dark/light theme switcher."
  :global t
  :group  'hek-yinyang
  (if hek-yinyang-mode
    (unless hek-yinyang--timer
      (hek-yinyang--auto-update))
    (when hek-yinyang--timer
      (cancel-timer hek-yinyang--timer)
      (setq hek-yinyang--timer nil))))

(provide 'hek-yinyang)

;;; hek-yinyang.el ends here
