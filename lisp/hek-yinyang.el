;;; hek-yinyang.el --- Auto dark/light theme switcher -*- lexical-binding: t; -*-

;;; Commentary:

;; A minor mode is provided to switch between dark theme and light theme
;; automatically according to local time.

;;; Code:

(defgroup hek-yinyang nil
  "Auto dark/light theme switcher."
  :group 'faces
  :prefix "hek-yinyang-")

(defcustom hek-yinyang-dark-theme
  nil
  "Dark theme."
  :type 'symbol)

(defcustom hek-yinyang-light-theme
  nil
  "Light theme."
  :type 'symbol)

(defcustom hek-yinyang-sunrise
  '(06 . 00)
  "Time of sunset, after which light theme will be used."
  :type '(cons natnum natnum))

(defcustom hek-yinyang-sunset
  '(18 . 00)
  "Time of sunset, after which dark theme will be used."
  :type '(cons natnum natnum))

(defvar hek-yinyang-switch-hook
  nil
  "Hook run after theme switched.")

(defvar hek-yinyang--state nil) ;; t for light, nil for dark.
(defvar hek-yinyang--timer nil) ;; (timer1 . timer2)

(defun hek-yinyang-ensure-theme ()
  "Make sure the current theme respects `hek-yinyang--state'."
  (let ((expected-theme (if hek-yinyang--state
                            hek-yinyang-light-theme hek-yinyang-dark-theme))
        (another-theme  (if hek-yinyang--state
                            hek-yinyang-dark-theme hek-yinyang-light-theme)))
    (unless (custom-theme-p expected-theme)
      (load-theme expected-theme t t))
    (disable-theme another-theme) ;; `disable-theme' will check whether the theme is enabled
    (unless (eq expected-theme (car custom-enabled-themes))
      (enable-theme expected-theme)
      (run-hooks 'hek-yinyang-switch-hook)
      (message "Use theme `%s'" expected-theme))))

(defun hek-yinyang-switch-to (light/dark)
  "Switch to light (`t') or dark (`nil') theme."
  (setq hek-yinyang--state light/dark)
  (hek-yinyang-ensure-theme))

(defun hek-yinyang-toggle ()
  "Toggle between dark and light themes."
  (interactive)
  (hek-yinyang-switch-to (not hek-yinyang--state)))

(define-minor-mode hek-yinyang-mode
  "Auto dark/light theme switcher."
  :global t
  :group  'hek-yinyang
  (if hek-yinyang-mode
    (unless hek-yinyang--timer
      (let ((current-time (let ((time (decode-time)))
                            (+ (decoded-time-second time)
                               (* 60 (decoded-time-minute time))
                               (* 60 60 (decoded-time-hour time)))))
            (sunrise-time (+ (* 60 (cdr hek-yinyang-sunrise))
                             (* 60 60 (car hek-yinyang-sunrise))))
            (sunset-time  (+ (* 60 (cdr hek-yinyang-sunset))
                             (* 60 60 (car hek-yinyang-sunset)))))
        (let ((sunrise-time-dur (- sunrise-time current-time))
              (sunset-time-dur  (- sunset-time  current-time))
              (day-time-dur     (* 24 60 60)))
          (when (< sunrise-time-dur 0)
            (setq sunrise-time-dur (+ sunrise-time-dur day-time-dur)))
          (when (< sunset-time-dur 0)
            (setq sunset-time-dur (+ sunset-time-dur day-time-dur)))
          (setq hek-yinyang--timer
                (cons (run-at-time sunrise-time-dur day-time-dur #'hek-yinyang-switch-to t)
                      (run-at-time sunset-time-dur  day-time-dur #'hek-yinyang-switch-to nil)))))
      (hek-yinyang-ensure-theme))
    (when hek-yinyang--timer
      (cancel-timer (car hek-yinyang--timer))
      (cancel-timer (cdr hek-yinyang--timer))
      (setq hek-yinyang--timer nil))))

(provide 'hek-yinyang)
;;; hek-yinyang.el ends here
