;;; headerlinetest.el --- Some tests for the headder line

;; Author: nounch <nounch@nounch.nounch>
;; Keywords: tools


;;; Commentary:

;; ---

;;; Code:


(defvar ntfy-lock nil "Lock for any kind of notification.")


(defun ntfy-set-test-header ()
  "Defines a test header. Invoking it in a buffer adds the header line to
this buffer."
  (defconst test-header "Test Header")
  (setq header-line-format
        '(:eval (substring test-header (min (length test-header)
                                            (window-hscroll))))))

(defun ntfy-notification-header (message)
  ""
  (setq header-line-format message))

(defun ntfy-toggle-header (header-setter &rest args)
  "Takes a a HEADER-SETTER function which adds a header line to a buffer
and toggles it for the current buffer"
  (if (eq header-line-format nil)
      (funcall header-setter args)
      (setq header-line-format nil)))


(defun ntfy-fill-string-to-window-width (string)
  ""
  (let ((string-length (length string)))
    (concat string (make-string (* 2 (- (window-width) string-length))
                                ? ))))

(ntfy-fill-string-to-window-width "foobar")

(defun notify-show-notification (message &optional seconds)
  "Shows a notification with MESSAGE in the header line for and dismisses
it after SECONDS. If no SECONDS parameter is specified, the notification
will be dismissed after 3 seconds."
  (interactive "MMessage: ")
  (lexical-let ((old-format header-line-format))  ; XXX
    (setq header-line-format message)
    (redraw-display)
    (run-with-timer 1 nil
                    (lambda ()
                      (setq header-line-format old-format)
                      (redraw-display)))))

(defun ntfy-generate-colors (&optional type)
  (let ((fg-color "#000000")
        (bg-color "#ffffff"))
    (if (equal type "warning")
        (progn
          (setq fg-color "#ffffff")
          (setq bg-color "#ff0000"))
        (if (equal type "ok")
            (setq bg-color "#00ff00"))
        (if (equal type "info")
            (setq bg-color "#ffff00")))
    (cons fg-color bg-color)))

(defun notify-notify (type message &optional seconds)
  "Insert MESSAGE of TYPE as notification at the top of the current window
\(as the header line\) for SECONDS time. SECONDS defaults to 3.

Available types:
\"warning\" red background, white foreground
\"ok\": green background, black foreground
\"info\": yellow background, black foreground"
  (interactive "MMessage: ")
  (lexical-let* ((colors (ntfy-generate-colors type))
                 (fg-color (car colors))
                 (bg-color (cdr colors)))
    (notify-show-notification (propertize (ntfy-fill-string-to-window-width
                                           message) 'face
                                           (list
                                            :foreground
                                            bg-color
                                            :background
                                            fg-color)) seconds)))

;; FIXME: Prevent scenarios like this one:
;;   1. Display `warning' notification
;;   2. (While still displaying warning) Display `ok' notification
;;   3. Restore old state: now 1. instead of real original state
(defun notify-notify-all-windows (type message &optional seconds)
  "Displays MESSAGE as notification \(as header line\) for SECONDS in each
window in the current frame."
  (lexical-let* ((old-format header-line-format)
                 (color (ntfy-generate-colors type))
                 (bg-color (car color))
                 (fg-color (cdr color)))
    (dolist (window (window-list))
      (with-selected-window window
        (setq header-line-format (propertize
                                  (ntfy-fill-string-to-window-width
                                   message) 'face
                                   (list
                                    :foreground
                                    fg-color
                                    :background
                                    bg-color)))))
    (run-with-timer 3 nil (lambda () (dolist (window (window-list))
                                       (with-selected-window window
                                         (setq header-line-format
                                               old-format)))))))


(notify-notify-all-windows "warning" "foobar")


;; Conclusion
;; Clearly `foo' is not handled correctly.

;; Is combining `let' with `run-with-timer' a bad idea?
;; Or is the `lambda'/`progn' combination messing things up?



;; (setq header-line-format nil)

;; (propertize "foobar" 'face '(:foreground "red" :background "green"))


;; Tests

;; (ntfy-toggle-header 'ntfy-set-test-header)
;; (ntfy-toggle-header 'ntfy-notification-header message)

;; (notify-show-notification "foo")
(setq header-line-format "foobar")
(notify-notify "warning" "Warning")
;; (notify-notify "ok" "OK")
;; (notify-notify "info" "Info")

(dolist (window (window-list))
  (with-selected-window window
    (notify-notify "warning" "Warning")))


(provide 'headerlinetest)
;;; headerlinetest.el ends here
