;;; headerlinetest.el --- Some tests for the headder line

;; Copyright (C) 2013  nounch

;; Author: nounch <nounch@nounch.nounch>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ---

;;; Code:



(defun set-test-header ()
  "Defines a test header. Invoking it in a buffer adds the header line to
this buffer."
  (defconst test-header "Test Header")
  (setq header-line-format
        '(:eval (substring test-header (min (length test-header)
                                            (window-hscroll))))))

(defun notification-header (message)
  ""
  (setq header-line-format message))

(defun toggle-header (header-setter &rest args)
  "Takes a a HEADER-SETTER function which adds a header line to a buffer
and toggles it for the current buffer"
  (if (eq header-line-format nil)
      (funcall header-setter args)
      (setq header-line-format nil)))


(defun show-notification (message &optional seconds)
  "Shows a notification with MESSAGE in the header line for and dismisses
it after SECONDS. If no SECONDS parameter is specified, the notification
will be dismissed after 3 seconds."
  (interactive "MMessage: ")
  (setq header-line-format message)
  (redraw-display)
  (run-with-timer 1 nil
                  (lambda ()
                    (let ((old-format header-line-format))
                      (setq header-line-format nil)
                      (redraw-display)))))


;; Tests

;; (toggle-header 'set-test-header)
;; (toggle-header 'notification-header message)

(show-notification "foo")foonilbarnilnilnilnilnilnil



(provide 'headerlinetest)
;;; headerlinetest.el ends here

