;;; arborio.el --- A package spawning a buffer that gives you some general information.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  

;; Author: ian
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defgroup arborio-mode ()
  "Major mode for viewing the arborio."
  :group 'home)

(define-derived-mode arborio-mode special-mode "arborio"
  (define-key arborio-mode-map (kbd "SPC") 'arborio-refresh))

(defvar *arborio-content* nil)

(defvar *arborio-timer* nil)

(defvar *arborio-spinny-animation* '("|" "/" "-" "\\"))

(defvar *arborio-animation-directory* (expand-file-name "./frames"))

(defvar *arborio-pizza-animation-frame-files* (list
					       (format "%s/%s" *arborio-animation-directory* "pizza1")
					       (format "%s/%s" *arborio-animation-directory* "pizza2")
					       (format "%s/%s" *arborio-animation-directory* "pizza3")
					       (format "%s/%s" *arborio-animation-directory* "pizza4")
					       ))

(defvar *arborio-pizza-animation* nil)

(defvar *arborio-rainbow-colors* '("red" "orange" "yellow" "green" "blue" "violet"))

(defun arborio-load-animation-frames ()
  (interactive)
  (setq *arborio-pizza-animation* (mapcar #'arborio-load-frame-from-file *arborio-pizza-animation-frame-files*)))

(defun arborio-load-frame-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun timer-test ()
  (message "I have been called %s" (current-time-string)))

(defun arborio ()
  (interactive)
  (switch-to-buffer "*arborio*")
  (arborio-mode)
  (arborio-load-animation-frames)
  (arborio-run-once)
  (arborio-timer-start)
  (get-buffer-create "*arborio*"))

(defun arborio-timer-start ()
  (interactive)
  (when (timerp *arborio-timer*)
    (cancel-timer *arborio-timer*))
  (setq *arborio-timer*
	(run-with-timer 2 2 #'arborio-refresh)))

(defun arborio-timer-stop ()
  (interactive)
  (when (timerp *arborio-timer*)
    (cancel-timer *arborio-timer*))
  (setq *arborio-timer* nil))

(defun arborio-run-once ()
  (interactive)
  (when (timerp *arborio-timer*)
    (cancel-timer *arborio-timer*))
  (setq *arborio-timer*
	(run-with-idle-timer 1 nil #'arborio-refresh)))

(defun arborio-refresh ()
  (interactive)
  (save-window-excursion
    (pop-to-buffer "*arborio*" '(display-buffer-no-window))
    (let (
	  (cpu-perc 0.37)
	  (mem-perc (arborio-get-mem-perc)))
      (setq *arborio-content*
	    (list
	     (arborio-text (arborio-figlet "Linux Club Hackathon 22" "block") "rainbow")
	     (arborio-text (arborio-figlet (arborio-timedate) "script"))
	     (arborio-text (format "%s %s %s\n" "CPU" cpu-perc (make-bar cpu-perc)))
	     (arborio-text (format "%s %s %s\n" "MEM" mem-perc (make-bar mem-perc)))
	     (arborio-text (arborio-animation-advance-frame *arborio-spinny-animation*))
	     (arborio-text (arborio-animation-advance-frame *arborio-pizza-animation*) "orange")
	     (arborio-text "pizza time" "red")
	     (arborio-whitespace 3)
	     (arborio-text (arborio-elfeed-unread))
	     (arborio-whitespace)
	     (arborio-text (arborio-calendar-month))
	     ;(arborio-org-agenda-list)
	     )))
    (arborio-print)))

(defun arborio-text (str &optional color)
  (if color (arborio-colorize-text str color) str))

(defun arborio-colorize-text (str color)
  (let ((color (if (string= color "rainbow") (arborio-rainbow-cycle) color)))
    (propertize str 'font-lock-face `(:foreground ,color))))

(defun arborio-rainbow-cycle ()
  (arborio-cycle-list *arborio-rainbow-colors*))

(defun arborio-print ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((content *arborio-content*))
      (while content
	(insert (car content))
	(setq content (cdr content))))))

(defun arborio-whitespace (&optional n)
  (make-string (or n 1) ?\n))

(defun arborio-timedate (&optional format)
  (if (null format) (format-time-string "%A, %H:%M:%S") (format-time-string format)))

(defun make-bar (perc &optional length)
  (let ((length (or length (setq length 100))) (fill (round (* perc length))))
    (format "[%s%s]" (make-string fill ?#) (make-string (- length fill) ?\xB7))))

(defun arborio-get-cpu-perc-bad ()
  (interactive)
  (string-to-number
   (shell-command-to-string "printf %s \"$(mpstat 1 1 | tail -n 1 | awk '$12 ~ /[0-9.]+/ { print (100 - $12) / 100 }')\"")))

(defun arborio-read-proc ()
  (interactive)
  (let ((stats (split-string
		(shell-command-to-string "cat /proc/stat")
		"\n")))
    (mapcar (lambda (str)
	      (let ((s (split-string str)))
		(cons (car s)
		      (mapcar #'string-to-number
			      (cdr s)))))
	    stats)))

; todo: make this not bad
(defun arborio-get-cpu-perc ()
  (interactive)
  (let ((proc-data (arborio-read-proc)))
    (format "%s" )))

(defun arborio-get-mem-perc ()
  (interactive)
  (string-to-number
   (shell-command-to-string "printf %s \"$(free | head -2 | tail -1 | awk '{ print $3/$2 }')\"")))

(defun arborio-figlet (str &optional font)
  (interactive)
  (let ((font (or font (setq font "standard"))) (func (format "figlet -w %d -f %s \'%s\'" (window-width) font str)))
    (shell-command-to-string func)))

(defun arborio-elfeed-unread ()
  (interactive)
  (require 'elfeed)
  (format "%s unread articles\n" (elfeed-search--count-unread)))

(defun arborio-calendar-month ()
  (interactive)
  (require 'calendar)
  (let ((date (calendar-current-date)))
    (with-temp-buffer
      (calendar-generate-month (car date) (car (last date)) 0)
      (format "%s\n" (buffer-string)))))

					; todo: find a better way to get the agenda list or make it interactable.
(defun arborio-org-agenda-list ()
  (interactive)
  (require 'org)
  (with-temp-buffer
    (org-agenda-list)
    (format "%s\n" (buffer-string))))

(defun arborio-animation-advance-frame (framedata)
  (arborio-cycle-list framedata))

(defun arborio-cycle-list (l)
  					; wraps the car of l to the end of the list, returns that element.
  (let ((first (car l)))
    (setcar l (car (cdr l)))
    (setcdr l (append (cdr (cdr l)) (list first)))
    first))
  

(provide 'arborio)
;;; arborio.el ends 
