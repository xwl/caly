;;; caly.el --- display year calendar

;;; Commentary:

;; It displays a year calendar and holiday faces. When displaying the
;; current year, the cursor will be placed on the day. It doesn't
;; handle any of the calendar input, such as motion.

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;; Evaluate buffer
;;
;; M-x caly

;;; Code:

(require 'calendar)

(defvar caly-buffer "*caly*"
  "Name of the calendar year buffer")

(defvar displayed-month)
(defvar displayed-year)
(defun caly-calendar-generate-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted at the top of the buffer in which point is currently
located, but indented INDENT spaces.  The indentation is done from the first
character on the line and does not disturb the first INDENT characters on the
line."
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
        (last (calendar-last-day-of-month month year))
        (trunc (min calendar-intermonth-spacing
                    (1- calendar-left-margin)))
        (day 1)
        (row 0)
        string)
    (calendar-move-to-column indent)
    (insert
     (calendar-string-spread (list calendar-month-header)
                             ?\s calendar-month-digit-width))
    (calendar-ensure-newline)
    (calendar-insert-at-column indent calendar-intermonth-header trunc)
    ;; Use the first two characters of each day to head the columns.
    (dotimes (i 7)
      (insert
       (progn
         (setq string
               (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t))
         (if enable-multibyte-characters
             (truncate-string-to-width string calendar-day-header-width)
           (substring string 0 calendar-day-header-width)))
       (make-string (- calendar-column-width calendar-day-header-width) ?\s)))
    (calendar-ensure-newline)
    (calendar-insert-at-column indent calendar-intermonth-text trunc)
    ;; Add blank days before the first of the month.
    (insert (make-string (* blank-days calendar-column-width) ?\s))
    ;; Put in the days of the month.
    (dotimes (i last)
      (setq day (1+ i))
      ;; TODO should numbers be left-justified, centered...?
      (insert (format (format "%%%dd%%s" calendar-day-digit-width) day
                      (make-string
                       (- calendar-column-width calendar-day-digit-width) ?\s)))
      ;; 'date property prevents intermonth text confusing re-searches.
      ;; (Tried intangible, it did not really work.)
      (set-text-properties
       (- (point) (1+ calendar-day-digit-width)) (1- (point))
       `(mouse-face highlight help-echo ,(eval calendar-date-echo-text)
                    date t))
      (when (and (zerop (mod (+ day blank-days) 7))
                 (/= day last))
        (calendar-ensure-newline)
        (setq row (1+ row))
        (setq day (1+ day))              ; first day of next week
        (calendar-insert-at-column indent calendar-intermonth-text trunc)))
    row))

(defun caly-calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (goto-char (point-min))
    (forward-line (+ calendar-first-date-row -1
                     (/ (+ day -1
                           (mod
                            (- (calendar-day-of-week (list month 1 year))
                               calendar-week-start-day)
                            7))
                        7)))
    (move-to-column (+ calendar-left-margin (1- calendar-day-digit-width)
                       (* calendar-month-width
                          (1+ (calendar-interval
                               displayed-month displayed-year month year)))
                       (* calendar-column-width
                          (mod
                           (- (calendar-day-of-week date)
                              calendar-week-start-day)
                           7))))))

(define-derived-mode caly-calendar-mode nil "Calendar"
  "A major mode for the year calendar window."
  (setq buffer-read-only t
        buffer-undo-list t
        indent-tabs-mode nil)
  (set (make-local-variable 'scroll-margin) 0) ; bug#10379
  (make-local-variable 'calendar-mark-ring)
  (make-local-variable 'displayed-month) ; month in middle of window
  (make-local-variable 'displayed-year)  ; year in middle of window
  (setq displayed-month 1)
  (setq displayed-year 2001))

;;;###autoload
(defun caly ()
  (interactive)
  (let* ((today (calendar-current-date))
         (today-month (nth 0 today))
         (today-day (nth 1 today))
         (today-year (nth 2 today))
         (orig-calendar-buffer calendar-buffer)
         (mark '(1))
         (count 0)
         row this-year year)
    (setq
     this-year
     (string-to-number (read-from-minibuffer "Enter year to display: "
                                             (int-to-string today-year)))
     year this-year)
    (when (get-buffer caly-buffer)
      (kill-buffer caly-buffer))
    (switch-to-buffer (get-buffer-create caly-buffer))
    (dolist (month '(1 4 7 10))
      (dotimes (i 3)
        (setq row (caly-calendar-generate-month month year
                                                (+ calendar-left-margin
                                                   (* calendar-month-width i))))
        (calendar-increment-month month year 1)
        (when (> (% (1+ i) 3) 0)
          (forward-line (- (+ row 2)))
          (beginning-of-line)))
      (goto-char (point-max))
      (insert "\n\n")
      (add-to-list 'mark (point) t))
    (caly-calendar-mode)
    (setq displayed-year this-year)
    (setq calendar-buffer caly-buffer)
    (when calendar-mark-holidays-flag
      (dolist (month '(1 4 7 10))
        (narrow-to-region (nth count mark) (nth (1+ count) mark))
        (setq displayed-month (1+ month))
        (calendar-mark-holidays)
        (setq count (1+ count))))
    (widen)
    (if (= today-year this-year)
        (progn
          (setq count (/ today-month 3))
          (narrow-to-region (nth count mark) (nth (1+ count) mark))
          (setq displayed-month (+ (* (/ (- today-month 1) 3) 3) 2))
          (calendar-cursor-to-visible-date today)
          (widen))
      (goto-char (point-min)))
    (setq calendar-buffer orig-calendar-buffer)
    (other-window 1)

  ;; show all holidays of the year
  (list-holidays today-year)))


;; Copyright (C) 2013 Ivan Kanis
;; Author: Ivan Kanis
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;; vi:et:sw=4:ts=4:
;; Local Variables:
;; compile-command: "make"
;; End:
