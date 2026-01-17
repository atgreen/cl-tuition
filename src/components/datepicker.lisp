;;; components/datepicker.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Date picker component - interactive calendar for date selection
;;;; Inspired by ethanefung/bubble-datepicker

(defpackage #:tuition.components.datepicker
  (:use #:cl)
  (:nicknames #:tui.datepicker)
  (:documentation "Date picker component: interactive calendar for date selection.")
  (:export
   ;; Model
   #:datepicker
   #:make-datepicker

   ;; Accessors
   #:datepicker-time
   #:datepicker-selected
   #:datepicker-focused
   #:datepicker-styles

   ;; Styles
   #:datepicker-styles
   #:make-datepicker-styles
   #:datepicker-styles-header
   #:datepicker-styles-day-names
   #:datepicker-styles-day
   #:datepicker-styles-today
   #:datepicker-styles-selected
   #:datepicker-styles-cursor
   #:datepicker-styles-selected-cursor
   #:datepicker-styles-outside-month

   ;; Operations
   #:datepicker-init
   #:datepicker-update
   #:datepicker-view

   ;; Navigation
   #:datepicker-yesterday
   #:datepicker-tomorrow
   #:datepicker-last-week
   #:datepicker-next-week
   #:datepicker-last-month
   #:datepicker-next-month
   #:datepicker-last-year
   #:datepicker-next-year

   ;; Selection
   #:datepicker-select
   #:datepicker-unselect
   #:datepicker-set-time

   ;; Focus
   #:datepicker-focus
   #:datepicker-blur))

(in-package #:tuition.components.datepicker)

;;; Day name constants
(defparameter *day-names-short* #("Su" "Mo" "Tu" "We" "Th" "Fr" "Sa")
  "Short day names for calendar header.")

(defparameter *month-names*
  #("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December")
  "Full month names.")

;;; Date utilities using Common Lisp universal time

(defun decode-date (universal-time)
  "Decode universal-time to (values year month day day-of-week).
   Day-of-week: 0=Monday ... 6=Sunday (CL convention).
   We convert to 0=Sunday for calendar display."
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    ;; Convert CL dow (0=Mon) to calendar dow (0=Sun)
    (let ((calendar-dow (mod (1+ dow) 7)))
      (values year month day calendar-dow))))

(defun encode-date (year month day)
  "Encode year, month, day to universal-time (at midnight)."
  (encode-universal-time 0 0 0 day month year))

(defun days-in-month (year month)
  "Return number of days in the given month."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (or (and (zerop (mod year 4))
                    (not (zerop (mod year 100))))
               (zerop (mod year 400)))
           29
           28))))

(defun add-days (universal-time days)
  "Add DAYS to universal-time, handling month/year rollover correctly.
   Works across DST boundaries by operating on date components."
  (multiple-value-bind (year month day) (decode-date universal-time)
    (let ((new-day (+ day days)))
      ;; Handle forward overflow
      (loop while (> new-day (days-in-month year month))
            do (decf new-day (days-in-month year month))
               (incf month)
               (when (> month 12)
                 (setf month 1)
                 (incf year)))
      ;; Handle backward underflow
      (loop while (< new-day 1)
            do (decf month)
               (when (< month 1)
                 (setf month 12)
                 (decf year))
               (incf new-day (days-in-month year month)))
      (encode-date year month new-day))))

(defun same-day-p (time1 time2)
  "Return T if both times represent the same calendar day."
  (multiple-value-bind (y1 m1 d1) (decode-date time1)
    (multiple-value-bind (y2 m2 d2) (decode-date time2)
      (and (= y1 y2) (= m1 m2) (= d1 d2)))))

(defun first-day-of-month (year month)
  "Return universal-time for the first day of the given month."
  (encode-date year month 1))

(defun start-of-calendar-grid (year month)
  "Return the Sunday before or on the first of the month."
  (let* ((first (first-day-of-month year month)))
    (multiple-value-bind (y m d dow) (decode-date first)
      (declare (ignore y m d))
      (add-days first (- dow)))))

;;; Datepicker styles

(defclass datepicker-styles ()
  ((header :initarg :header
           :initform nil
           :accessor datepicker-styles-header
           :documentation "Style for month/year header")
   (day-names :initarg :day-names
              :initform nil
              :accessor datepicker-styles-day-names
              :documentation "Style for day name row (Su Mo Tu...)")
   (day :initarg :day
        :initform nil
        :accessor datepicker-styles-day
        :documentation "Style for normal days in current month")
   (today :initarg :today
          :initform nil
          :accessor datepicker-styles-today
          :documentation "Style for today's date")
   (selected :initarg :selected
             :initform nil
             :accessor datepicker-styles-selected
             :documentation "Style for selected date")
   (cursor :initarg :cursor
           :initform nil
           :accessor datepicker-styles-cursor
           :documentation "Style for cursor (focused date)")
   (selected-cursor :initarg :selected-cursor
                    :initform nil
                    :accessor datepicker-styles-selected-cursor
                    :documentation "Style when cursor is on selected date")
   (outside-month :initarg :outside-month
                  :initform nil
                  :accessor datepicker-styles-outside-month
                  :documentation "Style for days outside current month"))
  (:documentation "Customizable styles for datepicker rendering."))

(defun make-datepicker-styles (&key header day-names day today selected
                                    cursor selected-cursor outside-month)
  "Create a datepicker-styles object with custom styles.
   Each style can be a tuition:style object or NIL for defaults.
   Defaults:
   - header: none
   - day-names: none
   - day: none
   - today: bold
   - selected: reverse video
   - cursor: underline
   - selected-cursor: bold + reverse
   - outside-month: faint/dim"
  (make-instance 'datepicker-styles
                 :header (or header (tuition:make-style))
                 :day-names (or day-names (tuition:make-style))
                 :day (or day (tuition:make-style))
                 :today (or today (tuition:make-style :bold t))
                 :selected (or selected (tuition:make-style :reverse t))
                 :cursor (or cursor (tuition:make-style :underline t))
                 :selected-cursor (or selected-cursor
                                      (tuition:make-style :bold t :reverse t))
                 :outside-month (or outside-month (tuition:make-style :faint t))))

(defparameter *default-datepicker-styles* (make-datepicker-styles)
  "Default styles for datepicker.")

;;; Datepicker model

(defclass datepicker ()
  ((time :initarg :time
         :accessor datepicker-time
         :documentation "Currently focused date (universal-time)")
   (selected :initarg :selected
             :initform nil
             :accessor datepicker-selected
             :documentation "Selected date (universal-time) or NIL")
   (focused :initform t
            :accessor datepicker-focused
            :documentation "Whether the datepicker has focus")
   (show-header :initarg :show-header
                :initform t
                :accessor datepicker-show-header
                :documentation "Whether to show month/year header")
   (highlight-today :initarg :highlight-today
                    :initform t
                    :accessor datepicker-highlight-today
                    :documentation "Whether to highlight today's date")
   (styles :initarg :styles
           :accessor datepicker-styles
           :documentation "Datepicker-styles object for customizing appearance"))
  (:documentation "An interactive calendar date picker component."))

(defun make-datepicker (&key time selected (show-header t) (highlight-today t) styles)
  "Create a new datepicker.
   TIME defaults to current date.
   SELECTED is the initially selected date (or NIL).
   STYLES is a datepicker-styles object (defaults to *default-datepicker-styles*)."
  (make-instance 'datepicker
                 :time (or time (get-universal-time))
                 :selected selected
                 :show-header show-header
                 :highlight-today highlight-today
                 :styles (or styles *default-datepicker-styles*)))

;;; Component operations

(defun datepicker-init (picker)
  "Initialize the datepicker. Returns nil (no command needed)."
  (declare (ignore picker))
  nil)

;;; Navigation functions

(defun datepicker-yesterday (picker)
  "Move focus to yesterday."
  (setf (datepicker-time picker)
        (add-days (datepicker-time picker) -1))
  picker)

(defun datepicker-tomorrow (picker)
  "Move focus to tomorrow."
  (setf (datepicker-time picker)
        (add-days (datepicker-time picker) 1))
  picker)

(defun datepicker-last-week (picker)
  "Move focus back one week."
  (setf (datepicker-time picker)
        (add-days (datepicker-time picker) -7))
  picker)

(defun datepicker-next-week (picker)
  "Move focus forward one week."
  (setf (datepicker-time picker)
        (add-days (datepicker-time picker) 7))
  picker)

(defun datepicker-last-month (picker)
  "Move focus back one month."
  (multiple-value-bind (year month day) (decode-date (datepicker-time picker))
    (let* ((new-month (if (= month 1) 12 (1- month)))
           (new-year (if (= month 1) (1- year) year))
           (max-day (days-in-month new-year new-month))
           (new-day (min day max-day)))
      (setf (datepicker-time picker) (encode-date new-year new-month new-day))))
  picker)

(defun datepicker-next-month (picker)
  "Move focus forward one month."
  (multiple-value-bind (year month day) (decode-date (datepicker-time picker))
    (let* ((new-month (if (= month 12) 1 (1+ month)))
           (new-year (if (= month 12) (1+ year) year))
           (max-day (days-in-month new-year new-month))
           (new-day (min day max-day)))
      (setf (datepicker-time picker) (encode-date new-year new-month new-day))))
  picker)

(defun datepicker-last-year (picker)
  "Move focus back one year."
  (multiple-value-bind (year month day) (decode-date (datepicker-time picker))
    (let* ((new-year (1- year))
           (max-day (days-in-month new-year month))
           (new-day (min day max-day)))
      (setf (datepicker-time picker) (encode-date new-year month new-day))))
  picker)

(defun datepicker-next-year (picker)
  "Move focus forward one year."
  (multiple-value-bind (year month day) (decode-date (datepicker-time picker))
    (let* ((new-year (1+ year))
           (max-day (days-in-month new-year month))
           (new-day (min day max-day)))
      (setf (datepicker-time picker) (encode-date new-year month new-day))))
  picker)

;;; Selection functions

(defun datepicker-select (picker)
  "Select the currently focused date."
  (setf (datepicker-selected picker) (datepicker-time picker))
  picker)

(defun datepicker-unselect (picker)
  "Clear the selection."
  (setf (datepicker-selected picker) nil)
  picker)

(defun datepicker-set-time (picker time)
  "Set the focused time."
  (setf (datepicker-time picker) time)
  picker)

;;; Focus functions

(defun datepicker-focus (picker)
  "Give focus to the datepicker."
  (setf (datepicker-focused picker) t)
  picker)

(defun datepicker-blur (picker)
  "Remove focus from the datepicker."
  (setf (datepicker-focused picker) nil)
  picker)

;;; Update function

(defun datepicker-update (picker msg)
  "Update the datepicker with a message. Returns (values new-picker cmd)."
  (cond
    ((and (typep msg 'tuition:key-msg) (datepicker-focused picker))
     (let ((key (tuition:key-msg-key msg))
           (ctrl (tuition:key-msg-ctrl msg)))
       (cond
         ;; Left arrow or h - previous day
         ((or (eq key :left)
              (and (characterp key) (char= key #\h)))
          (datepicker-yesterday picker)
          (values picker nil))

         ;; Right arrow or l - next day
         ((or (eq key :right)
              (and (characterp key) (char= key #\l)))
          (datepicker-tomorrow picker)
          (values picker nil))

         ;; Up arrow or k - previous week
         ((or (eq key :up)
              (and (characterp key) (char= key #\k)))
          (datepicker-last-week picker)
          (values picker nil))

         ;; Down arrow or j - next week
         ((or (eq key :down)
              (and (characterp key) (char= key #\j)))
          (datepicker-next-week picker)
          (values picker nil))

         ;; Page Up or [ - previous month
         ((or (eq key :page-up)
              (and (characterp key) (char= key #\[)))
          (datepicker-last-month picker)
          (values picker nil))

         ;; Page Down or ] - next month
         ((or (eq key :page-down)
              (and (characterp key) (char= key #\])))
          (datepicker-next-month picker)
          (values picker nil))

         ;; Ctrl+Page Up or { - previous year
         ((or (and ctrl (eq key :page-up))
              (and (characterp key) (char= key #\{)))
          (datepicker-last-year picker)
          (values picker nil))

         ;; Ctrl+Page Down or } - next year
         ((or (and ctrl (eq key :page-down))
              (and (characterp key) (char= key #\})))
          (datepicker-next-year picker)
          (values picker nil))

         ;; Home - go to today
         ((eq key :home)
          (datepicker-set-time picker (get-universal-time))
          (values picker nil))

         ;; Enter or Space - select date
         ((or (eq key :enter)
              (and (characterp key) (char= key #\Space)))
          (datepicker-select picker)
          (values picker nil))

         ;; Escape - clear selection
         ((eq key :escape)
          (datepicker-unselect picker)
          (values picker nil))

         (t (values picker nil)))))

    (t (values picker nil))))

;;; View function

(defun datepicker-view (picker)
  "Render the datepicker as a calendar grid."
  (multiple-value-bind (year month day) (decode-date (datepicker-time picker))
    (declare (ignore day))
    (let* ((today (get-universal-time))
           (selected (datepicker-selected picker))
           (focused (datepicker-focused picker))
           (styles (datepicker-styles picker))
           (lines '()))

      ;; Header: Month Year
      (when (datepicker-show-header picker)
        (let* ((header-text (format nil "~A ~D" (aref *month-names* (1- month)) year))
               (styled-header (tuition:render-styled
                               (datepicker-styles-header styles)
                               header-text)))
          (push (format nil "     ~A" styled-header) lines)))

      ;; Day name headers
      (let* ((day-names-text (format nil "~{ ~A~}" (coerce *day-names-short* 'list)))
             (styled-day-names (tuition:render-styled
                                (datepicker-styles-day-names styles)
                                day-names-text)))
        (push styled-day-names lines))

      ;; Calendar grid - 6 weeks to cover all possible month layouts
      (let ((current-date (start-of-calendar-grid year month)))
        (dotimes (week 6)
          (let ((week-str ""))
            (dotimes (dow 7)
              (multiple-value-bind (d-year d-month d-day) (decode-date current-date)
                (declare (ignore d-year))
                (let* ((in-month (= d-month month))
                       (is-today (and (datepicker-highlight-today picker)
                                      (same-day-p current-date today)))
                       (is-selected (and selected (same-day-p current-date selected)))
                       (is-focused (same-day-p current-date (datepicker-time picker)))
                       (day-str (format nil "~2D" d-day))
                       ;; Select appropriate style based on state
                       (style (cond
                                ;; Selected and focused (cursor on selection)
                                ((and is-selected is-focused focused)
                                 (datepicker-styles-selected-cursor styles))
                                ;; Selected but not focused
                                (is-selected
                                 (datepicker-styles-selected styles))
                                ;; Cursor position (focused)
                                ((and is-focused focused)
                                 (datepicker-styles-cursor styles))
                                ;; Today's date
                                (is-today
                                 (datepicker-styles-today styles))
                                ;; Outside current month
                                ((not in-month)
                                 (datepicker-styles-outside-month styles))
                                ;; Normal day in current month
                                (t
                                 (datepicker-styles-day styles))))
                       (styled-day (tuition:render-styled style day-str)))
                  (setf week-str (concatenate 'string week-str " " styled-day))))
              (setf current-date (add-days current-date 1)))
            (push week-str lines))))

      ;; Join lines
      (format nil "~{~A~^~%~}" (nreverse lines)))))
