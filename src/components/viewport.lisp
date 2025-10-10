;;; components/viewport.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Viewport component for scrollable content

(defpackage #:tuition.components.viewport
  (:use #:cl)
  (:nicknames #:tui.viewport)
  (:export
   ;; Viewport creation
   #:viewport
   #:make-viewport

   ;; Accessors
   #:viewport-width
   #:viewport-height
   #:viewport-y-offset
   #:viewport-x-offset
   #:viewport-content

   ;; Operations
   #:viewport-init
   #:viewport-update
   #:viewport-view

   ;; Scrolling
   #:viewport-scroll-down
   #:viewport-scroll-up
   #:viewport-page-down
   #:viewport-page-up
   #:viewport-half-page-down
   #:viewport-half-page-up
   #:viewport-goto-top
   #:viewport-goto-bottom
   #:viewport-scroll-left
   #:viewport-scroll-right

   ;; Status checks
   #:viewport-at-top-p
   #:viewport-at-bottom-p
   #:viewport-scroll-percent

   ;; Content
   #:viewport-set-content
   #:viewport-total-lines
   #:viewport-visible-lines))

(in-package #:tuition.components.viewport)

;;; Viewport model
(defclass viewport ()
  ((width :initarg :width :accessor viewport-width
          :initform 80
          :documentation "Width of the viewport")
   (height :initarg :height :accessor viewport-height
           :initform 24
           :documentation "Height of the viewport")
   (y-offset :initform 0 :accessor viewport-y-offset
             :documentation "Vertical scroll position")
   (x-offset :initform 0 :accessor viewport-x-offset
             :documentation "Horizontal scroll position")
   (lines :initform nil :accessor viewport-lines
          :documentation "Content lines")
   (mouse-wheel-enabled :initarg :mouse-wheel-enabled
                        :accessor viewport-mouse-wheel-enabled
                        :initform t
                        :documentation "Whether mouse wheel scrolling is enabled")
   (mouse-wheel-delta :initarg :mouse-wheel-delta
                      :accessor viewport-mouse-wheel-delta
                      :initform 3
                      :documentation "Number of lines to scroll with mouse wheel")
   (horizontal-step :initarg :horizontal-step
                    :accessor viewport-horizontal-step
                    :initform 4
                    :documentation "Number of columns to scroll horizontally"))
  (:documentation "A viewport for scrollable content."))

(defun make-viewport (&key (width 80) (height 24) content)
  "Create a new viewport with the given dimensions."
  (let ((vp (make-instance 'viewport :width width :height height)))
    (when content
      (viewport-set-content vp content))
    vp))

;;; Content management

(defun viewport-set-content (viewport content)
  "Set the viewport's content."
  (setf (viewport-lines viewport)
        (tuition:split-string-by-newline content))
  ;; Adjust offset if we're past the bottom
  (when (> (viewport-y-offset viewport)
           (viewport-max-y-offset viewport))
    (viewport-goto-bottom viewport)))

(defun viewport-content (viewport)
  "Get the viewport's full content as a string."
  (format nil "~{~A~^~%~}" (viewport-lines viewport)))

;;; Helper functions

(defun viewport-max-y-offset (viewport)
  "Calculate the maximum Y offset."
  (max 0 (- (length (viewport-lines viewport))
            (viewport-height viewport))))

(defun viewport-longest-line-width (viewport)
  "Find the width of the longest line."
  (let ((lines (viewport-lines viewport)))
    (if lines
        (loop for line in lines
              maximize (tuition:visible-length line))
        0)))

(defun viewport-visible-lines-list (viewport)
  "Get the list of currently visible lines."
  (let* ((lines (viewport-lines viewport))
         (h (viewport-height viewport))
         (y (viewport-y-offset viewport))
         (x (viewport-x-offset viewport))
         (w (viewport-width viewport))
         (longest (viewport-longest-line-width viewport)))

    (when (and lines (> (length lines) 0))
      (let* ((top (max 0 y))
             (bottom (min (+ y h) (length lines)))
             (visible (subseq lines top bottom)))

        ;; Handle horizontal scrolling if needed
        (if (and (> longest w) (> x 0))
            (mapcar (lambda (line)
                      (let* ((len (tuition:visible-length line))
                             (start (min x len))
                             (end (min (+ start w) len)))
                        (if (>= start len)
                            ""
                            (subseq line start end))))
                    visible)
            visible)))))

;;; Status checks

(defun viewport-at-top-p (viewport)
  "Check if viewport is at the top."
  (<= (viewport-y-offset viewport) 0))

(defun viewport-at-bottom-p (viewport)
  "Check if viewport is at or past the bottom."
  (>= (viewport-y-offset viewport)
      (viewport-max-y-offset viewport)))

(defun viewport-scroll-percent (viewport)
  "Get scroll position as a percentage (0.0 to 1.0)."
  (let ((lines (length (viewport-lines viewport)))
        (height (viewport-height viewport))
        (offset (viewport-y-offset viewport)))
    (if (>= height lines)
        1.0
        (let ((max-offset (- lines height)))
          (if (<= max-offset 0)
              1.0
              (max 0.0 (min 1.0 (/ (float offset) max-offset))))))))

(defun viewport-total-lines (viewport)
  "Get total number of lines in the viewport."
  (length (viewport-lines viewport)))

(defun viewport-visible-lines (viewport)
  "Get number of currently visible lines."
  (length (viewport-visible-lines-list viewport)))

;;; Scrolling operations

(defun viewport-set-y-offset (viewport offset)
  "Set Y offset, clamping to valid range."
  (setf (viewport-y-offset viewport)
        (max 0 (min offset (viewport-max-y-offset viewport)))))

(defun viewport-set-x-offset (viewport offset)
  "Set X offset, clamping to valid range."
  (setf (viewport-x-offset viewport)
        (max 0 (min offset
                    (max 0 (- (viewport-longest-line-width viewport)
                              (viewport-width viewport)))))))

(defun viewport-scroll-down (viewport &optional (n 1))
  "Scroll down by N lines."
  (unless (viewport-at-bottom-p viewport)
    (viewport-set-y-offset viewport (+ (viewport-y-offset viewport) n)))
  viewport)

(defun viewport-scroll-up (viewport &optional (n 1))
  "Scroll up by N lines."
  (unless (viewport-at-top-p viewport)
    (viewport-set-y-offset viewport (- (viewport-y-offset viewport) n)))
  viewport)

(defun viewport-page-down (viewport)
  "Scroll down by one page (viewport height)."
  (viewport-scroll-down viewport (viewport-height viewport)))

(defun viewport-page-up (viewport)
  "Scroll up by one page (viewport height)."
  (viewport-scroll-up viewport (viewport-height viewport)))

(defun viewport-half-page-down (viewport)
  "Scroll down by half a page."
  (viewport-scroll-down viewport (floor (viewport-height viewport) 2)))

(defun viewport-half-page-up (viewport)
  "Scroll up by half a page."
  (viewport-scroll-up viewport (floor (viewport-height viewport) 2)))

(defun viewport-goto-top (viewport)
  "Go to the top of the content."
  (viewport-set-y-offset viewport 0)
  viewport)

(defun viewport-goto-bottom (viewport)
  "Go to the bottom of the content."
  (viewport-set-y-offset viewport (viewport-max-y-offset viewport))
  viewport)

(defun viewport-scroll-left (viewport &optional (n nil))
  "Scroll left by N columns (or horizontal-step if N is nil)."
  (let ((step (or n (viewport-horizontal-step viewport))))
    (viewport-set-x-offset viewport (- (viewport-x-offset viewport) step)))
  viewport)

(defun viewport-scroll-right (viewport &optional (n nil))
  "Scroll right by N columns (or horizontal-step if N is nil)."
  (let ((step (or n (viewport-horizontal-step viewport))))
    (viewport-set-x-offset viewport (+ (viewport-x-offset viewport) step)))
  viewport)

;;; TEA protocol implementation

(defun viewport-init (viewport)
  "Initialize the viewport. Returns nil (no command)."
  (declare (ignore viewport))
  nil)

(defun viewport-update (viewport msg)
  "Update viewport with a message. Returns (values new-viewport cmd)."
  (cond
    ;; Key messages - standard scrolling keys
    ((tuition:key-msg-p msg)
     (let ((key (tuition:key-msg-key msg))
           (ctrl (tuition:key-msg-ctrl msg)))
       (cond
         ;; Page down: Space, Page Down, or Ctrl+F
         ((or (and (characterp key) (char= key #\Space))
              (eq key :page-down)
              (and ctrl (characterp key) (char= key #\f)))
          (values (viewport-page-down viewport) nil))

         ;; Page up: Page Up, or Ctrl+B
         ((or (eq key :page-up)
              (and ctrl (characterp key) (char= key #\b)))
          (values (viewport-page-up viewport) nil))

         ;; Half page down: Ctrl+D
         ((and ctrl (characterp key) (char= key #\d))
          (values (viewport-half-page-down viewport) nil))

         ;; Half page up: Ctrl+U
         ((and ctrl (characterp key) (char= key #\u))
          (values (viewport-half-page-up viewport) nil))

         ;; Down arrow or j
         ((or (eq key :down)
              (and (characterp key) (char= key #\j)))
          (values (viewport-scroll-down viewport) nil))

         ;; Up arrow or k
         ((or (eq key :up)
              (and (characterp key) (char= key #\k)))
          (values (viewport-scroll-up viewport) nil))

         ;; Left arrow or h
         ((or (eq key :left)
              (and (characterp key) (char= key #\h)))
          (values (viewport-scroll-left viewport) nil))

         ;; Right arrow or l
         ((or (eq key :right)
              (and (characterp key) (char= key #\l)))
          (values (viewport-scroll-right viewport) nil))

         ;; Home or g: go to top
         ((or (eq key :home)
              (and (characterp key) (char= key #\g)))
          (values (viewport-goto-top viewport) nil))

         ;; End or G: go to bottom
         ((or (eq key :end)
              (and (characterp key) (char= key #\G)))
          (values (viewport-goto-bottom viewport) nil))

         ;; No match
         (t (values viewport nil)))))

    ;; Mouse wheel scrolling
    ((tuition:mouse-msg-p msg)
     (when (viewport-mouse-wheel-enabled viewport)
       (let ((button (tuition:mouse-msg-button msg)))
         (cond
           ((eq button :wheel-up)
            (values (viewport-scroll-up viewport
                                       (viewport-mouse-wheel-delta viewport))
                    nil))
           ((eq button :wheel-down)
            (values (viewport-scroll-down viewport
                                         (viewport-mouse-wheel-delta viewport))
                    nil))
           (t (values viewport nil)))))
     (values viewport nil))

    ;; Default: no change
    (t (values viewport nil))))

(defun viewport-view (viewport)
  "Render the viewport to a string."
  (let* ((visible (viewport-visible-lines-list viewport))
         (height (viewport-height viewport))
         (actual-lines (length visible))
         (padding-needed (max 0 (- height actual-lines))))

    (with-output-to-string (s)
      ;; Render visible lines
      (loop for line in visible
            for i from 0
            do (when (> i 0) (format s "~%"))
               (format s "~A" line))

      ;; Add blank lines if content is shorter than viewport
      (dotimes (i padding-needed)
        (when (or (> actual-lines 0) (> i 0))
          (format s "~%"))))))
