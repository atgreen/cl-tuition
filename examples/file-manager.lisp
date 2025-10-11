;;; file-manager.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; File Manager - A text-based file manager built with Tuition

(asdf:load-system :tuition)

(defpackage #:tuition-example-file-manager
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-file-manager)

;;; File item structure
(defstruct file-item
  name
  path
  directory-p
  size
  permissions)

;;; Model
(defclass file-manager-model ()
  ((current-dir :initform (namestring (user-homedir-pathname))
                :accessor current-dir)
   (items :initform '() :accessor items)
   (selected :initform 0 :accessor selected)
   (scroll-offset :initform 0 :accessor scroll-offset)
   (term-width :initform 80 :accessor term-width)
   (term-height :initform 24 :accessor term-height)
   (status-message :initform "" :accessor status-message)
   (error-message :initform nil :accessor error-message)
   (mode :initform :browse :accessor mode) ; :browse, :delete-confirm, :mkdir, :rename
   (input-buffer :initform "" :accessor input-buffer)
   (show-hidden :initform nil :accessor show-hidden)))

;;; Helper functions

(defun get-file-size (path)
  "Get file size in bytes"
  (handler-case
      (with-open-file (stream path :direction :input :if-does-not-exist nil)
        (when stream
          (file-length stream)))
    (error () nil)))

(defun format-size (bytes)
  "Format bytes into human readable format"
  (cond
    ((null bytes) "???")
    ((< bytes 1024) (format nil "~DB" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,1FKB" (/ bytes 1024.0)))
    ((< bytes (* 1024 1024 1024)) (format nil "~,1FMB" (/ bytes (* 1024.0 1024))))
    (t (format nil "~,1FGB" (/ bytes (* 1024.0 1024 1024))))))

(defun get-file-permissions (path)
  "Get file permissions (placeholder - platform dependent)"
  (handler-case
      (let ((truename (truename path)))
        (if (probe-file truename)
            "rwxr-xr-x" ; Simplified - actual implementation would need platform-specific code
            "?????????"))
    (error () "?????????")))

(defun load-directory (path show-hidden)
  "Load directory contents into file-item list"
  (handler-case
      (let* ((dir (truename (pathname path)))
             (entries (directory (merge-pathnames "*.*" dir)))
             (dirs (directory (merge-pathnames "*/" dir)))
             (all-entries (append dirs entries))
             (unique-entries (remove-duplicates all-entries :test #'equal)))
        (sort
         (mapcar (lambda (entry)
                   (let* ((is-dir (null (pathname-name entry)))
                          (name (if is-dir
                                    (car (last (pathname-directory entry)))
                                    (file-namestring entry)))
                          (hidden-p (and name (> (length name) 0)
                                        (char= (char name 0) #\.))))
                     (when (and name (or show-hidden (not hidden-p)))
                       (make-file-item
                        :name name
                        :path (namestring entry)
                        :directory-p is-dir
                        :size (unless is-dir (get-file-size entry))
                        :permissions (get-file-permissions entry)))))
                 unique-entries)
         #'string<
         :key (lambda (item)
                (when item
                  (if (file-item-directory-p item)
                      (concatenate 'string "0" (file-item-name item))
                      (concatenate 'string "1" (file-item-name item)))))))
    (error (e)
      (list (make-file-item
             :name (format nil "Error: ~A" e)
             :path ""
             :directory-p nil
             :size nil
             :permissions "")))))

(defun parent-directory (path)
  "Get parent directory path"
  (let* ((dir (pathname path))
         (parent-dirs (butlast (pathname-directory dir))))
    (if parent-dirs
        (namestring (make-pathname :directory parent-dirs
                                   :defaults dir))
        "/")))

;;; Helper for viewport scrolling
(defun adjust-scroll (model viewport-height)
  "Adjust scroll offset to keep selected item visible"
  (let* ((selected (selected model))
         (offset (scroll-offset model))
         (num-items (length (items model)))
         ;; Calculate the ideal offset to show as many items as possible
         (max-offset (max 0 (- num-items viewport-height))))
    (cond
      ;; Selected item is above viewport - scroll up
      ((< selected offset)
       (setf (scroll-offset model) selected))
      ;; Selected item is below viewport - scroll down
      ((>= selected (+ offset viewport-height))
       (setf (scroll-offset model) (min max-offset (- selected viewport-height -1))))
      ;; If we're scrolled down but there's room to show more items from the top
      ;; (happens when terminal gets taller), adjust scroll offset down
      ((> offset max-offset)
       (setf (scroll-offset model) max-offset)))))

;;; Forward declarations for helper functions
(defun delete-directory-recursive (path)
  "Recursively delete a directory"
  (let ((dir (truename (pathname path))))
    ;; Delete all files
    (dolist (file (directory (merge-pathnames "*.*" dir)))
      (delete-file file))
    ;; Delete all subdirectories
    (dolist (subdir (directory (merge-pathnames "*/" dir)))
      (when (pathname-name subdir)
        (delete-directory-recursive subdir)))
    ;; Delete the directory itself
    (delete-file dir)))

;;; Debug logging
(defun log-debug (msg &rest args)
  (with-open-file (out "/tmp/file-manager-debug.log"
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~A: ~?~%" (get-universal-time) msg args)))

;;; Init
(defmethod tui:init ((model file-manager-model))
  ;; Initialize terminal size immediately so first render is correct
  (let ((size (tui:get-terminal-size)))
    (log-debug "init: get-terminal-size returned: ~S" size)
    (when size
      (setf (term-width model) (car size))
      (setf (term-height model) (cdr size))
      (log-debug "init: set dimensions to ~Dx~D" (term-width model) (term-height model))))
  ;; The model will also receive window-size messages on resize
  (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
  (setf (status-message model) (format nil "Loaded ~D items" (length (items model))))
  nil)

;;; Update message handlers

(defun handle-browse-keys (model msg)
  "Handle keys in browse mode"
  (let ((key (tui:key-msg-key msg)))
    (cond
      ;; Quit
      ((and (characterp key) (char= key #\q))
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (values model (tui:quit-cmd)))

      ;; Navigation
      ((or (eq key :up) (and (characterp key) (char= key #\k)))
       (when (> (selected model) 0)
         (decf (selected model))
         (setf (error-message model) nil))
       (values model nil))

      ((or (eq key :down) (and (characterp key) (char= key #\j)))
       (when (< (selected model) (1- (length (items model))))
         (incf (selected model))
         (setf (error-message model) nil))
       (values model nil))

      ;; Enter directory
      ((eq key :enter)
       (when (items model)
         (let ((item (nth (selected model) (items model))))
           (when (and item (file-item-directory-p item))
             (setf (current-dir model) (file-item-path item))
             (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
             (setf (selected model) 0)
             (setf (status-message model) (format nil "~A (~D items)"
                                                  (current-dir model)
                                                  (length (items model))))
             (setf (error-message model) nil))))
       (values model nil))

      ;; Go to parent directory
      ((and (characterp key) (char= key #\h))
       (let ((parent (parent-directory (current-dir model))))
         (setf (current-dir model) parent)
         (setf (items model) (remove nil (load-directory parent (show-hidden model))))
         (setf (selected model) 0)
         (setf (status-message model) (format nil "~A (~D items)" parent (length (items model))))
         (setf (error-message model) nil))
       (values model nil))

      ;; Go to home
      ((and (characterp key) (char= key #\~))
       (let ((home (namestring (user-homedir-pathname))))
         (setf (current-dir model) home)
         (setf (items model) (remove nil (load-directory home (show-hidden model))))
         (setf (selected model) 0)
         (setf (status-message model) (format nil "~A (~D items)" home (length (items model))))
         (setf (error-message model) nil))
       (values model nil))

      ;; Refresh
      ((and (characterp key) (char= key #\r))
       (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
       (when (>= (selected model) (length (items model)))
         (setf (selected model) (max 0 (1- (length (items model))))))
       (setf (status-message model) "Refreshed")
       (setf (error-message model) nil)
       (values model nil))

      ;; Toggle hidden files
      ((and (characterp key) (char= key #\.))
       (setf (show-hidden model) (not (show-hidden model)))
       (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
       (setf (status-message model) (if (show-hidden model)
                                        "Showing hidden files"
                                        "Hiding hidden files"))
       (setf (error-message model) nil)
       (values model nil))

      ;; Delete file
      ((and (characterp key) (char= key #\d))
       (when (items model)
         (setf (mode model) :delete-confirm)
         (setf (error-message model) nil))
       (values model nil))

      ;; Create directory
      ((and (characterp key) (char= key #\n))
       (setf (mode model) :mkdir)
       (setf (input-buffer model) "")
       (setf (error-message model) nil)
       (values model nil))

      ;; Rename
      ((and (characterp key) (char= key #\R))
       (when (items model)
         (let ((item (nth (selected model) (items model))))
           (when item
             (setf (mode model) :rename)
             (setf (input-buffer model) (file-item-name item))
             (setf (error-message model) nil))))
       (values model nil))

      (t (values model nil)))))

(defun handle-delete-confirm-keys (model msg)
  "Handle keys in delete confirmation mode"
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\y))
       (let ((item (nth (selected model) (items model))))
         (when item
           (handler-case
               (progn
                 (if (file-item-directory-p item)
                     (delete-directory-recursive (file-item-path item))
                     (delete-file (file-item-path item)))
                 (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
                 (when (>= (selected model) (length (items model)))
                   (setf (selected model) (max 0 (1- (length (items model))))))
                 (setf (status-message model) (format nil "Deleted: ~A" (file-item-name item)))
                 (setf (error-message model) nil))
             (error (e)
               (setf (error-message model) (format nil "Error deleting: ~A" e))))))
       (setf (mode model) :browse)
       (values model nil))

      (t
       (setf (mode model) :browse)
       (values model nil)))))

(defun handle-mkdir-keys (model msg)
  "Handle keys in mkdir mode"
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((eq key :enter)
       (when (> (length (input-buffer model)) 0)
         (let ((new-dir-path (merge-pathnames (input-buffer model) (current-dir model))))
           (handler-case
               (progn
                 (ensure-directories-exist new-dir-path)
                 (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
                 (setf (status-message model) (format nil "Created: ~A" (input-buffer model)))
                 (setf (error-message model) nil))
             (error (e)
               (setf (error-message model) (format nil "Error creating directory: ~A" e))))))
       (setf (mode model) :browse)
       (setf (input-buffer model) "")
       (values model nil))

      ((eq key :escape)
       (setf (mode model) :browse)
       (setf (input-buffer model) "")
       (values model nil))

      ((eq key :backspace)
       (when (> (length (input-buffer model)) 0)
         (setf (input-buffer model) (subseq (input-buffer model) 0 (1- (length (input-buffer model))))))
       (values model nil))

      ((characterp key)
       (setf (input-buffer model) (concatenate 'string (input-buffer model) (string key)))
       (values model nil))

      (t (values model nil)))))

(defun handle-rename-keys (model msg)
  "Handle keys in rename mode"
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((eq key :enter)
       (when (> (length (input-buffer model)) 0)
         (let* ((item (nth (selected model) (items model)))
                (old-path (file-item-path item))
                (new-path (merge-pathnames (input-buffer model) (current-dir model))))
           (handler-case
               (progn
                 (rename-file old-path new-path)
                 (setf (items model) (remove nil (load-directory (current-dir model) (show-hidden model))))
                 (setf (status-message model) (format nil "Renamed to: ~A" (input-buffer model)))
                 (setf (error-message model) nil))
             (error (e)
               (setf (error-message model) (format nil "Error renaming: ~A" e))))))
       (setf (mode model) :browse)
       (setf (input-buffer model) "")
       (values model nil))

      ((eq key :escape)
       (setf (mode model) :browse)
       (setf (input-buffer model) "")
       (values model nil))

      ((eq key :backspace)
       (when (> (length (input-buffer model)) 0)
         (setf (input-buffer model) (subseq (input-buffer model) 0 (1- (length (input-buffer model))))))
       (values model nil))

      ((characterp key)
       (setf (input-buffer model) (concatenate 'string (input-buffer model) (string key)))
       (values model nil))

      (t (values model nil)))))

;;; Update messages dispatch

(defmethod tui:update-message ((model file-manager-model) (msg tui:key-msg))
  (case (mode model)
    (:browse (handle-browse-keys model msg))
    (:delete-confirm (handle-delete-confirm-keys model msg))
    (:mkdir (handle-mkdir-keys model msg))
    (:rename (handle-rename-keys model msg))
    (t (values model nil))))

(defmethod tui:update-message ((model file-manager-model) (msg tui:window-size-msg))
  ;; Update terminal dimensions when window is resized
  (log-debug "window-size-msg: ~Dx~D"
             (tui:window-size-msg-width msg)
             (tui:window-size-msg-height msg))
  (setf (term-width model) (tui:window-size-msg-width msg))
  (setf (term-height model) (tui:window-size-msg-height msg))
  (values model nil))

;;; View

;; Forward declarations
(declaim (ftype (function (file-manager-model) string) view-browse view-delete-confirm view-mkdir view-rename))

(defmethod tui:view ((model file-manager-model))
  (case (mode model)
    (:browse (view-browse model))
    (:delete-confirm (view-delete-confirm model))
    (:mkdir (view-mkdir model))
    (:rename (view-rename model))
    (t "")))

(defun view-browse (model)
  "Main browse view"
  (let* ((term-height (term-height model))
         (term-width (term-width model))
         ;; Reserve space for title bar (1 line), border (2 lines), status (2 lines), help (3 lines), padding (2 lines)
         (available-height (max 5 (- term-height 10)))
         (viewport-height available-height)
         (box-width (- term-width 2)))

    (log-debug "view-browse: term=~Dx~D, avail-height=~D, viewport=~D"
               term-width term-height available-height viewport-height)

    ;; Adjust scroll to keep selected item visible
    (adjust-scroll model viewport-height)

    (with-output-to-string (s)
      ;; Title bar with background color
      (let* ((title "  FILE MANAGER  ")
             (path-str (format nil "  ~A  " (current-dir model)))
             (padding-len (max 0 (- box-width (+ (tui:visible-length title)
                                                  (tui:visible-length path-str))))))
        (format s "~%~A~A~A~%"
                (tui:colored title :fg tui:*fg-black* :bg tui:*bg-cyan*)
                (tui:colored (make-string padding-len :initial-element #\Space)
                            :bg tui:*bg-cyan*)
                (tui:colored path-str :fg tui:*fg-bright-white* :bg tui:*bg-blue*)))

      ;; File list with viewport scrolling - wrapped in a border
      (let ((content
              (with-output-to-string (content-stream)
                (if (null (items model))
                    (format content-stream "~A~%" (tui:italic "  Empty directory"))
                    (let* ((offset (scroll-offset model))
                           (visible-items (subseq (items model)
                                                 offset
                                                 (min (length (items model))
                                                      (+ offset viewport-height)))))
                      (loop for item in visible-items
                            for i from offset
                            do (let ((selected-p (= i (selected model)))
                                     (icon (if (file-item-directory-p item) "📁" "📄"))
                                     (name (file-item-name item))
                                     (size (format-size (file-item-size item))))
                                 (if selected-p
                                     (format content-stream " ~A ~A ~A~A~%"
                                             (tui:colored ">" :fg tui:*fg-bright-green*)
                                             icon
                                             (tui:colored (tui:bold name) :fg tui:*fg-bright-cyan*)
                                             (if (file-item-directory-p item)
                                                 ""
                                                 (tui:colored (format nil " (~A)" size)
                                                             :fg tui:*fg-bright-black*)))
                                     (format content-stream "   ~A ~A~A~%"
                                             icon
                                             name
                                             (if (file-item-directory-p item)
                                                 ""
                                                 (tui:colored (format nil " (~A)" size)
                                                              :fg tui:*fg-bright-black*))))))

                      ;; Show scroll indicator if there are more items
                      (when (> (length (items model)) viewport-height)
                        (format content-stream "~%~A"
                                (tui:colored
                                 (format nil " Showing ~D-~D of ~D items "
                                         (1+ offset)
                                         (min (length (items model)) (+ offset viewport-height))
                                         (length (items model)))
                                 :fg tui:*fg-bright-black*))))))))
        ;; Render the border around the content with fixed width and height
        (let* ((lines (tui:split-string-by-newline content))
               (max-content-width (- box-width 4))
               ;; Pad lines to fixed width
               (padded-lines (mapcar (lambda (line)
                                      (let* ((vis-len (tui:visible-length line))
                                             (padding (max 0 (- max-content-width vis-len))))
                                        (format nil "~A~A"
                                                line
                                                (make-string padding :initial-element #\Space))))
                                    lines))
               ;; Pad height to fill viewport with empty lines
               (lines-needed (- viewport-height (length padded-lines)))
               (empty-line (make-string max-content-width :initial-element #\Space))
               (height-padded-lines (append padded-lines
                                           (loop repeat (max 0 lines-needed)
                                                 collect empty-line)))
               (padded-content (format nil "~{~A~^~%~}" height-padded-lines)))
          (format s "~A~%" (tui:render-border padded-content tui:*border-rounded*))))

      ;; Status or error message
      (when (error-message model)
        (format s "  ~A~%" (tui:colored (error-message model) :fg tui:*fg-red*)))

      (when (and (not (error-message model)) (> (length (status-message model)) 0))
        (format s "  ~A [term: ~Dx~D]~%"
                (tui:colored (status-message model) :fg tui:*fg-bright-green*)
                (term-width model)
                (term-height model)))

      ;; Help
      (format s "~%  ~A~%"
              (tui:colored "↑/k up • ↓/j down • enter open • h parent • ~ home • r refresh • . hidden" :fg tui:*fg-yellow*))
      (format s "  ~A~%"
              (tui:colored "n new dir • R rename • d delete • q quit" :fg tui:*fg-yellow*)))))

(defun view-delete-confirm (model)
  "Delete confirmation view"
  (let* ((item (nth (selected model) (items model)))
         (term-width (term-width model))
         (content
           (with-output-to-string (s)
             (format s "~%  ~A~%~%" (tui:bold (tui:colored "DELETE CONFIRMATION" :fg tui:*fg-red*)))
             (when item
               (format s "  Delete ~A?~%~%" (tui:bold (file-item-name item)))
               (format s "  Type: ~A~%" (if (file-item-directory-p item) "Directory" "File"))
               (format s "  Path: ~A~%~%" (file-item-path item)))
             (format s "  ~A~%" (tui:colored "y confirm • any other key cancel" :fg tui:*fg-yellow*)))))
    (tui:place-horizontal (max 60 (min 80 (- term-width 4))) tui:+center+
      (tui:render-border content tui:*border-double*))))

(defun view-mkdir (model)
  "Create directory view"
  (let* ((term-width (term-width model))
         (content
           (with-output-to-string (s)
             (format s "~%  ~A~%~%" (tui:bold (tui:colored "CREATE DIRECTORY" :fg tui:*fg-cyan*)))
             (format s "  Directory name: ~A~%~%" (tui:bold (input-buffer model)))
             (format s "  ~A~%" (tui:colored "enter confirm • esc cancel" :fg tui:*fg-yellow*)))))
    (tui:place-horizontal (max 50 (min 70 (- term-width 4))) tui:+center+
      (tui:render-border content tui:*border-rounded*))))

(defun view-rename (model)
  "Rename view"
  (let* ((item (nth (selected model) (items model)))
         (term-width (term-width model))
         (content
           (with-output-to-string (s)
             (format s "~%  ~A~%~%" (tui:bold (tui:colored "RENAME" :fg tui:*fg-cyan*)))
             (when item
               (format s "  Old name: ~A~%" (file-item-name item)))
             (format s "  New name: ~A~%~%" (tui:bold (input-buffer model)))
             (format s "  ~A~%" (tui:colored "enter confirm • esc cancel" :fg tui:*fg-yellow*)))))
    (tui:place-horizontal (max 50 (min 70 (- term-width 4))) tui:+center+
      (tui:render-border content tui:*border-rounded*))))

;;; Main entry point
(defun main ()
  ;; Suppress poll warnings from SBCL when terminal resizes
  ;; These are harmless warnings about signal handling during I/O
  #+sbcl
  (handler-bind ((warning #'muffle-warning))
    (let ((program (tui:make-program (make-instance 'file-manager-model)
                                     :alt-screen t)))
      (tui:run program)))
  #-sbcl
  (let ((program (tui:make-program (make-instance 'file-manager-model)
                                   :alt-screen t)))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))

#+nil
(main)
