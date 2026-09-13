;;; components/filepicker.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;;; File picker component (ports the bubbles filepicker).
;;;;
;;;; Directory listings are read asynchronously: FILEPICKER-INIT (and
;;;; navigation into another directory) returns a command that reads the
;;;; directory and produces a FILEPICKER-READ-DIR-MSG, which the update
;;;; function consumes.  Delegate all messages to FILEPICKER-UPDATE and call
;;;; FILEPICKER-DID-SELECT-FILE after it to learn when the user picked a file.

(defpackage #:tuition.components.filepicker
  (:use #:cl)
  (:nicknames #:tui.filepicker)
  (:export
   ;; Model
   #:filepicker
   #:make-filepicker
   #:filepicker-current-directory
   #:filepicker-path
   #:filepicker-allowed-types
   #:filepicker-show-permissions
   #:filepicker-show-size
   #:filepicker-show-hidden
   #:filepicker-dir-allowed
   #:filepicker-file-allowed
   #:filepicker-auto-height
   #:filepicker-height
   #:filepicker-set-height
   #:filepicker-cursor
   #:filepicker-selected

   ;; Styles
   #:filepicker-cursor-style
   #:filepicker-selected-style
   #:filepicker-directory-style
   #:filepicker-symlink-style
   #:filepicker-file-style
   #:filepicker-disabled-style
   #:filepicker-permission-style
   #:filepicker-size-style
   #:filepicker-empty-style

   ;; Operations
   #:filepicker-init
   #:filepicker-update
   #:filepicker-view
   #:filepicker-did-select-file
   #:filepicker-did-select-disabled-file
   #:filepicker-highlighted-path

   ;; Messages
   #:filepicker-read-dir-msg
   #:filepicker-read-dir-msg-p
   #:filepicker-error-msg
   #:filepicker-error-msg-p
   #:filepicker-error-msg-error))

(in-package #:tuition.components.filepicker)

(defconstant +margin-bottom+ 5
  "Rows reserved below the picker when AUTO-HEIGHT tracks the window size.")
(defconstant +file-size-width+ 7)

(defvar *filepicker-id-counter* 0)

;;; Directory entries

(defstruct (%fp-entry (:constructor %make-fp-entry))
  "One directory entry: display name, full native path, and metadata."
  name path dir-p symlink-p target size mode)

(defun %fp-mode-string (path dir-p symlink-p)
  "A ls-style permission string for PATH, or \"\" when unavailable."
  #-(and sbcl unix) (declare (ignore path dir-p symlink-p))
  #+(and sbcl unix)
  (handler-case
      (let* ((mode (sb-posix:stat-mode (sb-posix:lstat path)))
             (type-char (cond (symlink-p #\l) (dir-p #\d) (t #\-))))
        (with-output-to-string (s)
          (write-char type-char s)
          (loop for shift from 6 downto 0 by 3
                for bits = (ldb (byte 3 shift) mode)
                do (write-char (if (logtest bits 4) #\r #\-) s)
                   (write-char (if (logtest bits 2) #\w #\-) s)
                   (write-char (if (logtest bits 1) #\x #\-) s))))
    (error () ""))
  #-(and sbcl unix)
  "")

(defun %fp-symlink-p (path)
  "Whether PATH itself is a symbolic link."
  #-(and sbcl unix) (declare (ignore path))
  #+(and sbcl unix)
  (handler-case
      (= (logand (sb-posix:stat-mode (sb-posix:lstat path)) #o170000) #o120000)
    (error () nil))
  #-(and sbcl unix)
  nil)

(defun %fp-file-size (path)
  "Size of the file at PATH in bytes, or NIL when unavailable."
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (file-length in))
    (error () nil)))

(defun %fp-make-entry (name path dir-p)
  (let* ((symlink-p (%fp-symlink-p path))
         (target (when symlink-p
                   (handler-case (uiop:native-namestring (truename path))
                     (error () nil))))
         ;; A symlink whose target is a directory navigates like one.
         (dir-p (or dir-p
                    (and symlink-p target
                         (uiop:directory-exists-p target) t))))
    (%make-fp-entry :name name :path path
                    :dir-p dir-p :symlink-p symlink-p :target target
                    :size (unless dir-p (%fp-file-size path))
                    :mode (%fp-mode-string path dir-p symlink-p))))

(defun %fp-list-directory (dir show-hidden)
  "List DIR as a sorted list of %FP-ENTRY (directories first, then by name)."
  (let* ((dirpath (uiop:ensure-directory-pathname dir))
         (entries '()))
    (dolist (sub (uiop:subdirectories dirpath))
      (let ((name (car (last (pathname-directory sub)))))
        (push (%fp-make-entry name
                              (string-right-trim "/" (uiop:native-namestring sub))
                              t)
              entries)))
    (dolist (file (uiop:directory-files dirpath))
      (push (%fp-make-entry (file-namestring file)
                            (uiop:native-namestring file)
                            nil)
            entries))
    (unless show-hidden
      (setf entries (remove-if (lambda (e)
                                 (let ((n (%fp-entry-name e)))
                                   (and (plusp (length n))
                                        (char= (char n 0) #\.))))
                               entries)))
    (sort entries
          (lambda (a b)
            (if (eq (%fp-entry-dir-p a) (%fp-entry-dir-p b))
                (string< (%fp-entry-name a) (%fp-entry-name b))
                (%fp-entry-dir-p a))))))

(defun %fp-humanize-bytes (n)
  "Humanize a byte count with SI units, like go-humanize: 42B, 4.2kB, 1.0MB."
  (if (null n)
      ""
      (if (< n 1000)
          (format nil "~DB" n)
          (loop for unit in '("kB" "MB" "GB" "TB" "PB" "EB")
                for div = 1000d0 then (* div 1000)
                when (< n (* div 1000))
                  return (format nil "~,1F~A" (/ n div) unit)
                finally (return (format nil "~,1FZB" (/ n (* div 1000))))))))

;;; Messages

(defclass filepicker-read-dir-msg (tuition:message)
  ((id :initarg :id :reader filepicker-read-dir-msg-id)
   (entries :initarg :entries :reader filepicker-read-dir-msg-entries))
  (:documentation "A directory listing produced by the read-dir command."))

(defun filepicker-read-dir-msg-p (obj)
  (typep obj 'filepicker-read-dir-msg))

(defclass filepicker-error-msg (tuition:message)
  ((error :initarg :error :reader filepicker-error-msg-error))
  (:documentation "A failure reading a directory."))

(defun filepicker-error-msg-p (obj)
  (typep obj 'filepicker-error-msg))

;;; Model

(defclass filepicker ()
  ((id :initform (incf *filepicker-id-counter*) :reader filepicker-id)
   (current-directory :initarg :current-directory :initform "."
                      :accessor filepicker-current-directory
                      :documentation "The directory being listed")
   (path :initform nil :accessor filepicker-path
         :documentation "The path the user selected, or NIL")
   (allowed-types :initarg :allowed-types :initform nil
                  :accessor filepicker-allowed-types
                  :documentation "Selectable file suffixes (nil = any file)")
   (files :initform nil :accessor filepicker-files
          :documentation "Current directory entries (%FP-ENTRY list)")
   (show-permissions :initarg :show-permissions :initform t
                     :accessor filepicker-show-permissions)
   (show-size :initarg :show-size :initform t :accessor filepicker-show-size)
   (show-hidden :initarg :show-hidden :initform nil
                :accessor filepicker-show-hidden)
   (dir-allowed :initarg :dir-allowed :initform nil
                :accessor filepicker-dir-allowed
                :documentation "Whether directories can be selected")
   (file-allowed :initarg :file-allowed :initform t
                 :accessor filepicker-file-allowed
                 :documentation "Whether files can be selected")
   (selected :initform 0 :accessor filepicker-selected
             :documentation "Index of the highlighted entry")
   (min-idx :initform 0 :accessor filepicker-min-idx)
   (max-idx :initform 0 :accessor filepicker-max-idx)
   (selected-stack :initform nil :accessor filepicker-selected-stack)
   (min-stack :initform nil :accessor filepicker-min-stack)
   (max-stack :initform nil :accessor filepicker-max-stack)
   (height :initarg :height :initform 0 :accessor filepicker-height
           :documentation "Viewport height in rows (0 = unset)")
   (auto-height :initarg :auto-height :initform t
                :accessor filepicker-auto-height
                :documentation "Track window-size messages, minus a margin")
   (cursor :initarg :cursor :initform ">" :accessor filepicker-cursor)
   ;; Styles
   (cursor-style :initarg :cursor-style
                 :initform (tuition:make-style :foreground tuition:*fg-magenta*)
                 :accessor filepicker-cursor-style)
   (selected-style :initarg :selected-style
                   :initform (tuition:make-style :foreground tuition:*fg-magenta*
                                                 :bold t)
                   :accessor filepicker-selected-style)
   (directory-style :initarg :directory-style
                    :initform (tuition:make-style :foreground tuition:*fg-blue*)
                    :accessor filepicker-directory-style)
   (symlink-style :initarg :symlink-style
                  :initform (tuition:make-style :foreground tuition:*fg-cyan*)
                  :accessor filepicker-symlink-style)
   (file-style :initarg :file-style :initform nil
               :accessor filepicker-file-style)
   (disabled-style :initarg :disabled-style
                   :initform (tuition:make-style
                              :foreground tuition:*fg-bright-black*)
                   :accessor filepicker-disabled-style)
   (permission-style :initarg :permission-style
                     :initform (tuition:make-style
                                :foreground tuition:*fg-bright-black*)
                     :accessor filepicker-permission-style)
   (size-style :initarg :size-style
               :initform (tuition:make-style
                          :foreground tuition:*fg-bright-black*)
               :accessor filepicker-size-style)
   (empty-style :initarg :empty-style
                :initform (tuition:make-style
                           :foreground tuition:*fg-bright-black*)
                :accessor filepicker-empty-style))
  (:documentation "A file picker over the local filesystem."))

(defun make-filepicker (&rest args &key &allow-other-keys)
  "Create a new filepicker.  See the FILEPICKER class for the options."
  (apply #'make-instance 'filepicker args))

;;; Reading directories (as commands)

(defun %fp-read-dir-cmd (fp)
  "A command that lists the picker's current directory."
  (let ((id (filepicker-id fp))
        (dir (filepicker-current-directory fp))
        (show-hidden (filepicker-show-hidden fp)))
    (lambda ()
      (handler-case
          (make-instance 'filepicker-read-dir-msg
                         :id id
                         :entries (%fp-list-directory dir show-hidden))
        (error (e)
          (make-instance 'filepicker-error-msg :error e))))))

(defun filepicker-init (fp)
  "Initialize the picker.  Returns the command that reads the directory."
  (%fp-read-dir-cmd fp))

;;; Viewport windowing

(defun %fp-bottom-idx (fp top)
  "Index of the last visible entry when TOP is first.  With no height set,
one entry is shown so the view is never blank (bubbles #1026)."
  (let ((h (filepicker-height fp)))
    (if (< h 1) top (+ top h -1))))

(defun filepicker-set-height (fp h)
  "Set the picker's viewport height."
  (setf (filepicker-height fp) h)
  (when (> (filepicker-max-idx fp) (1- h))
    (setf (filepicker-max-idx fp) (%fp-bottom-idx fp (filepicker-min-idx fp))))
  fp)

(defun %fp-push-view (fp)
  (push (filepicker-selected fp) (filepicker-selected-stack fp))
  (push (filepicker-min-idx fp) (filepicker-min-stack fp))
  (push (filepicker-max-idx fp) (filepicker-max-stack fp)))

(defun %fp-pop-view (fp)
  (setf (filepicker-selected fp) (pop (filepicker-selected-stack fp))
        (filepicker-min-idx fp) (pop (filepicker-min-stack fp))
        (filepicker-max-idx fp) (pop (filepicker-max-stack fp))))

;;; Selection rules

(defun %fp-can-select-name (fp name)
  "Whether NAME passes the ALLOWED-TYPES suffix filter."
  (let ((types (filepicker-allowed-types fp)))
    (or (null types)
        (some (lambda (ext)
                (and (>= (length name) (length ext))
                     (string= name ext :start1 (- (length name) (length ext)))))
              types))))

(defun %fp-current-entry (fp)
  (nth (filepicker-selected fp) (filepicker-files fp)))

(defun %fp-entry-disabled-p (fp entry)
  (and (not (%fp-entry-dir-p entry))
       (not (%fp-can-select-name fp (%fp-entry-name entry)))))

;;; Navigation

(defun %fp-go-to-top (fp)
  (setf (filepicker-selected fp) 0
        (filepicker-min-idx fp) 0
        (filepicker-max-idx fp) (%fp-bottom-idx fp 0)))

(defun %fp-go-to-last (fp)
  (let ((n (length (filepicker-files fp))))
    (setf (filepicker-selected fp) (1- n)
          (filepicker-min-idx fp) (max (- n (filepicker-height fp)) 0)
          (filepicker-max-idx fp) (1- n))))

(defun %fp-down (fp)
  (let ((n (length (filepicker-files fp))))
    (incf (filepicker-selected fp))
    (when (>= (filepicker-selected fp) n)
      (setf (filepicker-selected fp) (1- n)))
    (when (> (filepicker-selected fp) (filepicker-max-idx fp))
      (incf (filepicker-min-idx fp))
      (incf (filepicker-max-idx fp)))))

(defun %fp-up (fp)
  (decf (filepicker-selected fp))
  (when (< (filepicker-selected fp) 0)
    (setf (filepicker-selected fp) 0))
  (when (< (filepicker-selected fp) (filepicker-min-idx fp))
    (decf (filepicker-min-idx fp))
    (decf (filepicker-max-idx fp))))

(defun %fp-page-down (fp)
  (let ((n (length (filepicker-files fp)))
        (h (filepicker-height fp)))
    (incf (filepicker-selected fp) h)
    (when (>= (filepicker-selected fp) n)
      (setf (filepicker-selected fp) (1- n)))
    (incf (filepicker-min-idx fp) h)
    (incf (filepicker-max-idx fp) h)
    (when (>= (filepicker-max-idx fp) n)
      (setf (filepicker-max-idx fp) (1- n)
            (filepicker-min-idx fp) (max (- (filepicker-max-idx fp) h) 0)))))

(defun %fp-page-up (fp)
  (let ((h (filepicker-height fp)))
    (decf (filepicker-selected fp) h)
    (when (< (filepicker-selected fp) 0)
      (setf (filepicker-selected fp) 0))
    (decf (filepicker-min-idx fp) h)
    (decf (filepicker-max-idx fp) h)
    (when (< (filepicker-min-idx fp) 0)
      (setf (filepicker-min-idx fp) 0
            (filepicker-max-idx fp) h))))

(defun %fp-back (fp)
  "Navigate to the parent directory.  Returns the read-dir command."
  (setf (filepicker-current-directory fp)
        (uiop:native-namestring
         (uiop:pathname-parent-directory-pathname
          (uiop:ensure-directory-pathname
           (filepicker-current-directory fp)))))
  (if (filepicker-selected-stack fp)
      (%fp-pop-view fp)
      (%fp-go-to-top fp))
  (%fp-read-dir-cmd fp))

(defun %fp-open (fp select-p)
  "Open the highlighted entry.  With SELECT-P, record an allowed entry as the
selection.  Returns a read-dir command when descending into a directory."
  (let ((entry (%fp-current-entry fp)))
    (when entry
      (let ((dir-p (%fp-entry-dir-p entry)))
        (when (and select-p
                   (if dir-p
                       (filepicker-dir-allowed fp)
                       (and (filepicker-file-allowed fp)
                            (%fp-can-select-name fp (%fp-entry-name entry)))))
          (setf (filepicker-path fp) (%fp-entry-path entry)))
        (when dir-p
          (setf (filepicker-current-directory fp) (%fp-entry-path entry))
          (%fp-push-view fp)
          (%fp-go-to-top fp)
          (%fp-read-dir-cmd fp))))))

;;; TEA protocol

(defun filepicker-update (fp msg)
  "Update the picker with a message.  Returns (values fp cmd)."
  (cond
    ;; Directory listing arrived (for this picker instance).
    ((and (filepicker-read-dir-msg-p msg)
          (= (filepicker-read-dir-msg-id msg) (filepicker-id fp)))
     (setf (filepicker-files fp) (filepicker-read-dir-msg-entries msg))
     (setf (filepicker-max-idx fp)
           (max (filepicker-max-idx fp)
                (%fp-bottom-idx fp (filepicker-min-idx fp))))
     (values fp nil))

    ;; Window size
    ((tuition:window-size-msg-p msg)
     (when (filepicker-auto-height fp)
       (filepicker-set-height fp (- (tuition:window-size-msg-height msg)
                                    +margin-bottom+)))
     (setf (filepicker-max-idx fp)
           (%fp-bottom-idx fp (filepicker-min-idx fp)))
     (values fp nil))

    ((tuition:key-press-msg-p msg)
     (let ((key (tuition:key-event-code msg)))
       (cond
         ((and (characterp key) (char= key #\g)) (%fp-go-to-top fp)
          (values fp nil))
         ((and (characterp key) (char= key #\G)) (%fp-go-to-last fp)
          (values fp nil))
         ((or (eq key :down)
              (and (characterp key) (char= key #\j))
              (and (characterp key) (char= key #\n)
                   (tuition:mod-contains (tuition:key-event-mod msg)
                                         tuition:+mod-ctrl+)))
          (%fp-down fp)
          (values fp nil))
         ((or (eq key :up)
              (and (characterp key) (char= key #\k))
              (and (characterp key) (char= key #\p)
                   (tuition:mod-contains (tuition:key-event-mod msg)
                                         tuition:+mod-ctrl+)))
          (%fp-up fp)
          (values fp nil))
         ((or (eq key :page-down) (and (characterp key) (char= key #\J)))
          (%fp-page-down fp)
          (values fp nil))
         ((or (eq key :page-up) (and (characterp key) (char= key #\K)))
          (%fp-page-up fp)
          (values fp nil))
         ((or (eq key :backspace) (eq key :left) (eq key :escape)
              (and (characterp key) (char= key #\h)))
          (values fp (%fp-back fp)))
         ((or (eq key :enter) (eq key :right)
              (and (characterp key) (char= key #\l)))
          (if (null (filepicker-files fp))
              (values fp nil)
              (values fp (%fp-open fp (eq key :enter)))))
         (t (values fp nil)))))

    (t (values fp nil))))

;;; Rendering

(defun %fp-styled (style text)
  (if style (tuition:render-styled style text) text))

(defun filepicker-view (fp)
  "Render the picker's visible window."
  (let ((files (filepicker-files fp))
        (height (filepicker-height fp)))
    (when (null files)
      (return-from filepicker-view
        (with-output-to-string (s)
          (write-string (%fp-styled (filepicker-empty-style fp)
                                    "  Bummer. No Files Found.")
                        s)
          (loop repeat (max 0 (1- height)) do (write-char #\Newline s)))))
    (with-output-to-string (s)
      (let ((lines 0)
            (cursor (filepicker-cursor fp)))
        (loop for entry in files
              for i from 0
              do (when (and (>= i (filepicker-min-idx fp))
                            (<= i (filepicker-max-idx fp)))
                   (let* ((disabled (%fp-entry-disabled-p fp entry))
                          (name (%fp-entry-name entry))
                          (perms (%fp-entry-mode entry))
                          (size (format nil "~V@A" +file-size-width+
                                        (%fp-humanize-bytes (%fp-entry-size entry))))
                          (suffix (when (and (%fp-entry-symlink-p entry)
                                             (%fp-entry-target entry))
                                    (format nil " → ~A" (%fp-entry-target entry)))))
                     (if (= i (filepicker-selected fp))
                         ;; The highlighted row is styled as one unit.
                         (let ((row (concatenate
                                     'string
                                     (if (filepicker-show-permissions fp)
                                         (format nil " ~A" perms) "")
                                     (if (filepicker-show-size fp) size "")
                                     " " name (or suffix ""))))
                           (write-string
                            (%fp-styled (if disabled
                                            (filepicker-disabled-style fp)
                                            (filepicker-cursor-style fp))
                                        cursor)
                            s)
                           (write-string
                            (%fp-styled (if disabled
                                            (filepicker-disabled-style fp)
                                            (filepicker-selected-style fp))
                                        row)
                            s))
                         (progn
                           (write-string (make-string
                                          (tuition:visible-length cursor)
                                          :initial-element #\Space)
                                         s)
                           (when (filepicker-show-permissions fp)
                             (write-string " " s)
                             (write-string
                              (%fp-styled (filepicker-permission-style fp) perms)
                              s))
                           (when (filepicker-show-size fp)
                             (write-string
                              (%fp-styled (filepicker-size-style fp) size) s))
                           (write-string " " s)
                           (write-string
                            (%fp-styled
                             (cond ((%fp-entry-dir-p entry)
                                    (filepicker-directory-style fp))
                                   ((%fp-entry-symlink-p entry)
                                    (filepicker-symlink-style fp))
                                   (disabled (filepicker-disabled-style fp))
                                   (t (filepicker-file-style fp)))
                             name)
                            s)
                           (when suffix (write-string suffix s))))
                     (write-char #\Newline s)
                     (incf lines))))
        ;; Pad to the configured height.
        (loop for i from lines below height
              do (write-char #\Newline s))))))

;;; Selection queries

(defun %fp-did-select (fp msg)
  "Whether MSG was an Enter press that recorded a selection.
Returns (VALUES SELECTED-P PATH)."
  (if (and (filepicker-files fp)
           (tuition:key-press-msg-p msg)
           (eq (tuition:key-event-code msg) :enter)
           (%fp-current-entry fp)
           (filepicker-path fp))
      (let* ((entry (%fp-current-entry fp))
             (dir-p (%fp-entry-dir-p entry)))
        (if (or (and (not dir-p) (filepicker-file-allowed fp))
                (and dir-p (filepicker-dir-allowed fp)))
            (values t (filepicker-path fp))
            (values nil nil)))
      (values nil nil)))

(defun filepicker-did-select-file (fp msg)
  "After FILEPICKER-UPDATE, report whether MSG selected an allowed file.
Returns (VALUES SELECTED-P PATH)."
  (multiple-value-bind (did path) (%fp-did-select fp msg)
    (if (and did (%fp-can-select-name fp path))
        (values t path)
        (values nil nil))))

(defun filepicker-did-select-disabled-file (fp msg)
  "After FILEPICKER-UPDATE, report whether MSG tried to select a file
excluded by ALLOWED-TYPES.  Returns (VALUES SELECTED-P PATH)."
  (multiple-value-bind (did path) (%fp-did-select fp msg)
    (if (and did (not (%fp-can-select-name fp path)))
        (values t path)
        (values nil nil))))

(defun filepicker-highlighted-path (fp)
  "The full path of the highlighted entry, or NIL."
  (let ((entry (%fp-current-entry fp)))
    (when entry (%fp-entry-path entry))))
