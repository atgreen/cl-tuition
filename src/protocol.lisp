;;; protocol.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Core protocol for the Elm Architecture pattern

(in-package #:tuition)

;;; Messages
(deftype msg () t)
(deftype cmd () '(or null function list))

(defclass message () ()
  (:documentation "Root class of all messages."))

(defmacro defmessage (name slots &key print-name documentation)
  "Define a message class NAME with SLOTS and a simple printer.
SLOTS are DEFCLASS slot specs. PRINT-NAME overrides the printed tag.
DOCUMENTATION sets the class docstring (defaults to a short description)."
  (let ((pname (or print-name name))
        (doc (or documentation (format nil "Message ~A." name))))
    `(progn
       (defclass ,name (message)
         ,slots
         (:documentation ,doc))
       (defmethod print-object ((m ,name) stream)
         (print-unreadable-object (m stream :type t :identity t)
           (format stream "~A" ',pname))))))

;;; Model protocol
(defgeneric init (model)
  (:documentation "Initialize the model and return an optional initial command."))

(defgeneric update (model msg)
  (:documentation "Handle a message and return (values new-model cmd)."))

(defgeneric update-message (model msg)
  (:documentation "Dispatch helper that can be specialized by message classes. Default returns (values model nil)."))

(defmethod update ((model t) msg)
  (update-message model msg))

(defmethod update-message ((model t) (msg t))
  (declare (ignore msg))
  (values model nil))

(defgeneric view (model)
  (:documentation "Render the model to a view-state or string for display."))

;;; Built-in messages (CLOS)
(defmessage quit-msg () :print-name quit)

;;; ---------- Modifier bitmask system ----------

(defconstant +mod-shift+     #b000000001)
(defconstant +mod-alt+       #b000000010)
(defconstant +mod-ctrl+      #b000000100)
(defconstant +mod-meta+      #b000001000)
(defconstant +mod-hyper+     #b000010000)
(defconstant +mod-super+     #b000100000)
(defconstant +mod-caps-lock+ #b001000000)
(defconstant +mod-num-lock+  #b010000000)

(defun mod-contains (mod flag)
  "Return T if bitmask MOD contains FLAG."
  (logtest mod flag))

(defun make-mod (&key shift alt ctrl meta hyper super caps-lock num-lock)
  "Construct a modifier bitmask from keyword flags."
  (logior (if shift +mod-shift+ 0)
          (if alt +mod-alt+ 0)
          (if ctrl +mod-ctrl+ 0)
          (if meta +mod-meta+ 0)
          (if hyper +mod-hyper+ 0)
          (if super +mod-super+ 0)
          (if caps-lock +mod-caps-lock+ 0)
          (if num-lock +mod-num-lock+ 0)))

;;; ---------- Key event hierarchy ----------

(defclass key-event (message)
  ((code :initarg :code :reader key-event-code
         :documentation "Key code: character for printable, keyword for special keys")
   (mod :initarg :mod :initform 0 :reader key-event-mod
        :documentation "Modifier bitmask")
   (text :initarg :text :initform "" :reader key-event-text
         :documentation "Printable text produced by the key")
   (shifted-code :initarg :shifted-code :initform nil :reader key-event-shifted-code
                 :documentation "Shifted key code (Kitty protocol only)")
   (base-code :initarg :base-code :initform nil :reader key-event-base-code
              :documentation "Base key on PC-101 layout (Kitty protocol only)")
   (repeat-p :initarg :repeat-p :initform nil :reader key-event-repeat-p
             :documentation "Key repeat (Kitty protocol only)"))
  (:documentation "Base class for key events."))

(defclass key-press-msg (key-event) ()
  (:documentation "Key press event."))

(defclass key-release-msg (key-event) ()
  (:documentation "Key release event."))

(defun make-key-press-msg (&key code (mod 0) (text "") shifted-code base-code repeat-p)
  "Construct a key-press-msg."
  (make-instance 'key-press-msg :code code :mod mod :text text
                 :shifted-code shifted-code :base-code base-code :repeat-p repeat-p))

(defun make-key-release-msg (&key code (mod 0) (text "") shifted-code base-code)
  "Construct a key-release-msg."
  (make-instance 'key-release-msg :code code :mod mod :text text
                 :shifted-code shifted-code :base-code base-code))

(defun key-press-msg-p (obj)
  "Return true if OBJ is a key-press-msg."
  (typep obj 'key-press-msg))

(defun key-release-msg-p (obj)
  "Return true if OBJ is a key-release-msg."
  (typep obj 'key-release-msg))

;; Bracketed paste message (ESC [ 200 ~ ... ESC [ 201 ~)
(defmessage paste-msg
  ((text :initarg :text :reader paste-msg-text)))

(defmessage window-size-msg
  ((width :initarg :width :reader window-size-msg-width)
   (height :initarg :height :reader window-size-msg-height)))

;;; ---------- Mouse event hierarchy ----------

(defclass mouse-event (message)
  ((x :initarg :x :reader mouse-event-x :documentation "X coordinate (column, 1-based)")
   (y :initarg :y :reader mouse-event-y :documentation "Y coordinate (row, 1-based)")
   (mod :initarg :mod :initform 0 :reader mouse-event-mod
        :documentation "Modifier bitmask"))
  (:documentation "Base class for all mouse events."))

(defclass mouse-click-msg (mouse-event)
  ((button :initarg :button :reader mouse-event-button
           :documentation "Button: :left, :middle, or :right"))
  (:documentation "Mouse button clicked."))

(defclass mouse-release-msg (mouse-event)
  ((button :initarg :button :reader mouse-event-button
           :documentation "Button: :left, :middle, or :right"))
  (:documentation "Mouse button released."))

(defclass mouse-motion-msg (mouse-event)
  ((button :initarg :button :initform nil :reader mouse-event-button
           :documentation "Button held during motion, or nil"))
  (:documentation "Mouse moved (with or without button held)."))

(defclass mouse-wheel-msg (mouse-event)
  ((direction :initarg :direction :reader mouse-wheel-direction
              :documentation "Scroll direction: :up or :down")
   (count :initarg :count :initform 1 :accessor mouse-wheel-count
          :documentation "Number of scroll events coalesced."))
  (:documentation "Mouse scroll wheel event."))

;; Mouse event constructors
(defun make-mouse-click-msg (&key x y button (mod 0))
  "Construct a mouse-click-msg."
  (make-instance 'mouse-click-msg :x x :y y :button button :mod mod))

(defun make-mouse-release-msg (&key x y button (mod 0))
  "Construct a mouse-release-msg."
  (make-instance 'mouse-release-msg :x x :y y :button button :mod mod))

(defun make-mouse-motion-msg (&key x y button (mod 0))
  "Construct a mouse-motion-msg."
  (make-instance 'mouse-motion-msg :x x :y y :button button :mod mod))

(defun make-mouse-wheel-msg (&key x y direction (mod 0))
  "Construct a mouse-wheel-msg."
  (make-instance 'mouse-wheel-msg :x x :y y :direction direction :mod mod))

;; Mouse event predicates
(defun mouse-event-p (obj)
  "Return true if OBJ is a mouse-event."
  (typep obj 'mouse-event))

(defun mouse-click-msg-p (obj)
  "Return true if OBJ is a mouse-click-msg."
  (typep obj 'mouse-click-msg))

(defun mouse-release-msg-p (obj)
  "Return true if OBJ is a mouse-release-msg."
  (typep obj 'mouse-release-msg))

(defun mouse-motion-msg-p (obj)
  "Return true if OBJ is a mouse-motion-msg."
  (typep obj 'mouse-motion-msg))

(defun mouse-wheel-msg-p (obj)
  "Return true if OBJ is a mouse-wheel-msg."
  (typep obj 'mouse-wheel-msg))

;; Tick message for timer/animation updates
(defmessage tick-msg
  ((time :initarg :time :initform (get-internal-real-time) :reader tick-msg-time)))

;; Suspend/resume messages
(defmessage suspend-msg () :print-name suspend)
(defmessage resume-msg () :print-name resume)

;;; ---------- Focus events (renamed from v1) ----------

(defmessage focus-msg ()
  :documentation "Terminal window gained focus.")

(defmessage blur-msg ()
  :documentation "Terminal window lost focus.")

;;; ---------- Keyboard enhancements message ----------

(defmessage keyboard-enhancements-msg
  ((flags :initarg :flags :initform 0 :reader keyboard-enhancements-flags))
  :documentation "Keyboard enhancement capabilities reported by terminal.")

(defun keyboard-enhancements-msg-p (obj)
  "Return true if OBJ is a keyboard-enhancements-msg."
  (typep obj 'keyboard-enhancements-msg))

;;; ---------- Paste start/end messages ----------

(defmessage paste-start-msg ()
  :documentation "Bracketed paste started (ESC[200~).")

(defmessage paste-end-msg ()
  :documentation "Bracketed paste ended (ESC[201~).")

;;; ---------- Color query messages ----------

(defmessage background-color-msg
  ((color :initarg :color :reader background-color-msg-color))
  :documentation "Terminal background color response.")

(defmessage foreground-color-msg
  ((color :initarg :color :reader foreground-color-msg-color))
  :documentation "Terminal foreground color response.")

(defmessage cursor-color-msg
  ((color :initarg :color :reader cursor-color-msg-color))
  :documentation "Terminal cursor color response.")

(defmessage cursor-position-msg
  ((x :initarg :x :reader cursor-position-msg-x)
   (y :initarg :y :reader cursor-position-msg-y))
  :documentation "Cursor position response.")

;;; ---------- Terminal query messages ----------

(defmessage terminal-version-msg
  ((version :initarg :version :reader terminal-version-msg-version))
  :documentation "Terminal version/identification response.")

(defmessage mode-report-msg
  ((mode :initarg :mode :reader mode-report-msg-mode)
   (setting :initarg :setting :reader mode-report-msg-setting
            :documentation "0=not recognized, 1=set, 2=reset, 3=permanently set, 4=permanently reset"))
  :documentation "DEC Private Mode Report (DECRPM) response.")

;;; ---------- Clipboard messages ----------

(defmessage clipboard-msg
  ((content :initarg :content :reader clipboard-msg-content))
  :documentation "Clipboard content received via OSC 52.")

;;; ---------- Constructors and predicates ----------

(defun make-quit-msg ()
  "Construct a quit-msg instance."
  (make-instance 'quit-msg))

(defun quit-msg-p (obj)
  "Return true if OBJ is a quit-msg."
  (typep obj 'quit-msg))

(defun make-paste-msg (&key text)
  "Construct a paste-msg carrying TEXT."
  (make-instance 'paste-msg :text text))

(defun paste-msg-p (obj)
  "Return true if OBJ is a paste-msg."
  (typep obj 'paste-msg))

(defun make-window-size-msg (&key width height)
  "Construct a window-size-msg with WIDTH and HEIGHT."
  (make-instance 'window-size-msg :width width :height height))

(defun window-size-msg-p (obj)
  "Return true if OBJ is a window-size-msg."
  (typep obj 'window-size-msg))

(defun make-tick-msg (&key time)
  "Construct a tick-msg with TIME (defaults to current internal time)."
  (make-instance 'tick-msg :time (or time (get-internal-real-time))))

(defun tick-msg-p (obj)
  "Return true if OBJ is a tick-msg."
  (typep obj 'tick-msg))

(defun make-suspend-msg ()
  "Construct a suspend-msg."
  (make-instance 'suspend-msg))

(defun suspend-msg-p (obj)
  "Return true if OBJ is a suspend-msg."
  (typep obj 'suspend-msg))

(defun make-resume-msg ()
  "Construct a resume-msg."
  (make-instance 'resume-msg))

(defun resume-msg-p (obj)
  "Return true if OBJ is a resume-msg."
  (typep obj 'resume-msg))

(defun make-paste-start-msg ()
  "Construct a paste-start-msg."
  (make-instance 'paste-start-msg))

(defun paste-start-msg-p (obj)
  "Return true if OBJ is a paste-start-msg."
  (typep obj 'paste-start-msg))

(defun make-paste-end-msg ()
  "Construct a paste-end-msg."
  (make-instance 'paste-end-msg))

(defun paste-end-msg-p (obj)
  "Return true if OBJ is a paste-end-msg."
  (typep obj 'paste-end-msg))

(defun make-cursor-color-msg (&key color)
  "Construct a cursor-color-msg."
  (make-instance 'cursor-color-msg :color color))

(defun make-terminal-version-msg (&key version)
  "Construct a terminal-version-msg."
  (make-instance 'terminal-version-msg :version version))

(defun make-mode-report-msg (&key mode setting)
  "Construct a mode-report-msg."
  (make-instance 'mode-report-msg :mode mode :setting setting))

(defun make-clipboard-msg (&key content)
  "Construct a clipboard-msg."
  (make-instance 'clipboard-msg :content content))

(defun clipboard-msg-p (obj)
  "Return true if OBJ is a clipboard-msg."
  (typep obj 'clipboard-msg))

(defun make-focus-msg ()
  "Construct a focus-msg."
  (make-instance 'focus-msg))

(defun focus-msg-p (obj)
  "Return true if OBJ is a focus-msg."
  (typep obj 'focus-msg))

(defun make-blur-msg ()
  "Construct a blur-msg."
  (make-instance 'blur-msg))

(defun blur-msg-p (obj)
  "Return true if OBJ is a blur-msg."
  (typep obj 'blur-msg))

;;; ---------- Declarative view-state ----------

(defclass cursor ()
  ((x :initarg :x :initform 0 :accessor cursor-x)
   (y :initarg :y :initform 0 :accessor cursor-y)
   (shape :initarg :shape :initform :block :accessor cursor-shape
          :documentation "Cursor shape: :block, :underline, or :bar")
   (color :initarg :color :initform nil :accessor cursor-color)
   (blink :initarg :blink :initform nil :accessor cursor-blink))
  (:documentation "Cursor state for declarative view."))

(defun make-cursor (&key (x 0) (y 0) (shape :block) color blink)
  "Create a cursor instance."
  (make-instance 'cursor :x x :y y :shape shape :color color :blink blink))

(defclass view-state ()
  ((content :initarg :content :initform "" :accessor view-state-content
            :documentation "The rendered content string")
   (alt-screen :initarg :alt-screen :initform nil :accessor view-state-alt-screen
               :documentation "Whether to use alternate screen")
   (mouse-mode :initarg :mouse-mode :initform nil :accessor view-state-mouse-mode
               :documentation ":cell-motion, :all-motion, or nil")
   (report-focus :initarg :report-focus :initform nil :accessor view-state-report-focus
                 :documentation "Whether to report focus events")
   (window-title :initarg :window-title :initform nil :accessor view-state-window-title
                 :documentation "Window title or nil")
   (cursor :initarg :cursor :initform nil :accessor view-state-cursor
           :documentation "Cursor instance or nil (nil = hidden)")
   (foreground-color :initarg :foreground-color :initform nil :accessor view-state-foreground-color)
   (background-color :initarg :background-color :initform nil :accessor view-state-background-color)
   (keyboard-enhancements :initarg :keyboard-enhancements :initform nil
                          :accessor view-state-keyboard-enhancements
                          :documentation "Plist of requested keyboard enhancements")
   (on-mouse :initarg :on-mouse :initform nil :accessor view-state-on-mouse
             :documentation "Function (mouse-event) -> cmd, or nil")
   (disable-bracketed-paste :initarg :disable-bracketed-paste :initform nil
                            :accessor view-state-disable-bracketed-paste
                            :documentation "When T, disable bracketed paste mode")
   (unicode-mode :initarg :unicode-mode :initform nil :accessor view-state-unicode-mode
                 :documentation "When T, enable Unicode mode (mode 2027)"))
  (:documentation "Declarative view state returned by view methods."))

(defun make-view (content &key alt-screen mouse-mode report-focus window-title
                            cursor foreground-color background-color
                            keyboard-enhancements on-mouse
                            disable-bracketed-paste unicode-mode)
  "Create a view-state.  CONTENT is the rendered string."
  (make-instance 'view-state
                 :content content
                 :alt-screen alt-screen
                 :mouse-mode mouse-mode
                 :report-focus report-focus
                 :window-title window-title
                 :cursor cursor
                 :foreground-color foreground-color
                 :background-color background-color
                 :keyboard-enhancements keyboard-enhancements
                 :on-mouse on-mouse
                 :disable-bracketed-paste disable-bracketed-paste
                 :unicode-mode unicode-mode))

(defun view-state-p (obj)
  "Return true if OBJ is a view-state."
  (typep obj 'view-state))

;;; ---------- Terminal transition logic ----------

(defun apply-terminal-transitions (renderer prev current stream)
  "Compare PREV and CURRENT view-states and emit minimal escape sequences
to transition the terminal.  Called by the renderer before content diffing."
  (declare (ignore renderer))
  ;; Alt screen
  (let ((prev-alt (and prev (view-state-alt-screen prev)))
        (cur-alt (view-state-alt-screen current)))
    (when (and cur-alt (not prev-alt))
      (format stream "~C[?1049h" #\Escape))
    (when (and prev-alt (not cur-alt))
      (format stream "~C[?1049l" #\Escape)))

  ;; Mouse mode
  (let ((prev-mouse (and prev (view-state-mouse-mode prev)))
        (cur-mouse (view-state-mouse-mode current)))
    (unless (eq prev-mouse cur-mouse)
      ;; Disable old mode
      (when prev-mouse
        (format stream "~C[?1002l~C[?1003l~C[?1006l" #\Escape #\Escape #\Escape))
      ;; Enable new mode
      (ecase cur-mouse
        ((nil) nil)
        (:cell-motion
         (format stream "~C[?1002h~C[?1006h" #\Escape #\Escape))
        (:all-motion
         (format stream "~C[?1003h~C[?1006h" #\Escape #\Escape)))))

  ;; Focus reporting
  (let ((prev-focus (and prev (view-state-report-focus prev)))
        (cur-focus (view-state-report-focus current)))
    (when (and cur-focus (not prev-focus))
      (format stream "~C[?1004h" #\Escape))
    (when (and prev-focus (not cur-focus))
      (format stream "~C[?1004l" #\Escape)))

  ;; Window title
  (let ((cur-title (view-state-window-title current)))
    (when (and cur-title
               (not (equal cur-title (and prev (view-state-window-title prev)))))
      (format stream "~C]0;~A~C" #\Escape cur-title (code-char 7))))

  ;; Cursor shape
  (let ((cur-cursor (view-state-cursor current))
        (prev-cursor (and prev (view-state-cursor prev))))
    (when cur-cursor
      (when (or (not prev-cursor)
                (not (eq (cursor-shape cur-cursor) (cursor-shape prev-cursor)))
                (not (eq (cursor-blink cur-cursor) (cursor-blink prev-cursor))))
        (let* ((shape-num (ecase (cursor-shape cur-cursor)
                            (:block 0)
                            (:underline 1)
                            (:bar 2)))
               ;; Encode: shape*2 + 1 for blink, +1 more for no-blink
               (encoded (+ (* shape-num 2) 1 (if (cursor-blink cur-cursor) 0 1))))
          (format stream "~C[~D q" #\Escape encoded)))
      ;; Cursor color via OSC 12
      (when (and (cursor-color cur-cursor)
                 (not (equal (cursor-color cur-cursor)
                             (and prev-cursor (cursor-color prev-cursor)))))
        (format stream "~C]12;~A~C\\" #\Escape (cursor-color cur-cursor) #\Escape))))

  ;; Keyboard enhancements (Kitty protocol)
  (let ((prev-ke (and prev (view-state-keyboard-enhancements prev)))
        (cur-ke (view-state-keyboard-enhancements current)))
    (when (and (not (equal prev-ke cur-ke)) cur-ke)
      ;; Push keyboard mode flags
      ;; Flag 1 = disambiguate escape codes, flag 2 = report event types
      (let ((flags 1)) ; always disambiguate
        (when (getf cur-ke :report-event-types)
          (setf flags (logior flags 2)))
        (format stream "~C[>~Du" #\Escape flags)))
    (when (and prev-ke (not cur-ke))
      ;; Pop keyboard mode
      (format stream "~C[<u" #\Escape)))

  ;; Bracketed paste mode
  (let ((prev-dbp (and prev (view-state-disable-bracketed-paste prev)))
        (cur-dbp (view-state-disable-bracketed-paste current)))
    (cond
      ;; disable-bracketed-paste went from nil to T → disable it
      ((and cur-dbp (not prev-dbp))
       (format stream "~C[?2004l" #\Escape))
      ;; disable-bracketed-paste went from T to nil → re-enable it
      ((and prev-dbp (not cur-dbp))
       (format stream "~C[?2004h" #\Escape))))

  ;; Unicode mode (DEC private mode 2027)
  (let ((prev-um (and prev (view-state-unicode-mode prev)))
        (cur-um (view-state-unicode-mode current)))
    (when (and cur-um (not prev-um))
      (format stream "~C[?2027h" #\Escape))
    (when (and prev-um (not cur-um))
      (format stream "~C[?2027l" #\Escape))))

;;; ---------- Command utilities ----------

(defun quit-cmd ()
  "Return a command that quits the program."
  (lambda () (make-quit-msg)))

(defun batch (&rest cmds)
  "Batch multiple commands to run concurrently (filters out NILs)."
  (serapeum:keep #'identity cmds))

(defun cmd-sequence (&rest cmds)
  "Sequence multiple commands to run in order (filters out NILs)."
  (cons :sequence (serapeum:keep #'identity cmds)))

(defun tick (duration &optional (fn nil fn-supplied-p))
  "Create a command that waits for DURATION seconds then produces a message.
   If FN is provided, calls it to produce the message.
   If FN is not provided, returns a tick-msg."
  (lambda ()
    (sleep duration)
    (if fn-supplied-p
        (funcall fn)
        (make-tick-msg))))

;;; ---------- Query commands ----------

(defun request-background-color-cmd ()
  "Return a command that queries the terminal background color.
The terminal responds with a background-color-msg."
  (lambda ()
    ;; OSC 11 query - terminal responds with color
    nil))

(defun request-foreground-color-cmd ()
  "Return a command that queries the terminal foreground color.
The terminal responds with a foreground-color-msg."
  (lambda ()
    ;; OSC 10 query - terminal responds with color
    nil))

(defun request-cursor-color-cmd ()
  "Return a command that queries the terminal cursor color.
The terminal responds with a cursor-color-msg."
  (lambda ()
    ;; OSC 12 query - terminal responds with color
    nil))

(defun request-terminal-version-cmd ()
  "Return a command that queries the terminal version/identification.
The terminal responds with a terminal-version-msg."
  (lambda ()
    ;; XTVERSION query - terminal responds with version string
    nil))

(defun request-mode-report-cmd (mode)
  "Return a command that queries whether DEC private MODE is set.
The terminal responds with a mode-report-msg."
  (declare (ignore mode))
  (lambda ()
    ;; DECRPM query
    nil))

;;; ---------- Clipboard commands ----------

(defun set-clipboard-cmd (content)
  "Return a command that sets the system clipboard via OSC 52."
  (declare (ignore content))
  (lambda () nil))

(defun read-clipboard-cmd ()
  "Return a command that reads the system clipboard via OSC 52.
The terminal responds with a clipboard-msg."
  (lambda () nil))

(defun set-primary-clipboard-cmd (content)
  "Return a command that sets the primary selection via OSC 52."
  (declare (ignore content))
  (lambda () nil))

;;; ---------- Raw escape command ----------

(defun raw-cmd (sequence)
  "Return a command that writes a raw escape SEQUENCE to the terminal.
SEQUENCE is a string of escape codes to write directly."
  (declare (ignore sequence))
  (lambda () nil))

;;; ---------- Key event utilities ----------

(defun keystroke (key-event)
  "Return a human-readable string for a key event.
For example, a Ctrl+C press would return \"ctrl+c\"."
  (let ((code (key-event-code key-event))
        (mod (key-event-mod key-event))
        (parts nil))
    ;; Build modifier prefix
    (when (mod-contains mod +mod-ctrl+)  (push "ctrl" parts))
    (when (mod-contains mod +mod-alt+)   (push "alt" parts))
    (when (mod-contains mod +mod-shift+) (push "shift" parts))
    (when (mod-contains mod +mod-super+) (push "super" parts))
    (when (mod-contains mod +mod-hyper+) (push "hyper" parts))
    (when (mod-contains mod +mod-meta+)  (push "meta" parts))
    ;; Add key name
    (push (cond
            ((characterp code) (string-downcase (string code)))
            ((keywordp code) (string-downcase (symbol-name code)))
            (t (format nil "~A" code)))
          parts)
    (format nil "~{~A~^+~}" (nreverse parts))))

;;; Exec command - run an external program with full TUI suspension
(defstruct (exec-cmd (:constructor make-exec-cmd (program &key args callback)))
  "Command to run an external program with full TUI suspension."
  program
  args
  callback)
