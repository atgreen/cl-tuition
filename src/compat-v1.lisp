;;; compat-v1.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tuition v1 compatibility layer
;;;;
;;;; Provides the old v1 symbol names so existing code can work unchanged
;;;; against v2 internals.  Use package TUITION-V1 (nickname TUI-V1).

(defpackage #:tuition-v1
  (:use #:cl)
  (:nicknames #:tui-v1)
  (:documentation "Tuition v1 compatibility package — old names forwarding to v2.")

  ;; Shadow make-program so we can accept the old v1 keyword args
  (:shadow #:make-program)

  ;; Re-export every public symbol from tuition (except make-program, which is shadowed)
  (:import-from #:tuition . #.(let (syms)
                                 (do-external-symbols (s (find-package :tuition) (nreverse syms))
                                   (unless (string= (symbol-name s) "MAKE-PROGRAM")
                                     (push (make-symbol (symbol-name s)) syms)))))
  (:export . #.(let (syms)
                 (do-external-symbols (s (find-package :tuition) (nreverse syms))
                   (push (make-symbol (symbol-name s)) syms))))

  ;; v1 aliases — additional exports
  (:export
   ;; Class aliases
   #:key-msg
   #:mouse-press-event
   #:mouse-release-event
   #:mouse-drag-event
   #:mouse-move-event
   #:mouse-scroll-event
   #:focus-in-msg
   #:focus-out-msg

   ;; Accessor wrappers
   #:key-msg-key
   #:key-msg-ctrl
   #:key-msg-alt
   #:mouse-event-shift
   #:mouse-event-alt
   #:mouse-event-ctrl
   #:mouse-scroll-direction
   #:mouse-scroll-count

   ;; Predicate wrappers
   #:key-msg-p
   #:mouse-press-event-p
   #:mouse-release-event-p
   #:mouse-drag-event-p
   #:mouse-move-event-p
   #:mouse-scroll-event-p
   #:focus-in-msg-p
   #:focus-out-msg-p

   ;; Constructor wrapper
   #:make-key-msg))

(in-package #:tuition-v1)

;;; ---------- Class aliases ----------
;;;
;;; (setf find-class) makes the v1 symbol name the same CLOS class,
;;; so method specializers and TYPEP work transparently.

(setf (find-class 'key-msg)              (find-class 'tuition:key-press-msg))
(setf (find-class 'mouse-press-event)    (find-class 'tuition:mouse-click-msg))
(setf (find-class 'mouse-release-event)  (find-class 'tuition:mouse-release-msg))
(setf (find-class 'mouse-drag-event)     (find-class 'tuition:mouse-motion-msg))
(setf (find-class 'mouse-move-event)     (find-class 'tuition:mouse-motion-msg))
(setf (find-class 'mouse-scroll-event)   (find-class 'tuition:mouse-wheel-msg))
(setf (find-class 'focus-in-msg)         (find-class 'tuition:focus-msg))
(setf (find-class 'focus-out-msg)        (find-class 'tuition:blur-msg))

;;; ---------- make-program v1 shim ----------
;;;
;;; In v1, make-program accepted :alt-screen and :mouse.  In v2 those
;;; moved to the view-state returned by VIEW.  We stash them in the
;;; program's options plist and an :around method on VIEW auto-wraps
;;; plain-string returns into a view-state with the right settings.

(defun make-program (model &key alt-screen mouse (pool-size 4))
  "V1 compat: create a program, accepting the old :alt-screen and :mouse options.
When the VIEW method returns a plain string, these options are applied
automatically via a view-state wrapper."
  (let ((prog (tuition:make-program model :pool-size pool-size)))
    (when (or alt-screen mouse)
      (setf (tuition::program-options prog)
            (append (list :v1-alt-screen alt-screen
                          :v1-mouse mouse)
                    (tuition::program-options prog))))
    prog))

(defmethod tuition:view :around ((model t))
  "V1 compat: when the current program has v1 terminal options and VIEW returns
a plain string, wrap it in a view-state with the appropriate settings."
  (let ((result (call-next-method)))
    (if (and (stringp result)
             tuition:*current-program*)
        (let* ((opts (tuition::program-options tuition:*current-program*))
               (alt-screen (getf opts :v1-alt-screen))
               (mouse (getf opts :v1-mouse)))
          (if (or alt-screen mouse)
              (tuition:make-view result
                                :alt-screen alt-screen
                                :mouse-mode (when mouse :all-motion))
              result))
        result)))

;;; ---------- Accessor wrappers ----------

(defun key-msg-key (msg)
  "V1 compat: return the key code from a key event."
  (tuition:key-event-code msg))

(defun key-msg-ctrl (msg)
  "V1 compat: return T if Ctrl was held during a key event."
  (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-ctrl+))

(defun key-msg-alt (msg)
  "V1 compat: return T if Alt was held during a key event."
  (tuition:mod-contains (tuition:key-event-mod msg) tuition:+mod-alt+))

(defun mouse-event-shift (msg)
  "V1 compat: return T if Shift was held during a mouse event."
  (tuition:mod-contains (tuition:mouse-event-mod msg) tuition:+mod-shift+))

(defun mouse-event-alt (msg)
  "V1 compat: return T if Alt was held during a mouse event."
  (tuition:mod-contains (tuition:mouse-event-mod msg) tuition:+mod-alt+))

(defun mouse-event-ctrl (msg)
  "V1 compat: return T if Ctrl was held during a mouse event."
  (tuition:mod-contains (tuition:mouse-event-mod msg) tuition:+mod-ctrl+))

(defun mouse-scroll-direction (msg)
  "V1 compat: return the scroll direction from a mouse wheel event."
  (tuition:mouse-wheel-direction msg))

(defun mouse-scroll-count (msg)
  "V1 compat: return the scroll count from a mouse wheel event."
  (tuition:mouse-wheel-count msg))

;;; ---------- Predicate wrappers ----------

(defun key-msg-p (obj)
  "V1 compat: return T if OBJ is a key-press-msg."
  (typep obj 'tuition:key-press-msg))

(defun mouse-press-event-p (obj)
  "V1 compat: return T if OBJ is a mouse-click-msg."
  (tuition:mouse-click-msg-p obj))

(defun mouse-release-event-p (obj)
  "V1 compat: return T if OBJ is a mouse-release-msg."
  (tuition:mouse-release-msg-p obj))

(defun mouse-drag-event-p (obj)
  "V1 compat: return T if OBJ is a mouse-motion-msg."
  (tuition:mouse-motion-msg-p obj))

(defun mouse-move-event-p (obj)
  "V1 compat: return T if OBJ is a mouse-motion-msg."
  (tuition:mouse-motion-msg-p obj))

(defun mouse-scroll-event-p (obj)
  "V1 compat: return T if OBJ is a mouse-wheel-msg."
  (tuition:mouse-wheel-msg-p obj))

(defun focus-in-msg-p (obj)
  "V1 compat: return T if OBJ is a focus-msg."
  (typep obj 'tuition:focus-msg))

(defun focus-out-msg-p (obj)
  "V1 compat: return T if OBJ is a blur-msg."
  (typep obj 'tuition:blur-msg))

;;; ---------- Constructor wrapper ----------

(defun make-key-msg (&key key ctrl alt)
  "V1 compat: construct a key-press-msg from KEY, CTRL, and ALT booleans."
  (tuition:make-key-press-msg
   :code key
   :mod (tuition:make-mod :ctrl ctrl :alt alt)))
