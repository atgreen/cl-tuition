#!/usr/bin/env -S sbcl --script
;;; keybindings.lisp - Demonstrate the key binding system with help text
;;;
;;; This example shows how to use the keybinding system to create
;;; structured key handlers with auto-generated help text.

(asdf:load-system :tuition)

(defpackage #:keybindings-example
  (:use #:cl #:tuition))

(in-package #:keybindings-example)

;;; Model with keybindings
(defclass demo-model ()
  ((selected :initform 0 :accessor selected)
   (items :initform '("Apple" "Banana" "Cherry" "Date" "Elderberry")
          :accessor items)
   (mode :initform :normal :accessor mode)  ; :normal or :edit
   (keybindings :initform nil :accessor keybindings)))

(defmethod init ((model demo-model))
  ;; Initialize keybindings
  (let ((nav-bindings (make-navigation-keybindings)))
    (setf (keybindings model)
          (list :quit (make-quit-keybinding)
                :up (getf nav-bindings :up)
                :down (getf nav-bindings :down)
                :help (make-keybinding
                       :keys '(#\? #\h)
                       :help-key "?/h"
                       :help-desc "show help")
                :select (make-keybinding
                         :keys '(:enter)
                         :help-key "enter"
                         :help-desc "select item")
                :edit (make-keybinding
                       :keys '(#\e)
                       :help-key "e"
                       :help-desc "edit mode"
                       :enabled nil))))  ; disabled initially
  nil)

(defmethod update ((model demo-model) msg)
  (let ((bindings (keybindings model)))
    (cond
      ;; Check each keybinding
      ((keybinding-matches (getf bindings :quit) msg)
       (values model (quit-cmd)))

      ((keybinding-matches (getf bindings :up) msg)
       (setf (selected model)
             (mod (1- (selected model)) (length (items model))))
       (values model nil))

      ((keybinding-matches (getf bindings :down) msg)
       (setf (selected model)
             (mod (1+ (selected model)) (length (items model))))
       (values model nil))

      ((keybinding-matches (getf bindings :select) msg)
       ;; Select toggles edit mode
       (setf (mode model)
             (if (eq (mode model) :normal) :edit :normal))
       ;; Enable/disable edit keybinding based on mode
       (if (eq (mode model) :edit)
           (keybinding-enable (getf bindings :edit))
           (keybinding-disable (getf bindings :edit)))
       (values model nil))

      ((keybinding-matches (getf bindings :help) msg)
       ;; Toggle help visibility by cycling through modes
       (setf (mode model)
             (case (mode model)
               (:normal :help)
               (:help :normal)
               (t :normal)))
       (values model nil))

      (t (values model nil)))))

(defmethod view ((model demo-model))
  (let* ((bindings (keybindings model))
         (items (items model))
         (sel (selected model))
         (mode (mode model))
         ;; Generate help text from bindings
         (help-text (keybindings-help (list (getf bindings :quit)
                                            (getf bindings :up)
                                            (getf bindings :down)
                                            (getf bindings :select)
                                            (getf bindings :help))
                                      :separator " • "
                                      :format :inline)))
    (with-output-to-string (s)
      ;; Title
      (format s "~A~%~%"
              (colored (bold "Key Binding System Demo") :fg *fg-cyan*))

      ;; Status bar
      (format s "Mode: ~A~%~%"
              (colored (format nil "~A" mode)
                      :fg (if (eq mode :edit) *fg-green* *fg-yellow*)))

      ;; Items list
      (format s "Select an item:~%")
      (loop for item in items
            for i from 0
            do (format s "  ~A ~A~%"
                      (if (= i sel)
                          (colored "►" :fg *fg-green*)
                          " ")
                      (if (= i sel)
                          (bold item)
                          item)))

      ;; Help section
      (format s "~%")
      (if (eq mode :help)
          ;; Full help
          (progn
            (format s "~A~%~%"
                    (colored (bold "Available Keys:") :fg *fg-bright-blue*))
            (format s "~A~%"
                    (keybindings-help (list (getf bindings :quit)
                                            (getf bindings :up)
                                            (getf bindings :down)
                                            (getf bindings :select)
                                            (getf bindings :help)
                                            (getf bindings :edit))
                                      :format :full)))
          ;; Inline help
          (format s "~A: ~A~%"
                  (colored "Keys" :fg *fg-bright-black*)
                  (colored help-text :fg *fg-bright-black*))))))

;;; Run the program
(defun main ()
  (let* ((model (make-instance 'demo-model))
         (program (make-program model)))
    (run program)))

(main)
