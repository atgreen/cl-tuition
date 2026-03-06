;;; keybinding.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Key binding system with help text generation

(in-package #:tuition)

;;; Key binding definition
(defclass keybinding ()
  ((keys :initarg :keys
         :accessor keybinding-keys
         :documentation "List of key specifications that trigger this binding")
   (help-key :initarg :help-key
             :accessor keybinding-help-key
             :documentation "Short help text for the key (e.g., \"↑/k\")")
   (help-desc :initarg :help-desc
              :accessor keybinding-help-desc
              :documentation "Description of the action (e.g., \"move up\")")
   (enabled :initarg :enabled
            :initform t
            :accessor keybinding-enabled-p
            :documentation "Whether this binding is currently enabled"))
  (:documentation "A key binding with associated help text."))

(defun make-keybinding (&key keys help-key help-desc (enabled t))
  "Create a new keybinding.

Parameters:
- KEYS: A list of key specifications. Each spec is either:
  - A character (e.g., #\\k) - matches that character with no modifiers
  - A keyword (e.g., :up) - matches that key code with no modifiers
  - A list (key &key shift alt ctrl) for modified keys
- HELP-KEY: Short help text for the key (e.g., \"↑/k\")
- HELP-DESC: Description of the action (e.g., \"move up\")
- ENABLED: Whether the binding is enabled (default t)"
  (make-instance 'keybinding
                 :keys (if (listp keys) keys (list keys))
                 :help-key help-key
                 :help-desc help-desc
                 :enabled enabled))

(defun keybinding-enable (binding)
  "Enable a keybinding."
  (setf (keybinding-enabled-p binding) t))

(defun keybinding-disable (binding)
  "Disable a keybinding."
  (setf (keybinding-enabled-p binding) nil))

(defun keybinding-matches (binding msg)
  "Check if a key event matches this binding.
Returns T if the message matches any of the binding's keys and the binding is enabled."
  (when (and (keybinding-enabled-p binding)
             (typep msg 'key-event))
    (let ((msg-code (key-event-code msg))
          (msg-mod (key-event-mod msg)))
      (some (lambda (key-spec)
              (match-key-spec key-spec msg-code msg-mod))
            (keybinding-keys binding)))))

(defun match-key-spec (spec code mod)
  "Check if a key specification matches the given code and modifier bitmask.
Spec can be:
- A character or keyword (exact match, no modifiers)
- A list (key &key shift alt ctrl) for modified keys"
  (cond
    ;; Simple spec: character or keyword, must match with no modifiers
    ((or (characterp spec) (keywordp spec))
     (and (equal spec code)
          (= mod 0)))

    ;; Complex spec: (key &key shift alt ctrl)
    ((listp spec)
     (let ((spec-key (first spec))
           (spec-mod (make-mod :shift (getf (rest spec) :shift)
                               :alt (getf (rest spec) :alt)
                               :ctrl (getf (rest spec) :ctrl))))
       (and (equal spec-key code)
            (= spec-mod mod))))

    (t nil)))

;;; Key binding help generation

(defun keybinding-help-line (binding)
  "Generate a single-line help string for a keybinding."
  (when (keybinding-enabled-p binding)
    (format nil "~A    ~A"
            (keybinding-help-key binding)
            (keybinding-help-desc binding))))

(defun keybindings-help (bindings &key (separator "  ") (format :inline))
  "Generate help text from a list of keybindings."
  (let ((enabled-bindings (remove-if-not #'keybinding-enabled-p bindings)))
    (case format
      (:inline
       (let ((help-strings (mapcar (lambda (b)
                                     (format nil "~A ~A"
                                             (keybinding-help-key b)
                                             (keybinding-help-desc b)))
                                   enabled-bindings)))
         (format nil "~{~A~^~A~}" help-strings separator)))

      (:full
       (when enabled-bindings
         (let* ((max-key-len (reduce #'max enabled-bindings
                                     :key (lambda (b)
                                            (length (keybinding-help-key b)))))
                (format-str (format nil "~~A~~~DA  ~~A" (+ max-key-len 2))))
           (format nil "~{~A~^~%~}"
                   (mapcar (lambda (b)
                             (format nil format-str
                                     (keybinding-help-key b)
                                     ""
                                     (keybinding-help-desc b)))
                           enabled-bindings)))))

      (t (keybindings-help bindings :separator separator :format :inline)))))

;;; Common key binding presets

(defun make-quit-keybinding ()
  "Standard quit keybinding (q, ctrl+c, esc)."
  (make-keybinding
   :keys (list #\q '(#\c :ctrl t) :escape)
   :help-key "q/esc"
   :help-desc "quit"))

(defun make-navigation-keybindings ()
  "Standard navigation keybindings.
Returns a plist: (:up binding :down binding :left binding :right binding)."
  (list :up (make-keybinding
             :keys (list #\k :up)
             :help-key "↑/k"
             :help-desc "move up")
        :down (make-keybinding
               :keys (list #\j :down)
               :help-key "↓/j"
               :help-desc "move down")
        :left (make-keybinding
               :keys (list #\h :left)
               :help-key "←/h"
               :help-desc "move left")
        :right (make-keybinding
                :keys (list #\l :right)
                :help-key "→/l"
                :help-desc "move right")))

(defun make-selection-keybindings ()
  "Standard selection keybindings."
  (list :enter (make-keybinding
                :keys (list :enter #\Return)
                :help-key "enter"
                :help-desc "select")
        :space (make-keybinding
                :keys (list #\Space)
                :help-key "space"
                :help-desc "toggle")))

(defun make-scroll-keybindings ()
  "Standard scroll keybindings."
  (list :page-up (make-keybinding
                  :keys (list #\u :page-up '(#\u :ctrl t))
                  :help-key "pgup/u"
                  :help-desc "page up")
        :page-down (make-keybinding
                    :keys (list #\d :page-down '(#\d :ctrl t))
                    :help-key "pgdn/d"
                    :help-desc "page down")
        :home (make-keybinding
               :keys (list :home #\g)
               :help-key "home/g"
               :help-desc "go to top")
        :end (make-keybinding
              :keys (list :end #\G)
              :help-key "end/G"
              :help-desc "go to bottom")))

;;; Convenience functions for matching messages

(defun key-matches (msg &rest key-specs)
  "Check if a key event matches any of the given key specifications.
This is a convenience function for simple key matching without creating a binding."
  (when (typep msg 'key-event)
    (let ((msg-code (key-event-code msg))
          (msg-mod (key-event-mod msg)))
      (some (lambda (spec)
              (match-key-spec spec msg-code msg-mod))
            key-specs))))
