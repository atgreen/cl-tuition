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
  - A character (e.g., #\\k)
  - A keyword (e.g., :up)
  - A list (key &key alt ctrl) for modified keys (e.g., (#\\c :ctrl t))
- HELP-KEY: Short help text for the key (e.g., \"↑/k\")
- HELP-DESC: Description of the action (e.g., \"move up\")
- ENABLED: Whether the binding is enabled (default t)

Example:
  (make-keybinding
    :keys '(#\\k :up)
    :help-key \"↑/k\"
    :help-desc \"move up\")"
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
  "Check if a key message matches this binding.
Returns T if the message matches any of the binding's keys and the binding is enabled."
  (when (and (keybinding-enabled-p binding)
             (typep msg 'key-msg))
    (let ((msg-key (key-msg-key msg))
          (msg-alt (key-msg-alt msg))
          (msg-ctrl (key-msg-ctrl msg)))
      (some (lambda (key-spec)
              (match-key-spec key-spec msg-key msg-alt msg-ctrl))
            (keybinding-keys binding)))))

(defun match-key-spec (spec key alt ctrl)
  "Check if a key specification matches the given key and modifiers.
Spec can be:
- A character or keyword (exact match, no modifiers)
- A list (key &key alt ctrl) for modified keys"
  (cond
    ;; Simple spec: character or keyword
    ((or (characterp spec) (keywordp spec))
     (and (equal spec key)
          (not alt)
          (not ctrl)))

    ;; Complex spec: (key &key alt ctrl)
    ((listp spec)
     (let ((spec-key (first spec))
           (spec-alt (getf (rest spec) :alt))
           (spec-ctrl (getf (rest spec) :ctrl)))
       (and (equal spec-key key)
            (eq (not (null spec-alt)) (not (null alt)))
            (eq (not (null spec-ctrl)) (not (null ctrl))))))

    (t nil)))

;;; Key binding help generation

(defun keybinding-help-line (binding)
  "Generate a single-line help string for a keybinding.
Returns a string in the format \"key    description\" if enabled, nil otherwise."
  (when (keybinding-enabled-p binding)
    (format nil "~A    ~A"
            (keybinding-help-key binding)
            (keybinding-help-desc binding))))

(defun keybindings-help (bindings &key (separator "  ") (format :inline))
  "Generate help text from a list of keybindings.

Parameters:
- BINDINGS: List of keybinding objects
- SEPARATOR: String to separate multiple bindings (default \"  \")
- FORMAT: One of :inline (single line) or :full (multi-line)

Returns a formatted help string showing all enabled bindings."
  (let ((enabled-bindings (remove-if-not #'keybinding-enabled-p bindings)))
    (case format
      (:inline
       ;; Single line: "key desc • key desc • key desc"
       (let ((help-strings (mapcar (lambda (b)
                                     (format nil "~A ~A"
                                             (keybinding-help-key b)
                                             (keybinding-help-desc b)))
                                   enabled-bindings)))
         (format nil "~{~A~^~A~}" help-strings separator)))

      (:full
       ;; Multi-line with alignment
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
  "Standard selection keybindings.
Returns a plist: (:enter binding :space binding)."
  (list :enter (make-keybinding
                :keys (list :enter #\Return)
                :help-key "enter"
                :help-desc "select")
        :space (make-keybinding
                :keys (list #\Space)
                :help-key "space"
                :help-desc "toggle")))

(defun make-scroll-keybindings ()
  "Standard scroll keybindings (page up/down, home/end).
Returns a plist: (:page-up binding :page-down binding :home binding :end binding)."
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
  "Check if a key message matches any of the given key specifications.
This is a convenience function for simple key matching without creating a binding.

Example:
  (key-matches msg #\\q :escape (#\\c :ctrl t))"
  (when (typep msg 'key-msg)
    (let ((msg-key (key-msg-key msg))
          (msg-alt (key-msg-alt msg))
          (msg-ctrl (key-msg-ctrl msg)))
      (some (lambda (spec)
              (match-key-spec spec msg-key msg-alt msg-ctrl))
            key-specs))))
