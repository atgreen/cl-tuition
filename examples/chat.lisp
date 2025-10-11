;;; chat.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; A simple chat demo demonstrating the textarea and viewport components
;;; working together. Port of the BubbleTea chat example.

(asdf:load-system :tuition)

(defpackage #:chat-demo
  (:use #:cl))

(in-package #:chat-demo)

;;; Model definition
(defclass chat-model ()
  ((viewport :initform nil :accessor viewport)
   (textarea :initform nil :accessor textarea)
   (messages :initform '() :accessor messages)
   (sender-style :initform nil :accessor sender-style)
   (term-width :initform 80 :accessor term-width)
   (term-height :initform 24 :accessor term-height)))

;;; Initialize the model
(defmethod tui:init ((model chat-model))
  "Initialize the chat model."
  ;; Create and configure textarea for input
  (let ((ta (tui.textarea:make-textarea
             :width 30
             :height 3
             :placeholder "Send a message...")))
    (setf (tui.textarea:textarea-prompt ta) "┃ ")
    (setf (tui.textarea:textarea-char-limit ta) 280)
    (setf (tui.textarea:textarea-show-line-numbers ta) nil)
    (tui.textarea:textarea-focus ta)
    (setf (textarea model) ta))

  ;; Create viewport for message history
  (setf (viewport model) (tui.viewport:make-viewport
                          :width 30
                          :height 5
                          :content (format nil "Welcome to the chat room!~%Type a message and press Enter to send.")))

  ;; Create sender style (magenta color)
  (setf (sender-style model) (tui:make-style :foreground tui:*fg-magenta*))

  ;; Return tick command for cursor blink
  (tui:tick 0.5))

;;; Update function
(defmethod tui:update ((model chat-model) msg)
  "Handle messages and update the model."
  (let ((ta-cmd nil)
        (vp-cmd nil)
        (pass-to-textarea t))

    ;; Handle specific message types first
    (cond
      ;; Key press - handle before passing to textarea
      ((tui:key-msg-p msg)
       (let ((key (tui:key-msg-key msg))
             (ctrl (tui:key-msg-ctrl msg)))
         (cond
           ;; Quit on Ctrl+C or Esc
           ((or (and ctrl (characterp key) (char= key #\c))
                (eq key :escape))
            (return-from tui:update (values model (tui:quit-cmd))))

           ;; Send message on Enter
           ((eq key :enter)
            (let ((text (tui.textarea:textarea-value (textarea model))))
              (when (and text (> (length (string-trim '(#\Space #\Tab #\Newline) text)) 0))
                ;; Add message to list with colored "You:" prefix
                (let ((prefix (tui:render-styled (sender-style model) "You: ")))
                  (push (format nil "~A~A" prefix text)
                        (messages model)))

                ;; Update viewport content
                (tui.viewport:viewport-set-content
                 (viewport model)
                 (format nil "~{~A~^~%~}" (reverse (messages model))))

                ;; Reset textarea
                (tui.textarea:textarea-reset (textarea model))

                ;; Scroll to bottom
                (tui.viewport:viewport-goto-bottom (viewport model))))
            ;; Don't pass Enter to textarea
            (setf pass-to-textarea nil)))))

      ;; Window resize
      ((tui:window-size-msg-p msg)
       (let ((width (tui:window-size-msg-width msg))
             (height (tui:window-size-msg-height msg)))
         (setf (term-width model) width)
         (setf (term-height model) height)

         ;; Resize viewport and textarea
         (setf (tui.viewport:viewport-width (viewport model)) width)
         (setf (tui.textarea:textarea-width (textarea model)) width)

         ;; Calculate viewport height (total - textarea height - gap)
         (let* ((textarea-height (tui.textarea:textarea-height (textarea model)))
                (gap-height 2)  ; "\n\n" = 2 lines
                (viewport-height (- height textarea-height gap-height)))
           (setf (tui.viewport:viewport-height (viewport model)) viewport-height))

         ;; Re-render messages with new width if we have any
         (when (messages model)
           (tui.viewport:viewport-set-content
            (viewport model)
            (format nil "~{~A~^~%~}" (reverse (messages model)))))

         ;; Scroll to bottom
         (tui.viewport:viewport-goto-bottom (viewport model))))

      ;; Tick for cursor blink
      ((tui:tick-msg-p msg)
       (setf ta-cmd (tui:tick 0.5))))

    ;; Update textarea and viewport with the message (unless we handled it above)
    (when pass-to-textarea
      (multiple-value-bind (new-ta ta-command)
          (tui.textarea:textarea-update (textarea model) msg)
        (setf (textarea model) new-ta)
        (setf ta-cmd ta-command)))

    (multiple-value-bind (new-vp vp-command)
        (tui.viewport:viewport-update (viewport model) msg)
      (setf (viewport model) new-vp)
      (setf vp-cmd vp-command))

    ;; Return updated model and batch commands
    (values model (tui:batch ta-cmd vp-cmd))))

;;; View function
(defmethod tui:view ((model chat-model))
  "Render the chat interface."
  (let ((vp-view (tui.viewport:viewport-view (viewport model)))
        (ta-view (tui.textarea:textarea-view (textarea model)))
        (gap (format nil "~%~%")))
    (format nil "~A~A~A" vp-view gap ta-view)))

;;; Main entry point
(defun main ()
  "Run the chat demo."
  (let ((model (make-instance 'chat-model)))
    (tui:run (tui:make-program model :alt-screen nil :mouse nil))))

;;; Run automatically when loaded
(main)
