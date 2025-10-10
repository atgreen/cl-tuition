;;; showcase.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Fancy showcase example for screenshots - demonstrates multiple Tuition features

(defpackage #:tuition-example-showcase
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-showcase)

;;; Model
(defclass showcase-model ()
  ((progress :initform 0.0 :accessor model-progress)
   (tick-count :initform 0 :accessor model-tick-count)
   (status :initform "Initializing..." :accessor model-status)
   (running :initform t :accessor model-running)))

;;; Custom tick message
(tui:defmessage tick-msg
  ())

;;; TEA Protocol
(defmethod tui:init ((model showcase-model))
  ;; Start ticking every 100ms
  (lambda ()
    (sleep 0.1)
    (make-instance 'tick-msg)))

(defmethod tui:update-message ((model showcase-model) (msg tick-msg))
  (incf (model-tick-count model))

  ;; Update progress
  (let ((new-progress (min 1.0 (+ (model-progress model) 0.01))))
    (setf (model-progress model) new-progress)

    ;; Update status based on progress
    (setf (model-status model)
          (cond
            ((< new-progress 0.25) "Loading resources...")
            ((< new-progress 0.50) "Compiling shaders...")
            ((< new-progress 0.75) "Building world...")
            ((< new-progress 1.0) "Finalizing...")
            (t "Complete!")))

    ;; Stop after complete
    (when (>= new-progress 1.0)
      (setf (model-running model) nil)))

  (values model
          (if (model-running model)
              ;; Keep ticking
              (lambda ()
                (sleep 0.1)
                (make-instance 'tick-msg))
              nil)))

(defmethod tui:update-message ((model showcase-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (if (and (characterp key) (char= key #\q))
        (values model (tui:quit-cmd))
        (values model nil))))

(defmethod tui:view ((model showcase-model))
  (let* ((width 70)
         (progress (model-progress model))

         ;; Title with border
         (title-style (tui:make-style
                       :foreground tui:*fg-bright-magenta*
                       :bold t
                       :align :center
                       :width width))
         (title (tui:render-styled title-style "🎨 TUITION SHOWCASE"))
         (title-box (tui:render-border title tui:*border-rounded*
                                      :fg-color tui:*fg-bright-magenta*))

         ;; Subtitle
         (subtitle-style (tui:make-style
                          :foreground tui:*fg-bright-cyan*
                          :italic t
                          :align :center
                          :width width))
         (subtitle (tui:render-styled subtitle-style
                                     "A Common Lisp TUI Framework"))

         ;; Progress section
         (progress-label-style (tui:make-style
                                :foreground tui:*fg-bright-yellow*
                                :bold t))
         (progress-label (tui:render-styled progress-label-style
                                           (format nil "Progress: ~D%"
                                                  (round (* progress 100)))))

         (progress-bar-width (- width 4))
         (filled-width (round (* progress progress-bar-width)))
         (empty-width (- progress-bar-width filled-width))
         (bar-filled (tui:render-styled
                      (tui:make-style :background tui:*bg-bright-green*)
                      (make-string filled-width :initial-element #\Space)))
         (bar-empty (tui:render-styled
                     (tui:make-style :background tui:*bg-bright-black*)
                     (make-string empty-width :initial-element #\Space)))
         (progress-bar (format nil "  ~A~A" bar-filled bar-empty))

         ;; Status
         (status-style (tui:make-style
                        :foreground tui:*fg-bright-white*
                        :align :center
                        :width width))
         (status-text (tui:render-styled status-style (model-status model)))

         ;; Feature list
         (feature-style (tui:make-style :foreground tui:*fg-green*))
         (features (list
                    (tui:render-styled feature-style "✓ The Elm Architecture (TEA)")
                    (tui:render-styled feature-style "✓ CLOS-based Messages")
                    (tui:render-styled feature-style "✓ Terminal Styling & Colors")
                    (tui:render-styled feature-style "✓ Borders & Layout")
                    (tui:render-styled feature-style "✓ Mouse Support")
                    (tui:render-styled feature-style "✓ Spring Physics")
                    (tui:render-styled feature-style "✓ Markdown Rendering")))

         ;; Info box
         (info-title-style (tui:make-style
                            :foreground tui:*fg-bright-blue*
                            :bold t))
         (info-title (tui:render-styled info-title-style "Features:"))
         (feature-list (format nil "~{  ~A~^~%~}" features))
         (info-content (format nil "~A~%~A" info-title feature-list))
         (info-box (tui:render-border info-content tui:*border-normal*
                                     :fg-color tui:*fg-blue*))

         ;; Stats
         (stats-style (tui:make-style
                       :foreground tui:*fg-bright-black*
                       :align :center
                       :width width))
         (stats (tui:render-styled stats-style
                                  (format nil "Ticks: ~D" (model-tick-count model))))

         ;; Footer
         (footer-style (tui:make-style
                        :foreground tui:*fg-bright-black*
                        :italic t
                        :align :center
                        :width width))
         (footer (tui:render-styled footer-style "Press 'q' to quit")))

    ;; Assemble everything
    (tui:join-vertical tui:+center+
                      ""
                      title-box
                      subtitle
                      ""
                      progress-label
                      progress-bar
                      status-text
                      ""
                      info-box
                      ""
                      stats
                      ""
                      footer
                      "")))

;;; Entry point
(defun main ()
  "Run the showcase example."
  (let* ((model (make-instance 'showcase-model))
         (program (tui:make-program model :alt-screen t)))
    (tui:run program)))

#+nil
(main)
