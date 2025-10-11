;;; Interactive Showcase Example
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; This example demonstrates interactive layouts with mouse zones.
;;; It features clickable tabs, toggleable lists, and clickable dialogs.
;;; Ported from bubblezone's full-lipgloss example.

(asdf:load-system :tuition)

(defpackage :tuition.examples.showcase-interactive
  (:use :cl)
  (:local-nicknames (#:tui #:tuition)))

(in-package :tuition.examples.showcase-interactive)

;;; Color definitions
(defparameter *subtle* (tui:parse-hex-color "#383838"))
(defparameter *highlight* (tui:parse-hex-color "#7D56F4"))
(defparameter *special* (tui:parse-hex-color "#73F59F"))

;;; Tabs Model
(defclass tabs-model ()
  ((id :initarg :id :accessor tabs-id)
   (height :initform 3 :accessor tabs-height)
   (width :initform 0 :accessor tabs-width)
   (active :initform "Tuition" :accessor tabs-active)
   (items :initform '("Tuition" "Styles" "Layouts" "Colors" "Borders" "History") :accessor tabs-items)))

(defmethod tui:init ((model tabs-model))
  nil)

(defmethod tui:update-message ((model tabs-model) (msg tui:window-size-msg))
  (setf (tabs-width model) (tui:window-size-msg-width msg))
  (values model nil))

(defmethod tui:view ((model tabs-model))
  (let* ((active-tab-border (tui:make-border :top "─" :bottom " "
                                             :left "│" :right "│"
                                             :top-left "╭" :top-right "╮"
                                             :bottom-left "┘" :bottom-right "└"))
         (tab-border (tui:make-border :top "─" :bottom "─"
                                     :left "│" :right "│"
                                     :top-left "╭" :top-right "╮"
                                     :bottom-left "┴" :bottom-right "┴"))
         (rendered-tabs
          (loop for item in (tabs-items model)
                collect (let* ((zone-id (concatenate 'string (tabs-id model) item))
                              (border (if (string= item (tabs-active model))
                                         active-tab-border
                                         tab-border))
                              (rendered (tui:render-border (concatenate 'string " " item " ")
                                                          border
                                                          :fg-color *highlight*)))
                         (tui:zone-mark zone-id rendered))))
         (row (apply #'tui:join-horizontal tui:+top+ rendered-tabs))
         (gap-width (max 0 (- (tabs-width model) (tui:width row) 2)))
         (gap-border (tui:make-border :top "" :bottom "─"
                                     :left "" :right ""
                                     :top-left "" :top-right ""
                                     :bottom-left "" :bottom-right ""))
         (gap (tui:render-border (make-string gap-width :initial-element #\Space)
                                gap-border :top nil :left nil :right nil
                                :fg-color *highlight*)))
    (tui:join-horizontal tui:+bottom+ row gap)))

;;; Dialog Model
(defclass dialog-model ()
  ((id :initarg :id :accessor dialog-id)
   (height :initform 8 :accessor dialog-height)
   (width :initform 0 :accessor dialog-width)
   (active :initform "confirm" :accessor dialog-active)
   (question :initform "Are you sure you want to eat marmalade?" :accessor dialog-question)))

(defmethod tui:init ((model dialog-model))
  nil)

(defmethod tui:update-message ((model dialog-model) (msg tui:window-size-msg))
  (setf (dialog-width model) (tui:window-size-msg-width msg))
  (values model nil))

(defmethod tui:view ((model dialog-model))
  (let* ((active-button-style (tui:make-style :foreground (tui:parse-hex-color "#FFF7DB")
                                              :background (tui:parse-hex-color "#F25D94")
                                              :padding-left 3
                                              :padding-right 3
                                              :margin-top 1
                                              :margin-right 2
                                              :underline t))
         (button-style (tui:make-style :foreground (tui:parse-hex-color "#FFF7DB")
                                       :background (tui:parse-hex-color "#888B7E")
                                       :padding-left 3
                                       :padding-right 3
                                       :margin-top 1
                                       :margin-right 2))
         (ok-button (tui:zone-mark (concatenate 'string (dialog-id model) "confirm")
                                  (tui:render-styled (if (string= (dialog-active model) "confirm")
                                                        active-button-style
                                                        button-style)
                                                    "Yes")))
         (cancel-button (tui:zone-mark (concatenate 'string (dialog-id model) "cancel")
                                      (tui:render-styled (if (string= (dialog-active model) "cancel")
                                                            active-button-style
                                                            button-style)
                                                        "Maybe")))
         (question-style (tui:make-style :width 27 :align tui:+center+))
         (question (tui:render-styled question-style (dialog-question model)))
         (buttons (tui:join-horizontal tui:+top+ ok-button cancel-button))
         (ui (tui:join-vertical tui:+center+ question buttons)))
    (tui:render-border ui tui:*border-rounded*
                      :fg-color (tui:parse-hex-color "#874BFD"))))

;;; List Item
(defclass list-item ()
  ((name :initarg :name :accessor item-name)
   (done :initarg :done :accessor item-done)))

;;; List Model
(defclass list-model ()
  ((id :initarg :id :accessor list-id)
   (height :initform 8 :accessor list-height)
   (width :initform 0 :accessor list-width)
   (title :initarg :title :accessor list-title)
   (items :initarg :items :accessor list-items)))

(defmethod tui:init ((model list-model))
  nil)

(defmethod tui:update-message ((model list-model) (msg tui:window-size-msg))
  (setf (list-width model) (tui:window-size-msg-width msg))
  (values model nil))

(defmethod tui:view ((model list-model))
  (let* ((column-width 30)  ; Fixed width like lipgloss
         (header-border (tui:make-border :bottom "─" :top "" :left "" :right ""
                                        :top-left "" :top-right "" :bottom-left "" :bottom-right ""))
         (header (tui:render-border (list-title model) header-border
                                   :top nil :left nil :right nil
                                   :fg-color *subtle*))
         (checkmark (tui:colored "✓ " :fg *special*))
         (item-style (tui:make-style :padding-left 2))
         (done-style (tui:make-style :foreground (tui:parse-hex-color "#696969")
                                    :strikethrough t))
         (rendered-items
          (cons header
                (loop for item in (list-items model)
                      collect (let ((zone-id (concatenate 'string (list-id model) (item-name item))))
                               (tui:zone-mark zone-id
                                             (if (item-done item)
                                                 (concatenate 'string checkmark
                                                            (tui:render-styled done-style (item-name item)))
                                                 (tui:render-styled item-style (item-name item))))))))
         (list-border (tui:make-border :right "│" :top "" :bottom "" :left ""
                                      :top-left "" :top-right "" :bottom-left "" :bottom-right ""))
         (list-style (tui:make-style :width (+ column-width 1)
                                     :height (list-height model)
                                     :margin-right 2))
         (content (apply #'tui:join-vertical tui:+left+ rendered-items)))
    (tui:render-styled list-style
                      (tui:render-border content list-border
                                        :top nil :bottom nil :left nil
                                        :fg-color *subtle*))))

;;; History Model
(defclass history-model ()
  ((id :initarg :id :accessor history-id)
   (height :initform 0 :accessor history-height)
   (width :initform 0 :accessor history-width)
   (active :initform "" :accessor history-active)
   (items :initarg :items :accessor history-items)))

(defmethod tui:init ((model history-model))
  nil)

(defmethod tui:update-message ((model history-model) (msg tui:window-size-msg))
  (setf (history-height model) (tui:window-size-msg-height msg))
  (setf (history-width model) (tui:window-size-msg-width msg))
  (values model nil))

(defmethod tui:view ((model history-model))
  (let* ((num-items (length (history-items model)))
         (item-width (if (> num-items 0)
                        (max 1 (- (floor (history-width model) num-items) 2))
                        1))
         (item-height (max 1 (- (history-height model) 2))))
    (let ((rendered-items
           (loop for item in (history-items model)
                 for i from 0
                 for bg = (if (string= item (history-active model))
                             *highlight*
                             *subtle*)
                 collect (let* ((content-width (max 1 (- item-width 4)))
                                (content-height (max 1 (- item-height 2)))
                                (wrapped-text (tui:wrap-text item content-width :break-words t))
                                ;; Truncate to fit height
                                (lines (tui:split-string-by-newline wrapped-text))
                                (truncated-lines (if (> (length lines) content-height)
                                                    (append (subseq lines 0 (- content-height 1))
                                                            (list "..."))
                                                    lines))
                                (final-text (format nil "~{~A~^~%~}" truncated-lines))
                                (history-style (tui:make-style
                                                :foreground (tui:parse-hex-color "#FAFAFA")
                                                :background *highlight*  ; Use purple like lipgloss
                                                :margin-top 1
                                                :margin-right 3
                                                :margin-bottom 0
                                                :margin-left 0
                                                :padding-top 1
                                                :padding-bottom 1
                                                :padding-left 2
                                                :padding-right 2
                                                :width item-width
                                                :height item-height))
                                (rendered (tui:render-styled history-style final-text)))
                           ;; Debug: write rendered box info to file
                           (with-open-file (f "/tmp/history-margins-debug.txt" :direction :output
                                              :if-exists :append :if-does-not-exist :create)
                             (format f "Box ~A rendered width: ~A~%" i (tui:width rendered))
                             (format f "First line: '~A'~%~%" (car (tui:split-string-by-newline rendered)))
                             (finish-output f))
                           rendered))))
      (let ((result (apply #'tui:join-horizontal tui:+top+ rendered-items)))
        ;; Debug the final joined result
        (with-open-file (f "/tmp/history-margins-debug.txt" :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (format f "Number of boxes: ~A~%" (length rendered-items))
          (format f "First box width: ~A~%" (tui:width (first rendered-items)))
          (format f "Final joined width: ~A~%~%" (tui:width result))
          (format f "First 3 lines of result:~%")
          (let ((lines (tui:split-string-by-newline result)))
            (loop for i from 0 below (min 3 (length lines))
                  do (format f "~A: '~A'~%" i (nth i lines))))
          (finish-output f))
        result))))

;;; Main Model
(defclass showcase-model ()
  ((height :initform 24 :accessor model-height)
   (width :initform 80 :accessor model-width)
   (tabs :accessor model-tabs)
   (dialog :accessor model-dialog)
   (list1 :accessor model-list1)
   (list2 :accessor model-list2)
   (history :accessor model-history)))

(defmethod initialize-instance :after ((model showcase-model) &key)
  (setf (model-tabs model)
        (make-instance 'tabs-model :id (tui:zone-new-prefix)))
  (setf (model-dialog model)
        (make-instance 'dialog-model :id (tui:zone-new-prefix)))
  (setf (model-list1 model)
        (make-instance 'list-model
                       :id (tui:zone-new-prefix)
                       :title "Citrus Fruits to Try"
                       :items (list (make-instance 'list-item :name "Grapefruit" :done t)
                                   (make-instance 'list-item :name "Yuzu" :done nil)
                                   (make-instance 'list-item :name "Citron" :done nil)
                                   (make-instance 'list-item :name "Kumquat" :done t)
                                   (make-instance 'list-item :name "Pomelo" :done nil))))
  (setf (model-list2 model)
        (make-instance 'list-model
                       :id (tui:zone-new-prefix)
                       :title "Actual Lip Gloss Vendors"
                       :items (list (make-instance 'list-item :name "Glossier" :done t)
                                   (make-instance 'list-item :name "Claire's Boutique" :done t)
                                   (make-instance 'list-item :name "Nyx" :done nil)
                                   (make-instance 'list-item :name "Mac" :done nil)
                                   (make-instance 'list-item :name "Milk" :done nil))))
  (setf (model-history model)
        (make-instance 'history-model
                       :id (tui:zone-new-prefix)
                       :items (list
                              (concatenate 'string
                                          "The Romans learned from the Greeks that quinces slowly cooked with honey would \"set\" when cool. "
                                          "The Apicius gives a recipe for preserving whole quinces, stems and leaves attached, "
                                          "in a bath of honey diluted with defrutum: Roman marmalade. "
                                          "Preserves of quince and lemon appear (along with rose, apple, plum and pear) "
                                          "in the Book of ceremonies of the Byzantine Emperor Constantine VII Porphyrogennetos.")
                              (concatenate 'string
                                          "Medieval quince preserves, which went by the French name cotignac, "
                                          "produced in a clear version and a fruit pulp version, "
                                          "began to lose their medieval seasoning of spices in the 16th century. "
                                          "In the 17th century, La Varenne provided recipes for both thick and clear cotignac.")
                              (concatenate 'string
                                          "In 1524, Henry VIII, King of England, received a \"box of marmalade\" from Mr. Hull of Exeter. "
                                          "This was probably marmelada, a solid quince paste from Portugal, "
                                          "still made and sold in southern Europe today. "
                                          "It became a favourite treat of Anne Boleyn and her ladies in waiting.")))))

(defmethod tui:init ((model showcase-model))
  ;; Initialize child models with proper dimensions
  (let* ((adjusted-msg (tui:make-window-size-msg
                        :width (- (model-width model) 4)
                        :height (- (model-height model) 2))))
    (tui:update-message (model-tabs model) adjusted-msg)
    (tui:update-message (model-dialog model) adjusted-msg)
    (tui:update-message (model-list1 model) adjusted-msg)
    (tui:update-message (model-list2 model) adjusted-msg)

    ;; History gets even smaller height
    (let ((history-msg (tui:make-window-size-msg
                        :width (- (model-width model) 4)
                        :height (- (model-height model) 2
                                  (tabs-height (model-tabs model))
                                  (list-height (model-list1 model))))))
      (tui:update-message (model-history model) history-msg)))
  nil)

(defmethod tui:update-message ((model showcase-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (values model (tui:quit-cmd)))
      (t (values model nil)))))

(defmethod tui:update-message ((model showcase-model) (msg tui:window-size-msg))
  (setf (model-height model) (tui:window-size-msg-height msg))
  (setf (model-width model) (tui:window-size-msg-width msg))

  ;; Propagate to children
  (let* ((adjusted-msg (tui:make-window-size-msg
                       :width (- (tui:window-size-msg-width msg) 4)
                       :height (- (tui:window-size-msg-height msg) 2))))
    (tui:update-message (model-tabs model) adjusted-msg)
    (tui:update-message (model-dialog model) adjusted-msg)
    (tui:update-message (model-list1 model) adjusted-msg)
    (tui:update-message (model-list2 model) adjusted-msg)

    ;; History gets even smaller height
    (let ((history-msg (tui:make-window-size-msg
                       :width (- (tui:window-size-msg-width msg) 4)
                       :height (- (tui:window-size-msg-height msg) 2
                                 (tabs-height (model-tabs model))
                                 (list-height (model-list1 model))))))
      (tui:update-message (model-history model) history-msg)))

  (values model nil))

(defmethod tui:update-message ((model showcase-model) (msg tui:mouse-release-event))
  ;; Handle mouse clicks for all interactive elements
  (when (eq (tui:mouse-event-button msg) :left)

    ;; Handle tabs
    (let ((tabs (model-tabs model)))
      (dolist (item (tabs-items tabs))
        (let ((zone (tui:zone-get (concatenate 'string (tabs-id tabs) item))))
          (when (tui:zone-in-bounds-p zone msg)
            (setf (tabs-active tabs) item)
            (return-from tui:update-message (values model nil))))))

    ;; Handle dialog buttons
    (let ((dialog (model-dialog model)))
      (let ((confirm-zone (tui:zone-get (concatenate 'string (dialog-id dialog) "confirm")))
            (cancel-zone (tui:zone-get (concatenate 'string (dialog-id dialog) "cancel"))))
        (cond
          ((tui:zone-in-bounds-p confirm-zone msg)
           (setf (dialog-active dialog) "confirm")
           (return-from tui:update-message (values model nil)))
          ((tui:zone-in-bounds-p cancel-zone msg)
           (setf (dialog-active dialog) "cancel")
           (return-from tui:update-message (values model nil))))))

    ;; Handle list1 items
    (let ((list1 (model-list1 model)))
      (dolist (item (list-items list1))
        (let ((zone (tui:zone-get (concatenate 'string (list-id list1) (item-name item)))))
          (when (tui:zone-in-bounds-p zone msg)
            (setf (item-done item) (not (item-done item)))
            (return-from tui:update-message (values model nil))))))

    ;; Handle list2 items
    (let ((list2 (model-list2 model)))
      (dolist (item (list-items list2))
        (let ((zone (tui:zone-get (concatenate 'string (list-id list2) (item-name item)))))
          (when (tui:zone-in-bounds-p zone msg)
            (setf (item-done item) (not (item-done item)))
            (return-from tui:update-message (values model nil)))))))

  (values model nil))

(defmethod tui:view ((model showcase-model))
  ;; Show different content based on active tab
  (let* ((tabs-view (tui:view (model-tabs model)))
         (active-tab (tabs-active (model-tabs model)))
         (tab-content
          (cond
            ;; Tuition tab: Show lists and dialog (the main demo)
            ((string= active-tab "Tuition")
             (let ((list1-view (tui:view (model-list1 model)))
                   (list2-view (tui:view (model-list2 model)))
                   (dialog-view (tui:view (model-dialog model))))
               (tui:join-horizontal tui:+top+
                                    list1-view
                                    list2-view
                                    dialog-view)))

            ;; Styles tab: Show styled text examples
            ((string= active-tab "Styles")
             (let ((examples (list
                             (tui:bold "This is bold text")
                             (tui:italic "This is italic text")
                             (tui:underline "This is underlined text")
                             (tui:colored "This is colored text" :fg *highlight*)
                             (tui:render-styled (tui:make-style :foreground *special*
                                                                :background *subtle*
                                                                :padding-left 2
                                                                :padding-right 2)
                                               "Styled with padding"))))
               (apply #'tui:join-vertical tui:+left+ examples)))

            ;; Layouts tab: Show layout examples
            ((string= active-tab "Layouts")
             (let ((box1 (tui:render-styled (tui:make-style :background *highlight*
                                                            :padding 2
                                                            :margin 1)
                                           "Box 1"))
                   (box2 (tui:render-styled (tui:make-style :background *special*
                                                            :padding 2
                                                            :margin 1)
                                           "Box 2"))
                   (box3 (tui:render-styled (tui:make-style :background *subtle*
                                                            :padding 2
                                                            :margin 1)
                                           "Box 3")))
               (tui:join-vertical tui:+left+
                                 "Horizontal layout:"
                                 (tui:join-horizontal tui:+top+ box1 box2 box3)
                                 ""
                                 "Vertical layout:"
                                 (tui:join-vertical tui:+left+ box1 box2 box3))))

            ;; Colors tab: Show color palette
            ((string= active-tab "Colors")
             (let ((colors (list
                           (cons "Subtle" *subtle*)
                           (cons "Highlight" *highlight*)
                           (cons "Special" *special*)
                           (cons "Red" tui:*fg-red*)
                           (cons "Green" tui:*fg-green*)
                           (cons "Blue" tui:*fg-blue*)
                           (cons "Yellow" tui:*fg-yellow*)
                           (cons "Magenta" tui:*fg-magenta*)
                           (cons "Cyan" tui:*fg-cyan*))))
               (apply #'tui:join-vertical tui:+left+
                     "Color Palette:"
                     ""
                     (loop for (name . color) in colors
                           collect (tui:colored (format nil "  ~A" name) :fg color)))))

            ;; Borders tab: Show border styles
            ((string= active-tab "Borders")
             (let ((examples (list
                             (tui:render-border "Normal Border" tui:*border-normal*)
                             (tui:render-border "Rounded Border" tui:*border-rounded*)
                             (tui:render-border "Thick Border" tui:*border-thick*)
                             (tui:render-border "Double Border" tui:*border-double*)
                             (tui:render-border "ASCII Border" tui:*border-ascii*))))
               (apply #'tui:join-horizontal tui:+top+ examples)))

            ;; History tab: Show marmalade history
            ((string= active-tab "History")
             (tui:view (model-history model)))

            ;; Fallback
            (t "Unknown tab")))
         (content (tui:join-vertical tui:+top+
                                     tabs-view
                                     ""
                                     tab-content)))
    ;; Apply zone-scan to the final complete layout
    (tui:zone-scan content)))

;;; Main entry point
(defun main ()
  "Run the interactive showcase."
  ;; Initialize global zone manager
  (tui:init-global-zone-manager)

  ;; Suppress SBCL poll warnings during terminal I/O
  #+sbcl
  (handler-bind ((warning #'muffle-warning))
    (let ((program (tui:make-program (make-instance 'showcase-model)
                                    :alt-screen t
                                    :mouse :cell-motion)))
      (tui:run program)))

  #-sbcl
  (let ((program (tui:make-program (make-instance 'showcase-model)
                                  :alt-screen t
                                  :mouse :cell-motion)))
    (tui:run program)))

(eval-when (:load-toplevel :execute)
  (main))
