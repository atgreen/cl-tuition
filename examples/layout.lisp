;;; Layout Example
;;;
;;; This example demonstrates various Tuition style and layout features,
;;; ported from the Lipgloss layout example.
;;; It showcases tabs, dialogs, lists, color grids, and status bars.

(asdf:load-system :tuition)

(defpackage :tuition.examples.layout
  (:use :cl)
  (:local-nicknames (#:tui #:tuition)))

(in-package :tuition.examples.layout)

(defconstant +width+ 96)
(defconstant +column-width+ 30)

;;; Helper function to create rainbow text
(defun %hex->rgb (hex)
  (let* ((clean (string-upcase (string-trim '(#\#) hex)))
         (r (parse-integer (subseq clean 0 2) :radix 16))
         (g (parse-integer (subseq clean 2 4) :radix 16))
         (b (parse-integer (subseq clean 4 6) :radix 16)))
    (values r g b)))

(defun %rgb->hex (r g b)
  (format nil "#~2,'0X~2,'0X~2,'0X" (max 0 (min 255 r)) (max 0 (min 255 g)) (max 0 (min 255 b))))

(defun %lerp (a b frac)
  (+ a (round (* frac (- b a)))))

(defun gradient-colors (anchors steps)
  "Generate a smooth gradient of length STEPS across ANCHORS (list of #RRGGBB)."
  (let* ((n (length anchors))
         (segments (max 1 (1- n))))
    (loop for i from 0 below steps collect
          (let* ((pos (/ (float i) (max 1 (1- steps))))
                 (seg (min (1- segments) (floor (* pos segments))))
                 (local (- (* pos segments) seg))
                 (a (nth seg anchors))
                 (b (nth (min (1- n) (1+ seg)) anchors)))
            (multiple-value-bind (ar ag ab) (%hex->rgb a)
              (multiple-value-bind (br bg bb) (%hex->rgb b)
                (%rgb->hex (%lerp ar br local)
                           (%lerp ag bg local)
                           (%lerp ab bb local))))))))

(defun rainbow-text (text anchors)
  "Apply a smooth rainbow gradient across TEXT using ANCHORS."
  (let* ((chars (coerce text 'list))
         (steps (length chars))
         (palette (gradient-colors anchors steps))
         (result ""))
    (loop for ch in chars
          for hex in palette
          for code = (tui:parse-hex-color hex)
          do (setf result (concatenate 'string result (tui:colored (string ch) :fg code))))
    result))

;;; Helper function to generate a simple color gradient
(defun color-gradient (steps)
  "Generate a simple color gradient."
  (let ((colors '("#F25D94" "#E86BA0" "#DE7DAC" "#D48FB8"
                  "#CAA1C4" "#C0B3D0" "#B6C5DC" "#ACD7E8")))
    (subseq colors 0 (min steps (length colors)))))

;;; Helper to create a color grid with 2D interpolation (matches lipgloss)
(defun %lerp-rgb (r1 g1 b1 r2 g2 b2 frac)
  "Linearly interpolate between two RGB colors."
  (values (round (+ r1 (* frac (- r2 r1))))
          (round (+ g1 (* frac (- g2 g1))))
          (round (+ b1 (* frac (- b2 b1))))))

(defun color-grid (x-steps y-steps)
  "Create a color grid with 2D color interpolation between 4 corners."
  (with-output-to-string (s)
    ;; Get the 4 corner colors
    (multiple-value-bind (x0y0-r x0y0-g x0y0-b) (%hex->rgb "#F25D94")
      (multiple-value-bind (x1y0-r x1y0-g x1y0-b) (%hex->rgb "#EDFF82")
        (multiple-value-bind (x0y1-r x0y1-g x0y1-b) (%hex->rgb "#643AFF")
          (multiple-value-bind (x1y1-r x1y1-g x1y1-b) (%hex->rgb "#14F9D5")
            ;; For each row
            (dotimes (y y-steps)
              (let ((y-frac (if (> y-steps 1) (/ (float y) (float (1- y-steps))) 0.0)))
                ;; Interpolate left and right edge colors for this row
                (multiple-value-bind (left-r left-g left-b)
                    (%lerp-rgb x0y0-r x0y0-g x0y0-b x0y1-r x0y1-g x0y1-b y-frac)
                  (multiple-value-bind (right-r right-g right-b)
                      (%lerp-rgb x1y0-r x1y0-g x1y0-b x1y1-r x1y1-g x1y1-b y-frac)
                    ;; For each column in this row
                    (dotimes (x x-steps)
                      (let ((x-frac (if (> x-steps 1) (/ (float x) (float (1- x-steps))) 0.0)))
                        (multiple-value-bind (col-r col-g col-b)
                            (%lerp-rgb left-r left-g left-b right-r right-g right-b x-frac)
                          (let ((hex (%rgb->hex col-r col-g col-b)))
                            (format s "~A" (tui:colored "  " :bg (tui:parse-hex-color hex)))))))
                    ;; End of row - add newline
                    (format s "~%")))))))))))


;;; Build tabs section
(defun build-tabs ()
  "Build the tabs section."
  (let* ((active-tab-border (tui:make-border :top "─" :bottom " "
                                             :left "│" :right "│"
                                             :top-left "╭" :top-right "╮"
                                             :bottom-left "┘" :bottom-right "└"))
         (tab-border (tui:make-border :top "─" :bottom "─"
                                     :left "│" :right "│"
                                     :top-left "╭" :top-right "╮"
                                     :bottom-left "┴" :bottom-right "┴"))
         (highlight-color (tui:parse-hex-color "#7D56F4"))
         (row (tui:join-horizontal tui:+top+
                                  (tui:render-border " Tuition " active-tab-border
                                                    :fg-color highlight-color)
                                  (tui:render-border " Styles " tab-border
                                                    :fg-color highlight-color)
                                  (tui:render-border " Layouts " tab-border
                                                    :fg-color highlight-color)
                                  (tui:render-border " Colors " tab-border
                                                    :fg-color highlight-color)
                                  (tui:render-border " Borders " tab-border
                                                    :fg-color highlight-color)))
         (gap-width (max 0 (- +width+ (tui:width row) 2)))
         (gap-border (tui:make-border :top "" :bottom "─"
                                     :left "" :right ""
                                     :top-left "" :top-right ""
                                     :bottom-left "" :bottom-right ""))
         (gap (tui:render-border (make-string gap-width :initial-element #\Space)
                                gap-border :top nil :left nil :right nil
                                :fg-color highlight-color)))
    (concatenate 'string
                 (tui:join-horizontal tui:+bottom+ row gap)
                 (format nil "~%~%"))))

;;; Build title section
(defun build-title ()
  "Build the title section."
  (let* ((colors (color-gradient 5))
         (title-lines (loop for color-hex in colors
                           for i from 0
                           for color = (tui:parse-hex-color color-hex)
                           for fg-color = (tui:parse-hex-color "#FFF7DB")
                           for title-style = (tui:make-style
                                             :margin-left (* i 2)
                                             :margin-right 5
                                             :padding-left 1
                                             :padding-right 1
                                             :italic t
                                             :foreground fg-color
                                             :background color)
                           collect (tui:render-styled title-style "Tuition")))
         (title (format nil "~{~A~^~%~}" title-lines))
         (subtle-color (tui:parse-hex-color "#383838"))
         (normal-color (tui:parse-hex-color "#EEEEEE"))
         (special-color (tui:parse-hex-color "#73F59F"))
         (divider (tui:render-styled (tui:make-style :padding-left 1 :padding-right 1 :foreground subtle-color) "•"))
         (desc1 (tui:render-styled (tui:make-style :margin-top 1 :foreground normal-color)
                                  "Style Definitions for Nice Terminal Layouts"))
         (url (tui:colored "https://github.com/atgreen/tuition" :fg special-color))
         (desc2-border (tui:make-border :top "─" :top-left "─" :top-right "─"
                                       :bottom "" :left "" :right ""
                                       :bottom-left "" :bottom-right ""))
         (desc2 (tui:render-border
                (concatenate 'string "From Common Lisp" divider url)
                desc2-border
                :bottom nil :left nil :right nil
                :fg-color subtle-color))
         (desc (tui:join-vertical tui:+left+ desc1 desc2))
         (row (tui:join-horizontal tui:+top+ title desc)))
    (concatenate 'string row (format nil "~%~%"))))

;;; Build dialog section
(defun build-dialog ()
  "Build the dialog box section."
  (let* ((ok-style (tui:make-style :foreground (tui:parse-hex-color "#FFF7DB")
                                  :background (tui:parse-hex-color "#F25D94")
                                  :padding-left 3
                                  :padding-right 3
                                  :margin-top 1
                                  :margin-right 2
                                  :underline t))
         (cancel-style (tui:make-style :foreground (tui:parse-hex-color "#FFF7DB")
                                       :background (tui:parse-hex-color "#888B7E")
                                       :padding-left 3
                                       :padding-right 3
                                       :margin-top 1))
         (ok-button (tui:render-styled ok-style "Yes"))
         (cancel-button (tui:render-styled cancel-style "Maybe"))
         (rainbow-colors '("#F25D94" "#E86BA0" "#DE7DAC" "#D48FB8" "#CAA1C4"
                          "#C0B3D0" "#B6C5DC" "#ACD7E8" "#A2E9F4" "#98FBFF"
                          "#EDFF82" "#E8F57D" "#E3EB78" "#DEE173" "#D9D76E"))
         (question-style (tui:make-style :width 50 :align tui:+center+))
         (question (tui:render-styled question-style
                                     (rainbow-text "Are you sure you want to eat marmalade?" rainbow-colors)))
         (buttons (tui:join-horizontal tui:+top+ ok-button cancel-button))
         (ui (tui:join-vertical tui:+center+ question buttons))
         ;; Add padding (1, 0) to match lipgloss: 1 line top/bottom, 0 left/right
         (ui-with-padding (tui:render-styled (tui:make-style :padding-top 1 :padding-bottom 1) ui))
         (dialog-color (tui:parse-hex-color "#874BFD"))
         (dialog-box (tui:render-border ui-with-padding tui:*border-rounded*
                                       :fg-color dialog-color))
         (subtle-color (tui:parse-hex-color "#383838"))
         (dialog (tui:place +width+ 9 tui:+center+ tui:+center+ dialog-box
                           :whitespace-char #\猫
                           :whitespace-fg subtle-color)))
    (concatenate 'string dialog (format nil "~%~%"))))

;;; Build lists section
(defun build-lists ()
  "Build the two lists side by side."
  (let* ((subtle-color (tui:parse-hex-color "#383838"))
         (normal-color (tui:parse-hex-color "#EEEEEE"))
         (special-color (tui:parse-hex-color "#73F59F"))
         (done-color (tui:parse-hex-color "#696969"))

         (header-style (tui:make-style :foreground normal-color))
         (item-style (tui:make-style :foreground normal-color :padding-left 2))
         (done-style (tui:make-style :foreground done-color :strikethrough t))
         (checkmark (tui:colored "✓ " :fg special-color))

         ;; List 1
         (list1-border (tui:make-border :right "│" :top "" :bottom "" :left ""
                                       :top-left "" :top-right "" :bottom-left "" :bottom-right ""))
         (list1-header-border (tui:make-border :bottom "─" :top "" :left "" :right ""
                                              :top-left "" :top-right "" :bottom-left "" :bottom-right ""))
         (list1-header (tui:render-border
                       (tui:render-styled header-style "Citrus Fruits to Try")
                       list1-header-border :top nil :left nil :right nil
                       :fg-color subtle-color))
         (list1-items (tui:join-vertical tui:+left+
                                        (concatenate 'string checkmark
                                                    (tui:render-styled done-style "Grapefruit"))
                                        (concatenate 'string checkmark
                                                    (tui:render-styled done-style "Yuzu"))
                                        (tui:render-styled item-style "Citron")
                                        (tui:render-styled item-style "Kumquat")
                                        (tui:render-styled item-style "Pomelo")))
         (list1-content (tui:join-vertical tui:+left+ list1-header list1-items))
         (list1-style (tui:make-style :height 8 :width (1+ +column-width+)))
         (list1-box (tui:render-border (tui:render-styled list1-style list1-content)
                                       list1-border :top nil :bottom nil :left nil
                                       :fg-color subtle-color))
         (list1 (tui:render-styled (tui:make-style :margin-right 2) list1-box))

         ;; List 2
         (list2-header (tui:render-border
                       (tui:render-styled header-style "Actual Lip Gloss Vendors")
                       list1-header-border :top nil :left nil :right nil
                       :fg-color subtle-color))
         (list2-items (tui:join-vertical tui:+left+
                                        (tui:render-styled item-style "Glossier")
                                        (tui:render-styled item-style "Claire's Boutique")
                                        (concatenate 'string checkmark
                                                    (tui:render-styled done-style "Nyx"))
                                        (tui:render-styled item-style "Mac")
                                        (concatenate 'string checkmark
                                                    (tui:render-styled done-style "Milk"))))
          (list2-content (tui:join-vertical tui:+left+ list2-header list2-items))
          (list2-style (tui:make-style :height 8 :width +column-width+))
          (list2-box (tui:render-border (tui:render-styled list2-style list2-content)
                                        list1-border :top nil :bottom nil :left nil
                                        :fg-color subtle-color))
          (list2 (tui:render-styled (tui:make-style :margin-right 2) list2-box)))
    (list list1 list2)))

;;; Build history section
(defun build-history ()
  "Build the history/paragraphs section."
  (let* ((history-fg (tui:parse-hex-color "#FAFAFA"))
         (history-bg (tui:parse-hex-color "#7D56F4"))
         (history-style-right (tui:make-style :foreground history-fg
                                              :background history-bg
                                              :margin-top 1
                                              :margin-right 3
                                              :padding-top 1
                                              :padding-bottom 1
                                              :padding-left 2
                                              :padding-right 2
                                              :height 19
                                              :width +column-width+
                                              :align tui:+right+))
         (history-style-center (tui:make-style :foreground history-fg
                                               :background history-bg
                                               :margin-top 1
                                               :margin-right 3
                                               :padding-top 1
                                               :padding-bottom 1
                                               :padding-left 2
                                               :padding-right 2
                                               :height 19
                                               :width +column-width+
                                               :align tui:+center+))
         (history-style-left (tui:make-style :foreground history-fg
                                             :background history-bg
                                             :margin-top 1
                                             :padding-top 1
                                             :padding-bottom 1
                                             :padding-left 2
                                             :padding-right 2
                                             :height 19
                                             :width +column-width+
                                             :align tui:+left+))
         (history-a "The Romans learned from the Greeks that quinces slowly cooked with honey would \"set\" when cool. The Apicius gives a recipe for preserving whole quinces, stems and leaves attached, in a bath of honey diluted with defrutum: Roman marmalade. Preserves of quince and lemon appear (along with rose, apple, plum and pear) in the Book of ceremonies of the Byzantine Emperor Constantine VII Porphyrogennetos.")
         (history-b "Medieval quince preserves, which went by the French name cotignac, produced in a clear version and a fruit pulp version, began to lose their medieval seasoning of spices in the 16th century. In the 17th century, La Varenne provided recipes for both thick and clear cotignac.")
         (history-c "In 1524, Henry VIII, King of England, received a \"box of marmalade\" from Mr. Hull of Exeter. This was probably marmelada, a solid quince paste from Portugal, still made and sold in southern Europe today. It became a favourite treat of Anne Boleyn and her ladies in waiting."))
    (tui:join-horizontal tui:+top+
                        (tui:render-styled history-style-right history-a)
                        (tui:render-styled history-style-center history-b)
                        (tui:render-styled history-style-left history-c))))

;;; Build status bar
(defun build-status-bar ()
  "Build the status bar."
  (let* ((status-bar-fg (tui:parse-hex-color "#C1C6B2"))
         (status-bar-bg (tui:parse-hex-color "#353533"))
         (status-fg (tui:parse-hex-color "#FFFDF5"))
         (status-bg (tui:parse-hex-color "#FF5F87"))
         (encoding-bg (tui:parse-hex-color "#A550DF"))
         (fish-bg (tui:parse-hex-color "#6124DF"))

         (status-style (tui:make-style :foreground status-fg
                                      :background status-bg
                                      :padding-left 1
                                      :padding-right 1
                                      :margin-right 1))
         (nugget-style (tui:make-style :foreground status-fg :padding-left 1 :padding-right 1))
         (status-key (tui:render-styled status-style "STATUS"))
         (encoding-style (tui:make-style :foreground status-fg
                                        :background encoding-bg
                                        :padding-left 1
                                        :padding-right 1
                                        :align tui:+right+))
         (encoding (tui:render-styled encoding-style "UTF-8"))
         (fish-style (tui:make-style :foreground status-fg
                                    :background fish-bg
                                    :padding-left 1
                                    :padding-right 1))
         (fish-cake (tui:render-styled fish-style "🍥 Fish Cake"))
         ;; Be defensive about width math in case any earlier step
         ;; misreports widths. Clamp each part and the final remainder.
         (skw (min +width+ (max 0 (tui:width status-key))))
         (encw (min +width+ (max 0 (tui:width encoding))))
         (fcw (min +width+ (max 0 (tui:width fish-cake))))
         (status-val-width (max 0 (- +width+ skw encw fcw)))
         (status-val-style (tui:make-style :foreground status-bar-fg
                                          :background status-bar-bg
                                          :width status-val-width
                                          :max-width status-val-width))
         (status-val (tui:render-styled status-val-style "Ravishing")))
    ;; Join the final segments - no need to wrap in another style
    (tui:join-horizontal tui:+top+ status-key status-val encoding fish-cake)))

;;; Main function to build and display the layout
(defun main ()
  "Build and display the complete layout."
  (let* ((tabs (build-tabs))
         (title (build-title))
         (dialog (build-dialog))
         (lists (build-lists))
         ;; Mirror Lip Gloss: fixed color grid dimensions
         (colors (color-grid 14 8))
         (colors-styled colors)
         (l1 (car lists))
         (l2 (cadr lists))
         (lists-and-colors (tui:join-horizontal tui:+top+ l1 l2 colors-styled))
         (history (build-history))
         (status-bar (build-status-bar))
         (doc (concatenate 'string
                          tabs
                          title
                          dialog
                          lists-and-colors
                          history
                          (format nil "~%~%")
                          status-bar))
         (doc-style (tui:make-style :padding-top 1
                                   :padding-right 2
                                   :padding-bottom 1
                                   :padding-left 2))
         (final (tui:render-styled doc-style doc)))
    (format t "~A~%" final)))

;;; Run the example
(eval-when (:load-toplevel :execute)
  (main))
