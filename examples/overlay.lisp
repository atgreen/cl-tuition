;;; Overlay Example
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; This example demonstrates the overlay/composite functionality,
;;; inspired by github.com/rmhubbert/bubbletea-overlay.
;;;
;;; It shows how to composite a foreground dialog on top of a background UI.

(asdf:load-system :tuition)

(defpackage :tuition.examples.overlay
  (:use :cl)
  (:local-nicknames (#:tui #:tuition))
  (:documentation "Overlay example demonstrating text compositing."))

(in-package :tuition.examples.overlay)

;;; Build a colorful background grid
(defun build-background (width height)
  "Build a colorful background grid."
  (with-output-to-string (s)
    (dotimes (y height)
      (dotimes (x width)
        ;; Create a gradient pattern
        (let* ((r (floor (* 255 (/ x width))))
               (g (floor (* 255 (/ y height))))
               (b (floor (* 128 (+ (/ x width) (/ y height)))))
               (color (tui:color-rgb r g b :foreground nil)))
          (format s "~A" (tui:colored "  " :bg color))))
      (unless (= y (1- height))
        (format s "~%")))))

;;; Build a simple list UI as background
(defun build-list-background (width height)
  "Build a list-style UI as background."
  (let* ((items '("Documents" "Downloads" "Pictures" "Music" "Videos"
                  "Projects" "Desktop" "Applications" "Library" "System"))
         (header-style (tui:make-style :bold t
                                       :foreground tui:*fg-bright-cyan*
                                       :padding-bottom 1))
         (item-style (tui:make-style :foreground tui:*fg-white*
                                     :padding-left 2))
         (selected-style (tui:make-style :foreground tui:*fg-black*
                                         :background tui:*bg-cyan*
                                         :padding-left 2
                                         :width (- width 4)))
         (bg-style (tui:make-style :width width
                                   :height height
                                   :background tui:*bg-bright-black*)))
    (let ((content
            (with-output-to-string (s)
              (format s "~A~%" (tui:render-styled header-style "File Browser"))
              (loop for item in items
                    for i from 0
                    do (format s "~A~%"
                               (if (= i 2)
                                   (tui:render-styled selected-style item)
                                   (tui:render-styled item-style item)))))))
      (tui:render-styled bg-style content))))

;;; Build a dialog box
(defun build-dialog ()
  "Build a dialog box to overlay on the background."
  (let* ((title-style (tui:make-style :bold t
                                      :foreground tui:*fg-bright-yellow*
                                      :padding-bottom 1))
         (text-style (tui:make-style :foreground tui:*fg-white*
                                     :width 30
                                     :padding-bottom 1))
         (button-style (tui:make-style :foreground tui:*fg-black*
                                       :background tui:*bg-bright-green*
                                       :padding-left 2
                                       :padding-right 2))
         (cancel-style (tui:make-style :foreground tui:*fg-white*
                                       :background tui:*bg-red*
                                       :padding-left 2
                                       :padding-right 2
                                       :margin-left 2))
         (dialog-style (tui:make-style :background tui:*bg-blue*
                                       :padding 1))
         ;; Outer wrapper with background to create visual buffer around dialog
         (wrapper-style (tui:make-style :background tui:*bg-bright-black*
                                        :padding 1)))
    (let* ((title (tui:render-styled title-style "Confirm Delete"))
           (text (tui:render-styled text-style
                                    "Are you sure you want to delete this file? This action cannot be undone."))
           (ok-btn (tui:render-styled button-style " OK "))
           (cancel-btn (tui:render-styled cancel-style "Cancel"))
           (buttons (tui:join-horizontal tui:+left+ ok-btn cancel-btn))
           (content (tui:join-vertical tui:+left+ title text buttons))
           (styled (tui:render-styled dialog-style content))
           (bordered (tui:render-border styled tui:*border-rounded*
                                        :fg-color tui:*fg-bright-white*)))
      ;; Wrap with styled padding for visual separation from background
      (tui:render-styled wrapper-style bordered))))

;;; Build a tooltip
(defun build-tooltip (text)
  "Build a small tooltip."
  (let ((style (tui:make-style :background tui:*bg-yellow*
                               :foreground tui:*fg-black*
                               :padding-left 1
                               :padding-right 1)))
    (tui:render-styled style text)))

;;; Demo 1: Static overlay demonstration
(defun demo-static ()
  "Demonstrate static overlay compositing."
  (format t "~%=== Demo 1: Centered Dialog Overlay ===~%~%")

  (let* ((bg (build-list-background 60 15))
         (dialog (build-dialog))
         (result (tui:composite dialog bg
                                :x-position tui:+center+
                                :y-position tui:+middle+)))
    (format t "~A~%~%" result)))

;;; Demo 2: Multiple overlays
(defun demo-multiple ()
  "Demonstrate multiple overlays."
  (format t "~%=== Demo 2: Multiple Overlays with Offsets ===~%~%")

  (let* ((bg (build-list-background 70 18))
         (dialog (build-dialog))
         (tooltip (build-tooltip "Press Enter to confirm"))
         ;; First overlay the dialog centered
         (with-dialog (tui:composite dialog bg
                                     :x-position tui:+center+
                                     :y-position tui:+middle+))
         ;; Then overlay a tooltip at bottom-right
         (result (tui:composite tooltip with-dialog
                                :x-position tui:+right+
                                :y-position tui:+bottom+
                                :x-offset -2
                                :y-offset -1)))
    (format t "~A~%~%" result)))

;;; Demo 3: Corner positioning
(defun demo-corners ()
  "Demonstrate overlay positioning at corners."
  (format t "~%=== Demo 3: Corner Positioning ===~%~%")

  (let* ((bg (build-background 40 12))
         (tl (build-tooltip "Top-Left"))
         (tr (build-tooltip "Top-Right"))
         (bl (build-tooltip "Bottom-Left"))
         (br (build-tooltip "Bottom-Right"))
         (center (build-tooltip "CENTER")))
    ;; Layer overlays from back to front
    (let* ((r1 (tui:composite tl bg :x-position tui:+left+ :y-position tui:+top+))
           (r2 (tui:composite tr r1 :x-position tui:+right+ :y-position tui:+top+))
           (r3 (tui:composite bl r2 :x-position tui:+left+ :y-position tui:+bottom+))
           (r4 (tui:composite br r3 :x-position tui:+right+ :y-position tui:+bottom+))
           (result (tui:overlay-centered center r4)))
      (format t "~A~%~%" result))))

;;; Demo 4: Absolute positioning
(defun demo-absolute ()
  "Demonstrate absolute (x, y) positioning."
  (format t "~%=== Demo 4: Absolute Positioning ===~%~%")

  (let* ((bg (build-background 50 10))
         (marker (build-tooltip "X")))
    ;; Place markers at specific coordinates
    (let* ((r1 (tui:overlay-at marker bg 5 2))
           (r2 (tui:overlay-at marker r1 20 4))
           (r3 (tui:overlay-at marker r2 35 6))
           (result (tui:overlay-at marker r3 45 8)))
      (format t "~A~%~%" result))))

;;; Demo 5: Convenience functions
(defun demo-convenience ()
  "Demonstrate convenience overlay functions."
  (format t "~%=== Demo 5: Convenience Functions ===~%~%")

  (let* ((bg (build-list-background 50 12))
         (dialog (tui:render-border
                  (tui:render-styled
                   (tui:make-style :background tui:*bg-magenta*
                                   :foreground tui:*fg-white*
                                   :padding 1)
                   "overlay-centered")
                  tui:*border-double*
                  :fg-color tui:*fg-bright-magenta*)))
    (format t "Using overlay-centered:~%")
    (format t "~A~%~%" (tui:overlay-centered dialog bg))

    (format t "Using overlay-at (10, 3):~%")
    (format t "~A~%~%" (tui:overlay-at dialog bg 10 3))))

;;; Demo 6: Title bar on borders
(defun demo-title ()
  "Demonstrate title support in render-border."
  (format t "~%=== Demo 6: Border Titles ===~%~%")

  (let* ((content (tui:render-styled
                   (tui:make-style :foreground tui:*fg-white*
                                   :padding 1
                                   :width 30)
                   "This dialog has a title bar on the top border."))
         (title (tui:colored " Confirm " :fg tui:*fg-bright-yellow*))
         (dialog (tui:render-border content tui:*border-rounded*
                                    :fg-color tui:*fg-bright-white*
                                    :title title
                                    :title-position :center)))
    (format t "Centered title:~%")
    (format t "~A~%~%" dialog))

  (let* ((content (tui:render-styled
                   (tui:make-style :foreground tui:*fg-white*
                                   :padding 1
                                   :width 30)
                   "Left-aligned title example."))
         (title (tui:colored " Info " :fg tui:*fg-bright-cyan*))
         (dialog (tui:render-border content tui:*border-normal*
                                    :fg-color tui:*fg-cyan*
                                    :title title
                                    :title-position :left)))
    (format t "Left title:~%")
    (format t "~A~%~%" dialog)))

;;; Demo 7: Drop shadows
(defun demo-shadows ()
  "Demonstrate drop shadows on dialogs and buttons."
  (format t "~%=== Demo 7: Drop Shadows ===~%~%")

  ;; Dialog with transparent shadow on a background
  (let* ((bg (build-list-background 50 14))
         (content (tui:render-styled
                   (tui:make-style :foreground tui:*fg-white*
                                   :background tui:*bg-blue*
                                   :padding 1
                                   :width 28)
                   "A dialog box with a transparent drop shadow."))
         (title (tui:colored " Dialog " :fg tui:*fg-bright-yellow*))
         (dialog (tui:render-border content tui:*border-double*
                                    :fg-color tui:*fg-bright-white*
                                    :bg-color tui:*bg-blue*
                                    :title title))
         (result (tui:composite-with-shadow dialog bg
                                            :x-position 4
                                            :y-position 2)))
    (format t "Transparent shadow (background text shows through):~%")
    (format t "~A~%~%" result))

  ;; Opaque shadow style for comparison
  (let* ((content (tui:render-styled
                   (tui:make-style :foreground tui:*fg-white*
                                   :background tui:*bg-blue*
                                   :padding 1
                                   :width 28)
                   "Same dialog with an opaque block shadow."))
         (title (tui:colored " Dialog " :fg tui:*fg-bright-yellow*))
         (bordered (tui:render-border content tui:*border-double*
                                      :fg-color tui:*fg-bright-white*
                                      :bg-color tui:*bg-blue*
                                      :title title))
         (shadowed (tui:render-shadow bordered)))
    (format t "Opaque shadow (style :dark):~%")
    (format t "~A~%~%" shadowed))

  ;; Buttons with transparent shadows on a background
  (let* ((bg (tui:render-styled
              (tui:make-style :background tui:*bg-bright-black*
                              :width 40
                              :height 4)
              (format nil "  Button bar area~%  with background text~%  that shows through")))
         (ok-btn (tui:render-styled
                  (tui:make-style :foreground tui:*fg-black*
                                  :background tui:*bg-bright-green*
                                  :padding-left 2
                                  :padding-right 2)
                  "  OK  "))
         (cancel-btn (tui:render-styled
                      (tui:make-style :foreground tui:*fg-white*
                                      :background tui:*bg-red*
                                      :padding-left 2
                                      :padding-right 2)
                      "Cancel"))
         ;; Composite buttons with transparent shadows onto background
         (with-ok (tui:composite-with-shadow ok-btn bg
                                             :x-position 3
                                             :y-position 1
                                             :shadow-width 1
                                             :shadow-offset 0))
         (result (tui:composite-with-shadow cancel-btn with-ok
                                            :x-position 18
                                            :y-position 1
                                            :shadow-width 1
                                            :shadow-offset 0)))
    (format t "Buttons with transparent shadows:~%")
    (format t "~A~%~%" result)))

;;; Main function
(defun main ()
  "Run all overlay demos."
  (format t "~%")
  (format t "========================================~%")
  (format t "       Tuition Overlay Demo~%")
  (format t "========================================~%")
  (format t "~%Demonstrating text compositing inspired by~%")
  (format t "github.com/rmhubbert/bubbletea-overlay~%")

  (demo-static)
  (demo-multiple)
  (demo-corners)
  (demo-absolute)
  (demo-convenience)
  (demo-title)
  (demo-shadows)

  (format t "========================================~%")
  (format t "              Demo Complete~%")
  (format t "========================================~%~%"))

;;; Run the example
(eval-when (:load-toplevel :execute)
  (main))
