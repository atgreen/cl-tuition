;;; test-progress.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for the progress component (src/components/progress.lisp)

(in-package #:tuition-tests)

(def-suite progress-tests
  :description "Tests for the progress bar component."
  :in tuition-tests)

(in-suite progress-tests)

(defun progress-sgr-count (string)
  "Count the number of distinct SGR color sequences (ESC[38;2;...) in STRING."
  (count #\Escape string))

(test progress-plain-has-no-color
  "A progress bar without colors renders plain (no escape sequences)."
  (let ((bar (tui.progress:make-progress :percent 0.5 :width 10 :show-percentage nil)))
    (let ((view (tui.progress:progress-view bar)))
      (is (zerop (progress-sgr-count view)))
      (is (search "[█████░░░░░]" view)))))

(test progress-solid-fill
  "A single color fills the whole run with one solid color."
  (let ((bar (tui.progress:make-progress :percent 1.0 :width 5 :show-percentage nil
                                         :colors (list "#FF0000"))))
    (let ((view (tui.progress:progress-view bar)))
      ;; Solid red SGR present; a solid run is one open SGR + one reset = 2.
      (is (search (format nil "~C[38;2;255;0;0m" #\Escape) view))
      (is (= 2 (progress-sgr-count view))))))

(test progress-gradient-spans-stops
  "Two color stops produce a gradient using both endpoints."
  (let ((bar (tui.progress:make-progress :percent 1.0 :width 4 :show-percentage nil
                                         :colors (list "#FF0000" "#0000FF"))))
    (let ((view (tui.progress:progress-view bar)))
      ;; First stop (red) and last stop (blue) both appear.
      (is (search (format nil "~C[38;2;255;0;0m" #\Escape) view))
      (is (search (format nil "~C[38;2;0;0;255m" #\Escape) view))
      ;; Gradient emits more than one color segment.
      (is (< 1 (progress-sgr-count view))))))

(test progress-gradient-distributes-evenly
  "A 3-stop gradient hits all three stops across the fill."
  (let ((bar (tui.progress:make-progress :percent 1.0 :width 6 :show-percentage nil
                                         :colors (list "#FF0000" "#00FF00" "#0000FF"))))
    (let ((view (tui.progress:progress-view bar)))
      ;; All three stops appear: red start, green middle, blue end.
      (is (search (format nil "~C[38;2;255;0;0m" #\Escape) view))
      (is (search (format nil "~C[38;2;0;255;0m" #\Escape) view))
      (is (search (format nil "~C[38;2;0;0;255m" #\Escape) view)))))

(test progress-empty-color
  "EMPTY-COLOR colors the empty portion."
  (let ((bar (tui.progress:make-progress :percent 0.5 :width 4 :show-percentage nil
                                         :empty-color "#444444")))
    (let ((view (tui.progress:progress-view bar)))
      (is (search (format nil "~C[38;2;68;68;68m" #\Escape) view)))))

(test progress-still-shows-percentage
  "Percentage text is still rendered alongside the bar."
  (let ((bar (tui.progress:make-progress :percent 0.25 :width 40
                                         :colors (list "#FF0000" "#0000FF"))))
    (is (search "25%" (tui.progress:progress-view bar)))))

(test progress-zero-percent-no-fill
  "At 0% the gradient renders nothing colored."
  (let ((bar (tui.progress:make-progress :percent 0.0 :width 4 :show-percentage nil
                                         :colors (list "#FF0000" "#0000FF"))))
    (let ((view (tui.progress:progress-view bar)))
      (is (zerop (progress-sgr-count view))))))
