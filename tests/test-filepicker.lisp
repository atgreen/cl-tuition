;;; test-filepicker.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;;; Tests for the filepicker component (src/components/filepicker.lisp)

(in-package #:tuition-tests)

(def-suite filepicker-tests
  :description "Tests for the filepicker component."
  :in tuition-tests)

(in-suite filepicker-tests)

;;; --- test helpers ---

(defvar *fp-fixture* nil)

(defun fp-fixture-dir ()
  "Create (once) a small directory tree to pick from:
adir/, bdir/ (with one file), alpha.lisp, zeta.txt, .hidden"
  (or *fp-fixture*
      (let ((root (merge-pathnames
                   (format nil "tuition-fp-test-~D/" (get-universal-time))
                   (uiop:temporary-directory))))
        (ensure-directories-exist (merge-pathnames "adir/" root))
        (ensure-directories-exist (merge-pathnames "bdir/" root))
        (with-open-file (s (merge-pathnames "bdir/inner.txt" root)
                           :direction :output :if-does-not-exist :create)
          (write-string "inner" s))
        (with-open-file (s (merge-pathnames "alpha.lisp" root)
                           :direction :output :if-does-not-exist :create)
          (write-string "(defun hi ())" s))
        (with-open-file (s (merge-pathnames "zeta.txt" root)
                           :direction :output :if-does-not-exist :create)
          (write-string "zzz" s))
        (with-open-file (s (merge-pathnames ".hidden" root)
                           :direction :output :if-does-not-exist :create)
          (write-string "shh" s))
        (setf *fp-fixture* (uiop:native-namestring root)))))

(defun fp-make (&rest args)
  "Make a filepicker over the fixture directory and load its listing."
  (let ((fp (apply #'tui.filepicker:make-filepicker
                   :current-directory (fp-fixture-dir)
                   :auto-height nil
                   args)))
    (tui.filepicker:filepicker-update
     fp (funcall (tui.filepicker:filepicker-init fp)))
    fp))

(defun fp-key (fp code &optional mod)
  "Feed a key to the picker.  Returns the command (or nil)."
  (nth-value 1 (tui.filepicker:filepicker-update
                fp (make-key-press-msg :code code :mod (or mod 0)))))

(defun fp-names (fp)
  (mapcar #'tui.filepicker::%fp-entry-name
          (tui.filepicker::filepicker-files fp)))

;;; --- listing ---

(test filepicker-lists-sorted-dirs-first
  "Entries are sorted directories-first, then by name; dotfiles hidden."
  (let ((fp (fp-make :height 10)))
    (is (equal '("adir" "bdir" "alpha.lisp" "zeta.txt") (fp-names fp)))))

(test filepicker-show-hidden
  "With :show-hidden, dotfiles appear."
  (let ((fp (fp-make :height 10 :show-hidden t)))
    (is (member ".hidden" (fp-names fp) :test #'string=))))

(test filepicker-stale-listing-ignored
  "A read-dir message for another picker instance is ignored."
  (let ((fp (fp-make :height 10))
        (other (tui.filepicker:make-filepicker
                :current-directory (fp-fixture-dir))))
    (tui.filepicker:filepicker-update
     fp (funcall (tui.filepicker:filepicker-init other)))
    ;; Entries unchanged (same instance's listing still in place).
    (is (equal '("adir" "bdir" "alpha.lisp" "zeta.txt") (fp-names fp)))))

;;; --- windowing ---

(test filepicker-unset-height-shows-one-entry
  "With no height set, one entry is visible instead of a blank view
(bubbles #1026)."
  (let* ((fp (fp-make))
         (view (tui.filepicker:filepicker-view fp))
         (lines (remove "" (tuition:split-string-by-newline view)
                        :test #'string=)))
    (is (= 1 (length lines)))
    (is (search "adir" (first lines)))))

(test filepicker-window-follows-selection
  "Moving past the window bottom scrolls the window."
  (let ((fp (fp-make :height 2)))
    (is (= 0 (tui.filepicker::filepicker-min-idx fp)))
    (fp-key fp #\j) (fp-key fp #\j)          ; select index 2
    (is (= 2 (tui.filepicker:filepicker-selected fp)))
    (is (= 1 (tui.filepicker::filepicker-min-idx fp)))
    (is (= 2 (tui.filepicker::filepicker-max-idx fp)))
    (fp-key fp #\k) (fp-key fp #\k) (fp-key fp #\k)
    (is (= 0 (tui.filepicker:filepicker-selected fp)))
    (is (= 0 (tui.filepicker::filepicker-min-idx fp)))))

(test filepicker-go-to-top-and-last
  "g and G jump to the first and last entries."
  (let ((fp (fp-make :height 2)))
    (fp-key fp #\G)
    (is (= 3 (tui.filepicker:filepicker-selected fp)))
    (fp-key fp #\g)
    (is (= 0 (tui.filepicker:filepicker-selected fp)))))

;;; --- navigation ---

(test filepicker-open-descends-into-directory
  "Enter on a directory descends and returns a read-dir command."
  (let* ((fp (fp-make :height 10))
         (cmd (progn (fp-key fp #\j)          ; select "bdir"
                     (fp-key fp :enter))))
    (is (functionp cmd))
    (is (search "bdir" (tui.filepicker:filepicker-current-directory fp)))
    (tui.filepicker:filepicker-update fp (funcall cmd))
    (is (equal '("inner.txt") (fp-names fp)))))

(test filepicker-back-restores-view
  "Backspace returns to the parent and restores the previous selection."
  (let ((fp (fp-make :height 10)))
    (fp-key fp #\j)                           ; select "bdir" (index 1)
    (tui.filepicker:filepicker-update fp (funcall (fp-key fp :enter)))
    (let ((cmd (fp-key fp :backspace)))
      (is (functionp cmd))
      (tui.filepicker:filepicker-update fp (funcall cmd)))
    (is (= 1 (tui.filepicker:filepicker-selected fp)))
    (is (equal '("adir" "bdir" "alpha.lisp" "zeta.txt") (fp-names fp)))))

;;; --- selection ---

(test filepicker-select-file
  "Enter on a file records it, and did-select-file reports it."
  (let ((fp (fp-make :height 10)))
    (fp-key fp #\j) (fp-key fp #\j)           ; "alpha.lisp"
    (let ((msg (make-key-press-msg :code :enter :mod 0)))
      (tui.filepicker:filepicker-update fp msg)
      (multiple-value-bind (did path)
          (tui.filepicker:filepicker-did-select-file fp msg)
        (is (eq t did))
        (is (search "alpha.lisp" path))))))

(test filepicker-highlighted-path
  "The highlighted path tracks the selection without selecting."
  (let ((fp (fp-make :height 10)))
    (is (search "adir" (tui.filepicker:filepicker-highlighted-path fp)))
    (fp-key fp #\G)
    (is (search "zeta.txt" (tui.filepicker:filepicker-highlighted-path fp)))))

(test filepicker-allowed-types-disable-selection
  "Files excluded by :allowed-types can't be selected and report as disabled."
  (let ((fp (fp-make :height 10 :allowed-types '(".lisp"))))
    (fp-key fp #\G)                           ; "zeta.txt"
    (let ((msg (make-key-press-msg :code :enter :mod 0)))
      (tui.filepicker:filepicker-update fp msg)
      (is (null (tui.filepicker:filepicker-path fp)))
      (is (not (tui.filepicker:filepicker-did-select-file fp msg)))
      (is (not (tui.filepicker:filepicker-did-select-disabled-file fp msg))))
    ;; An allowed file still selects.
    (fp-key fp #\k)                           ; "alpha.lisp"
    (let ((msg (make-key-press-msg :code :enter :mod 0)))
      (tui.filepicker:filepicker-update fp msg)
      (multiple-value-bind (did path)
          (tui.filepicker:filepicker-did-select-file fp msg)
        (is (eq t did))
        (is (search "alpha.lisp" path))))))

(test filepicker-dir-selection-requires-dir-allowed
  "Enter on a directory only records it when :dir-allowed."
  (let ((fp (fp-make :height 10)))
    (tui.filepicker:filepicker-update fp (make-key-press-msg :code :enter :mod 0))
    (is (null (tui.filepicker:filepicker-path fp))))
  (let ((fp (fp-make :height 10 :dir-allowed t)))
    (tui.filepicker:filepicker-update fp (make-key-press-msg :code :enter :mod 0))
    (is (search "adir" (tui.filepicker:filepicker-path fp)))))

;;; --- rendering ---

(test filepicker-view-renders-entries
  "The view shows the cursor on the selected row and all visible names."
  (let* ((fp (fp-make :height 10))
         (view (tui.filepicker:filepicker-view fp)))
    (is (search "adir" view))
    (is (search "zeta.txt" view))
    (is (search ">" view))))

(test filepicker-empty-directory-message
  "An empty directory shows the placeholder message."
  (let* ((empty (merge-pathnames
                 (format nil "tuition-fp-empty-~D/" (get-universal-time))
                 (uiop:temporary-directory)))
         (fp (progn (ensure-directories-exist empty)
                    (let ((fp (tui.filepicker:make-filepicker
                               :current-directory (uiop:native-namestring empty)
                               :auto-height nil :height 3)))
                      (tui.filepicker:filepicker-update
                       fp (funcall (tui.filepicker:filepicker-init fp)))
                      fp))))
    (is (search "No Files Found" (tui.filepicker:filepicker-view fp)))))

(test filepicker-window-size-sets-height
  "A window-size message drives the height when :auto-height."
  (let ((fp (tui.filepicker:make-filepicker
             :current-directory (fp-fixture-dir))))
    (tui.filepicker:filepicker-update
     fp (make-instance 'tuition:window-size-msg :width 80 :height 24))
    (is (= 19 (tui.filepicker:filepicker-height fp)))))
