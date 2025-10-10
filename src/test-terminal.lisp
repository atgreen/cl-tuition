;;; test-terminal.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Test terminal I/O directly

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :tuition)

(format t "Testing raw mode...~%")
(force-output)

;; Enter raw mode
(tuition::enter-raw-mode)
(tuition::hide-cursor)

(format t "Type some keys (q to quit):~%")
(force-output)

(loop
  (let ((ch (read-char-no-hang *standard-input* nil nil)))
    (when ch
      (format t "~%Got char: ~S (code ~D)~%" ch (char-code ch))
      (force-output)
      (when (char= ch #\q)
        (return))))
  (sleep 0.01))

(tuition::show-cursor)
(tuition::exit-raw-mode)

(format t "~%Done!~%")
