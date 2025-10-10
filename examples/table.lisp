;;;; SPDX-License-Identifier: MIT
;;;; Table example - demonstrates table rendering

(defpackage #:tuition-example-table
  (:use #:cl #:tuition)
  (:export #:main))

(in-package #:tuition-example-table)

;;; Model
(defclass table-model ()
  ((quitting :initform nil :accessor model-quitting)))

;;; Init
(defmethod tui:init ((model table-model))
  nil)

;;; Update (CLOS message dispatch)
(defmethod tui:update-message ((model table-model) (msg tui:key-msg))
  (let ((key (tui:key-msg-key msg)))
    (cond
      ((and (characterp key) (char= key #\q))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))
      ((and (tui:key-msg-ctrl msg) (characterp key) (char= key #\c))
       (setf (model-quitting model) t)
       (values model (tui:quit-cmd)))
      (t (values model nil)))))

;;; View
(defmethod tui:view ((model table-model))
  (let ((languages-table
         (tui.table:make-table
          :headers '("Language" "Formal" "Informal")
          :rows '(("Chinese" "您好" "你好")
                 ("Japanese" "こんにちは" "やあ")
                 ("Arabic" "أهلين" "أهلا")
                 ("Russian" "Здравствуйте" "Привет")
                 ("Spanish" "Hola" "¿Qué tal?"))
          :border-style tui:*border-normal*))

        (ascii-table
         (tui.table:make-table
          :headers '("ID" "Name" "Status")
          :rows '(("1" "Alice" "Active")
                 ("2" "Bob" "Inactive")
                 ("3" "Charlie" "Active"))
          :border-style tui:*border-ascii*)))

    (format nil "~%~A~%~%~
                 ~A~%~%~
                 ~A~%~%~
                 Press q to quit~%"
            (tui:bold "Table Examples")
            (tui.table:table-render languages-table)
            (tui.table:table-render ascii-table))))

;;; Main entry point
(defun main ()
  (let ((program (tui:make-program (make-instance 'table-model))))
    (tui:run program)))

#+nil
(main)
