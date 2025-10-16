;;;; SPDX-License-Identifier: MIT

(defsystem "tuition"
  :description "A Common Lisp library for building TUIs"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "1.0.1"
  :depends-on ("bordeaux-threads"
               "trivial-channels"
               "version-string"
               "alexandria"
               "serapeum")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "protocol")
                             (:file "errors")
                             (:file "thread-pool")
                             (:file "style")
                             (:file "borders")
                             (:file "layout")
                             (:file "zone")
                             (:file "terminal")
                             (:file "input")
                             (:file "keybinding")
                             (:file "renderer")
                             (:file "program")
                             (:file "spring")
                             (:file "markdown")
                             ;; Sub-packages
                             (:file "table")
                             (:file "list")
                             (:file "tree")))
               (:module "src/components"
                :components ((:file "textinput")
                             (:file "viewport")
                             (:file "textarea")
                             (:file "paginator")
                             (:file "spinner")
                             (:file "progress")
                             (:file "stopwatch")
                             (:file "timer")
                             (:file "help"))))
  :in-order-to ((test-op (test-op "tuition/tests"))))

(defsystem "tuition/tests"
  :description "Test system for Tuition"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on ("tuition")
  :components ((:module "tests"
                :components ((:file "basic"))))
  :perform (test-op (o c)
                    (symbol-call :tuition-tests :run-tests)))
