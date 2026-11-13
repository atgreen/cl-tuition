;;;; SPDX-License-Identifier: MIT

(defsystem "tuition"
  :description "A Common Lisp library for building TUIs"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "2.6.0"
  :depends-on ("version-string"
               "alexandria"
               "cl-base64"
               (:feature (:not :tuition-single-threaded) "bordeaux-threads")
               (:feature (:not :tuition-single-threaded) "trivial-channels")
               (:feature :win32 "cffi"))
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "concurrency"
                              :if-feature :tuition-single-threaded)
                             (:file "protocol")
                             (:file "errors")
                             (:file "thread-pool"
                              :if-feature (:not :tuition-single-threaded))
                             (:file "style")
                             (:file "borders")
                             (:file "layout")
                             (:file "overlay")
                             (:file "zone")
                             (:file "terminal")
                             (:file "windows-console" :if-feature :win32)
                             (:file "input")
                             (:file "keybinding")
                             (:file "cells")
                             (:file "renderer")
                             (:file "program")
                             (:file "compositor")
                             (:file "spring")
                             (:file "markdown")
                             ;; Sub-packages
                             (:file "table")
                             (:file "list")
                             (:file "tree")
                             (:file "compat-v1")))
               (:module "src/components"
                :components ((:file "textinput")
                             (:file "viewport")
                             (:file "textarea")
                             (:file "paginator")
                             (:file "spinner")
                             (:file "progress")
                             (:file "stopwatch")
                             (:file "timer")
                             (:file "help")
                             (:file "datepicker")
                             (:file "list")
                             (:file "table")
                             (:file "tree")
                             (:file "filepicker"))))
  :in-order-to ((test-op (test-op "tuition/tests"))))

(defsystem "tuition/tests"
  :description "Test system for Tuition"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on ("tuition" "fiveam")
  :serial t
  :components ((:module "tests"
                :components ((:file "basic")
                             (:file "test-scheduling")
                             (:file "golden")
                             (:file "test-style")
                             (:file "test-borders")
                             (:file "test-layout")
                             (:file "test-list")
                             (:file "test-table")
                             (:file "test-tree")
                             (:file "test-input")
                             (:file "test-text")
                             (:file "test-progress")
                             (:file "test-textarea")
                             (:file "test-textinput")
                             (:file "test-viewport")
                             (:file "test-list-component")
                             (:file "test-tree-component")
                             (:file "test-escapes")
                             (:file "test-filepicker"))))
  :perform (test-op (o c)
                    (symbol-call :tuition-tests :run-tests)))
