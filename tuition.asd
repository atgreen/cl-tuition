;;;; SPDX-License-Identifier: MIT

(defsystem "tuition"
  :description "A Common Lisp library for building TUIs"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:bordeaux-threads
               #:trivial-channels
               #:version-string)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "protocol")
                             (:file "errors")
                             (:file "style")
                             (:file "borders")
                             (:file "layout")
                             (:file "zone")
                             (:file "terminal")
                             (:file "input")
                             (:file "renderer")
                             (:file "program")
                             (:file "spring")
                             (:file "markdown")
                             ;; Sub-packages
                             (:file "table")
                             (:file "list")
                             (:file "tree"))))
  :in-order-to ((test-op (test-op "tuition/components"))))

(defsystem "tuition/components"
  :description "Tuition UI components"
  :depends-on ("tuition")
  :serial t
  :components ((:module "src/components"
                :components ((:file "viewport")
                             (:file "textarea")
                             (:file "paginator")
                             (:file "help")))))
