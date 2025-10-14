;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(defpackage #:tuition
  (:use #:cl)
  (:nicknames #:tui)
  (:documentation "Tuition: A Common Lisp library for building TUIs.")
  (:export
   ;; Core protocol
   #:model
   #:init
   #:update
   #:update-message
   #:view

   ;; Messages and commands
   #:msg
   #:cmd
   #:message
   #:defmessage
   #:batch
   #:cmd-sequence

   ;; Program
   #:program
   #:make-program
   #:run
   #:send
   #:quit
   #:kill-program
   #:stop
   #:join
   #:with-raw-terminal
   #:defprogram

   ;; Built-in messages
   #:quit-msg
   #:quit-msg-p
   #:make-quit-msg
   #:key-msg
   #:key-msg-p
   #:key-msg-key
   #:key-msg-alt
   #:key-msg-ctrl
    #:make-key-msg
   #:key-string
   #:paste-msg
   #:paste-msg-p
   #:paste-msg-text
    #:make-paste-msg
   #:window-size-msg
   #:window-size-msg-p
   #:window-size-msg-width
   #:window-size-msg-height
    #:make-window-size-msg
   #:mouse-msg
   #:mouse-msg-p
   #:mouse-msg-x
   #:mouse-msg-y
   #:mouse-msg-button
   #:mouse-msg-action
   #:mouse-msg-shift
   #:mouse-msg-alt
   #:mouse-msg-ctrl
    #:make-mouse-msg
   #:tick-msg
   #:tick-msg-p
   #:tick-msg-time
    #:make-tick-msg
   #:suspend-msg
   #:suspend-msg-p
   #:make-suspend-msg
   #:resume-msg
   #:resume-msg-p
   #:make-resume-msg

   ;; Commands
   #:quit-cmd
   #:tick

   ;; Program options are keyword args to MAKE-PROGRAM

   ;; Styling
   #:style
   #:make-style
   #:render-styled
   #:bold
   #:italic
   #:underline
   #:colored

   ;; Reflow utilities
   #:wrap-text
   #:truncate-text
   #:ellipsize
   #:indent-lines

   ;; Measurement utilities
   #:width
   #:height
   #:size
   #:visible-length
   #:split-string-by-newline

   ;; Colors - 16 color
   #:*fg-black* #:*fg-red* #:*fg-green* #:*fg-yellow*
   #:*fg-blue* #:*fg-magenta* #:*fg-cyan* #:*fg-white*
   #:*fg-bright-black* #:*fg-bright-red* #:*fg-bright-green* #:*fg-bright-yellow*
   #:*fg-bright-blue* #:*fg-bright-magenta* #:*fg-bright-cyan* #:*fg-bright-white*
   #:*bg-black* #:*bg-red* #:*bg-green* #:*bg-yellow*
   #:*bg-blue* #:*bg-magenta* #:*bg-cyan* #:*bg-white*
   #:*bg-bright-black* #:*bg-bright-red* #:*bg-bright-green* #:*bg-bright-yellow*
   #:*bg-bright-blue* #:*bg-bright-magenta* #:*bg-bright-cyan* #:*bg-bright-white*

   ;; Extended colors
   #:color-256
   #:color-rgb
   #:parse-hex-color

   ;; Adaptive colors
   #:adaptive-color
   #:make-adaptive-color
   #:detect-dark-background
   #:detect-color-support

   ;; Complete colors
   #:complete-color
   #:make-complete-color
   #:complete-adaptive-color
   #:make-complete-adaptive-color

   ;; Layout utilities
   #:join-horizontal
   #:join-vertical
   #:place-horizontal
   #:place-vertical
   #:place
   #:+top+ #:+middle+ #:+bottom+
   #:+left+ #:+center+ #:+right+

   ;; Borders
   #:border
   #:make-border
   #:render-border
   #:*border-normal*
   #:*border-rounded*
   #:*border-thick*
   #:*border-double*
   #:*border-block*
   #:*border-hidden*
   #:*border-ascii*
   #:*border-markdown*

   ;; Zones (mouse tracking)
   #:*zone-manager*
   #:focus-in-msg-p
   #:focus-out-msg-p
   #:init-global-zone-manager
   #:make-focus-in-msg
   #:make-focus-out-msg
   #:make-zone-manager
   #:mouse-button-event
   #:mouse-drag-event-p
   #:mouse-event
   #:mouse-event-alt
   #:mouse-event-button
   #:mouse-event-ctrl
   #:mouse-event-shift
   #:mouse-event-x
   #:mouse-event-y
   #:mouse-move-event
   #:mouse-press-event
   #:mouse-press-event-p
   #:mouse-release-event
   #:mouse-release-event-p
   #:mouse-scroll-direction
   #:mouse-scroll-event
   #:mouse-scroll-event-p
   #:zone-clear
   #:zone-enabled-p
   #:zone-get
   #:zone-in-bounds-p
   #:zone-info
   #:zone-manager
   #:zone-mark
   #:zone-new-prefix
   #:zone-pos
   #:zone-scan
   #:zone-set-enabled

   ;; Spring physics (animations)
   #:spring
   #:make-spring
   #:make-spring-animation
   #:spring-update
   #:fps
   #:make-spring-smooth
   #:make-spring-bouncy
   #:make-spring-gentle
   #:make-spring-snappy

   ;; Markdown rendering
   #:markdown-style
   #:make-markdown-style
   #:make-style-dark
   #:make-style-light
   #:make-style-pink
   #:make-style-ascii
   #:markdown-renderer
   #:make-markdown-renderer
   #:render-markdown
   #:markdown

   ;; Terminal utilities
   #:get-terminal-size

   ;; Conditions and error handling
   #:tuition-error
   #:terminal-error
   #:terminal-operation-error
   #:input-error
   #:*error-handler*
   #:handle-error

   ;; Key bindings
   #:keybinding
   #:make-keybinding
   #:keybinding-keys
   #:keybinding-help-key
   #:keybinding-help-desc
   #:keybinding-enabled-p
   #:keybinding-enable
   #:keybinding-disable
   #:keybinding-matches
   #:keybinding-help-line
   #:keybindings-help
   #:make-quit-keybinding
   #:make-navigation-keybindings
   #:make-selection-keybindings
   #:make-scroll-keybindings
   #:key-matches

   ;; Version information
   #:+version+))

(in-package #:tuition)

;; Define version parameter using cl-version-string
(version-string:define-version-parameter +version+ :tuition)
