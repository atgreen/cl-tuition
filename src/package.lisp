;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(defpackage #:tuition
  (:use #:cl)
  (:nicknames #:tui)
  (:documentation "Tuition v2: A Common Lisp library for building TUIs.")
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
   #:program-input-paused
   #:*current-program*

   ;; Exec command (run external program with full TUI suspension)
   #:exec-cmd
   #:make-exec-cmd
   #:exec-cmd-p
   #:exec-cmd-program
   #:exec-cmd-args
   #:exec-cmd-callback

   ;; Built-in messages
   #:quit-msg
   #:quit-msg-p
   #:make-quit-msg
   #:paste-msg
   #:paste-msg-p
   #:paste-msg-text
   #:make-paste-msg
   #:window-size-msg
   #:window-size-msg-p
   #:window-size-msg-width
   #:window-size-msg-height
   #:make-window-size-msg
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

   ;; Modifier bitmask system
   #:+mod-shift+
   #:+mod-alt+
   #:+mod-ctrl+
   #:+mod-meta+
   #:+mod-hyper+
   #:+mod-super+
   #:+mod-caps-lock+
   #:+mod-num-lock+
   #:mod-contains
   #:make-mod

   ;; Key events (v2)
   #:key-event
   #:key-event-code
   #:key-event-mod
   #:key-event-text
   #:key-event-shifted-code
   #:key-event-base-code
   #:key-event-repeat-p
   #:key-press-msg
   #:key-press-msg-p
   #:make-key-press-msg
   #:key-release-msg
   #:key-release-msg-p
   #:make-key-release-msg
   #:key-string

   ;; Mouse events (v2)
   #:mouse-event
   #:mouse-event-p
   #:mouse-event-x
   #:mouse-event-y
   #:mouse-event-button
   #:mouse-event-mod
   #:mouse-click-msg
   #:mouse-click-msg-p
   #:make-mouse-click-msg
   #:mouse-release-msg
   #:mouse-release-msg-p
   #:make-mouse-release-msg
   #:mouse-motion-msg
   #:mouse-motion-msg-p
   #:make-mouse-motion-msg
   #:mouse-wheel-msg
   #:mouse-wheel-msg-p
   #:mouse-wheel-direction
   #:mouse-wheel-count
   #:make-mouse-wheel-msg

   ;; Focus events (v2)
   #:focus-msg
   #:focus-msg-p
   #:make-focus-msg
   #:blur-msg
   #:blur-msg-p
   #:make-blur-msg

   ;; Keyboard enhancements
   #:keyboard-enhancements-msg
   #:keyboard-enhancements-msg-p
   #:keyboard-enhancements-flags

   ;; Paste start/end messages (v2)
   #:paste-start-msg
   #:paste-start-msg-p
   #:make-paste-start-msg
   #:paste-end-msg
   #:paste-end-msg-p
   #:make-paste-end-msg

   ;; Color query messages
   #:background-color-msg
   #:background-color-msg-color
   #:foreground-color-msg
   #:foreground-color-msg-color
   #:cursor-color-msg
   #:cursor-color-msg-color
   #:make-cursor-color-msg
   #:cursor-position-msg
   #:cursor-position-msg-x
   #:cursor-position-msg-y

   ;; Terminal query messages (v2)
   #:terminal-version-msg
   #:terminal-version-msg-version
   #:make-terminal-version-msg
   #:mode-report-msg
   #:mode-report-msg-mode
   #:mode-report-msg-setting
   #:make-mode-report-msg

   ;; Clipboard messages (v2)
   #:clipboard-msg
   #:clipboard-msg-p
   #:clipboard-msg-content
   #:make-clipboard-msg

   ;; Commands
   #:quit-cmd
   #:tick
   #:request-background-color-cmd
   #:request-foreground-color-cmd
   #:request-cursor-color-cmd
   #:request-terminal-version-cmd
   #:request-mode-report-cmd

   ;; Clipboard commands (v2)
   #:set-clipboard-cmd
   #:read-clipboard-cmd
   #:set-primary-clipboard-cmd

   ;; Raw escape command (v2)
   #:raw-cmd

   ;; Key event utilities (v2)
   #:keystroke

   ;; Declarative view (v2)
   #:view-state
   #:view-state-p
   #:make-view
   #:view-state-content
   #:view-state-alt-screen
   #:view-state-mouse-mode
   #:view-state-report-focus
   #:view-state-window-title
   #:view-state-cursor
   #:view-state-foreground-color
   #:view-state-background-color
   #:view-state-keyboard-enhancements
   #:view-state-on-mouse
   #:view-state-disable-bracketed-paste
   #:view-state-unicode-mode
   #:cursor
   #:make-cursor
   #:cursor-x
   #:cursor-y
   #:cursor-shape
   #:cursor-color
   #:cursor-blink

   ;; Cell-based rendering (v2)
   #:cell
   #:make-cell
   #:blank-cell
   #:cell-char
   #:cell-width
   #:cell-fg
   #:cell-bg
   #:cell-attrs
   #:cell-link
   #:+attr-bold+
   #:+attr-italic+
   #:+attr-underline+
   #:+attr-blink+
   #:+attr-reverse+
   #:+attr-strikethrough+
   #:+attr-faint+
   #:screen-buffer
   #:make-screen-buffer
   #:screen-buffer-ref
   #:screen-buffer-width
   #:screen-buffer-height
   #:screen-buffer-clear
   #:screen-buffer-resize
   #:parse-styled-string

   ;; Compositor / Layer / Canvas (v2)
   #:layer
   #:make-layer
   #:layer-id
   #:layer-content
   #:layer-x
   #:layer-y
   #:layer-z
   #:layer-width
   #:layer-height
   #:layer-children
   #:layer-set-id
   #:layer-set-x
   #:layer-set-y
   #:layer-set-z
   #:layer-add-children
   #:layer-get-layer
   #:layer-max-z
   #:canvas
   #:make-canvas
   #:canvas-width
   #:canvas-height
   #:canvas-cell-at
   #:canvas-set-cell
   #:canvas-compose
   #:canvas-render
   #:compositor
   #:make-compositor
   #:compositor-add-layers
   #:compositor-hit
   #:compositor-get-layer
   #:compositor-render
   #:compositor-refresh

   ;; Styling
   #:style
   #:make-style
   #:copy-style
   #:render-styled
   #:bold
   #:italic
   #:underline
   #:colored

   ;; Extended underline styles (v2)
   #:style-underline-color

   ;; Hyperlink support (v2)
   #:style-hyperlink

   ;; Style accessors used by table renderer
   #:style-width

   ;; Custom fill characters (v2)
   #:style-padding-char
   #:style-margin-char

   ;; Color utilities (v2)
   #:darken-color
   #:lighten-color
   #:complementary-color
   #:blend-colors
   #:light-dark

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

   ;; Color conversion
   #:rgb-to-ansi256
   #:rgb-to-ansi16
   #:hex-to-ansi256
   #:hex-to-ansi16
   #:ansi256-to-hex
   #:as-foreground-code
   #:as-background-code

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
   #:resolve-color
   #:resolve-adaptive-color
   #:resolve-color-foreground
   #:resolve-color-background

   ;; Layout utilities
   #:join-horizontal
   #:join-vertical
   #:place-horizontal
   #:place-vertical
   #:place
   #:+top+ #:+middle+ #:+bottom+
   #:+left+ #:+center+ #:+right+

   ;; Overlay compositing
   #:composite
   #:composite-with-shadow
   #:overlay-centered
   #:overlay-at
   #:ansi-take-columns
   #:ansi-drop-columns

   ;; Borders
   #:border
   #:make-border
   #:render-border
   #:render-shadow
   #:*border-normal*
   #:*border-rounded*
   #:*border-thick*
   #:*border-double*
   #:*border-block*
   #:*border-hidden*
   #:*border-ascii*
   #:*border-markdown*

   ;; Zones (mouse tracking)
   #:zone-manager
   #:zone-info
   #:make-zone-manager
   #:init-global-zone-manager
   #:*zone-manager*
   #:zone-enabled-p
   #:zone-set-enabled
   #:zone-new-prefix
   #:zone-mark
   #:zone-clear
   #:zone-get
   #:zone-scan
   #:zone-in-bounds-p
   #:zone-pos

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
   #:make-markdown-style-from-colors
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
   #:set-terminal-title
   #:set-cursor-shape
   #:set-cursor-color
   #:request-background-color
   #:request-foreground-color
   #:enable-kitty-keyboard
   #:disable-kitty-keyboard
   #:request-kitty-keyboard

   ;; Unicode mode (v2)
   #:enable-unicode-mode
   #:disable-unicode-mode

   ;; Terminal queries (v2)
   #:request-terminal-version
   #:request-cursor-color
   #:request-mode-report

   ;; Clipboard (v2)
   #:set-clipboard
   #:read-clipboard
   #:set-primary-clipboard

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
