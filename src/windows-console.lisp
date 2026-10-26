;;; windows-console.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Windows Console API bindings for terminal control.
;;;;
;;;; This file is only compiled on Windows (see :if-feature in tuition.asd).
;;;; The body below is additionally reader-guarded so the CFFI definitions
;;;; are never even read elsewhere.

(in-package #:tuition)

#+win32
(progn

  ;;; Windows Console API Constants

  ;; GetStdHandle constants
  (defconstant +std-input-handle+ -10)
  (defconstant +std-output-handle+ -11)
  (defconstant +std-error-handle+ -12)

  ;; Console mode flags for input
  (defconstant +enable-echo-input+ #x0004)
  (defconstant +enable-insert-mode+ #x0020)
  (defconstant +enable-line-input+ #x0002)
  (defconstant +enable-mouse-input+ #x0010)
  (defconstant +enable-processed-input+ #x0001)
  (defconstant +enable-quick-edit-mode+ #x0040)
  (defconstant +enable-window-input+ #x0008)
  (defconstant +enable-virtual-terminal-input+ #x0200)
  (defconstant +enable-extended-flags+ #x0080)

  ;; Console mode flags for output
  (defconstant +enable-processed-output+ #x0001)
  (defconstant +enable-wrap-at-eol-output+ #x0002)
  (defconstant +enable-virtual-terminal-processing+ #x0004)
  (defconstant +disable-newline-auto-return+ #x0008)
  (defconstant +enable-lvb-grid-worldwide+ #x0010)

  ;;; Windows Console API FFI Bindings

  (cffi:define-foreign-library kernel32
    (t "kernel32.dll"))

  (cffi:use-foreign-library kernel32)

  (cffi:defcfun ("GetStdHandle" get-std-handle :convention :stdcall) :pointer
    (std-handle :int32))

  (cffi:defcfun ("GetConsoleMode" get-console-mode :convention :stdcall) :int
    (console-handle :pointer)
    (mode (:pointer :uint32)))

  (cffi:defcfun ("SetConsoleMode" set-console-mode :convention :stdcall) :int
    (console-handle :pointer)
    (mode :uint32))

  ;; CONSOLE_SCREEN_BUFFER_INFO with its COORD and SMALL_RECT members
  ;; flattened.  Every field is a SHORT or WORD, so the layout is unchanged.
  (cffi:defcstruct console-screen-buffer-info
    (size-x :short)
    (size-y :short)
    (cursor-x :short)
    (cursor-y :short)
    (attributes :unsigned-short)
    (window-left :short)
    (window-top :short)
    (window-right :short)
    (window-bottom :short)
    (max-window-x :short)
    (max-window-y :short))

  (cffi:defcfun ("GetConsoleScreenBufferInfo" get-console-screen-buffer-info
                 :convention :stdcall)
      :int
    (console-handle :pointer)
    (info (:pointer (:struct console-screen-buffer-info))))

  ;;; Lisp-friendly wrappers

  (defvar *original-input-mode* nil
    "Stores the original console input mode.")

  (defvar *original-output-mode* nil
    "Stores the original console output mode.")

  (defvar *stdin-handle* nil
    "Handle to standard input.")

  (defvar *stdout-handle* nil
    "Handle to standard output.")

  (defun init-console-handles ()
    "Initialize console handles if not already done."
    (unless *stdin-handle*
      (setf *stdin-handle* (get-std-handle +std-input-handle+)))
    (unless *stdout-handle*
      (setf *stdout-handle* (get-std-handle +std-output-handle+))))

  (defun win32-enter-raw-mode ()
    "Put the Windows console in raw mode for TUI applications."
    (init-console-handles)

    ;; Save original modes
    (cffi:with-foreign-objects ((input-mode :uint32)
                                (output-mode :uint32))
      (when (zerop (get-console-mode *stdin-handle* input-mode))
        (error "Failed to get console input mode"))
      (when (zerop (get-console-mode *stdout-handle* output-mode))
        (error "Failed to get console output mode"))

      (setf *original-input-mode* (cffi:mem-ref input-mode :uint32))
      (setf *original-output-mode* (cffi:mem-ref output-mode :uint32))

      ;; Set raw input mode: disable line input, echo, and processed input;
      ;; enable mouse input and virtual terminal input.
      (let ((new-input-mode (logior +enable-mouse-input+
                                    +enable-window-input+
                                    +enable-virtual-terminal-input+
                                    +enable-extended-flags+)))
        (when (zerop (set-console-mode *stdin-handle* new-input-mode))
          (error "Failed to set console input mode")))

      ;; Set raw output mode: enable virtual terminal processing for ANSI
      ;; escape codes.
      (let ((new-output-mode (logior +enable-processed-output+
                                     +enable-wrap-at-eol-output+
                                     +enable-virtual-terminal-processing+)))
        (when (zerop (set-console-mode *stdout-handle* new-output-mode))
          (error "Failed to set console output mode")))))

  (defun win32-exit-raw-mode ()
    "Restore the Windows console to its original state."
    (when *original-input-mode*
      (set-console-mode *stdin-handle* *original-input-mode*)
      (setf *original-input-mode* nil))
    (when *original-output-mode*
      (set-console-mode *stdout-handle* *original-output-mode*)
      (setf *original-output-mode* nil)))

  (defun win32-get-terminal-size ()
    "Get the current console size as (width . height)."
    (init-console-handles)
    (cffi:with-foreign-object (info '(:struct console-screen-buffer-info))
      (if (zerop (get-console-screen-buffer-info *stdout-handle* info))
          ;; Failed - return default
          (cons 80 24)
          ;; Success - calculate width and height from window rect
          (cffi:with-foreign-slots ((window-left window-top
                                     window-right window-bottom)
                                    info (:struct console-screen-buffer-info))
            (cons (1+ (- window-right window-left))
                  (1+ (- window-bottom window-top))))))))
