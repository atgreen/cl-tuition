;;; windows-console.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Windows Console API bindings for terminal control.
;;;;
;;;; This file is only compiled on Windows (see :if-feature in tuition.asd).
;;;; The body below is additionally reader-guarded so the alien definitions
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

  #+sbcl
  (progn
    (sb-alien:define-alien-routine ("GetStdHandle" get-std-handle)
        sb-alien:unsigned-long
      (std-handle sb-alien:long))

    (sb-alien:define-alien-routine ("GetConsoleMode" get-console-mode)
        sb-alien:int
      (console-handle sb-alien:unsigned-long)
      (mode (sb-alien:* sb-alien:unsigned-long)))

    (sb-alien:define-alien-routine ("SetConsoleMode" set-console-mode)
        sb-alien:int
      (console-handle sb-alien:unsigned-long)
      (mode sb-alien:unsigned-long))

    (sb-alien:define-alien-type console-screen-buffer-info
        (sb-alien:struct console-screen-buffer-info
                         (size (sb-alien:struct coord
                                                (x sb-alien:short)
                                                (y sb-alien:short)))
                         (cursor-position (sb-alien:struct cursor-coord
                                                           (x sb-alien:short)
                                                           (y sb-alien:short)))
                         (attributes sb-alien:unsigned-short)
                         (window (sb-alien:struct small-rect
                                                  (left sb-alien:short)
                                                  (top sb-alien:short)
                                                  (right sb-alien:short)
                                                  (bottom sb-alien:short)))
                         (max-window-size (sb-alien:struct max-coord
                                                           (x sb-alien:short)
                                                           (y sb-alien:short)))))

    (sb-alien:define-alien-routine
        ("GetConsoleScreenBufferInfo" get-console-screen-buffer-info)
        sb-alien:int
      (console-handle sb-alien:unsigned-long)
      (info (sb-alien:* console-screen-buffer-info))))

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
    (sb-alien:with-alien ((input-mode sb-alien:unsigned-long)
                          (output-mode sb-alien:unsigned-long))
      (when (zerop (get-console-mode *stdin-handle*
                                     (sb-alien:addr input-mode)))
        (error "Failed to get console input mode"))
      (when (zerop (get-console-mode *stdout-handle*
                                     (sb-alien:addr output-mode)))
        (error "Failed to get console output mode"))

      (setf *original-input-mode* input-mode)
      (setf *original-output-mode* output-mode)

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
    (sb-alien:with-alien ((info console-screen-buffer-info))
      (if (zerop (get-console-screen-buffer-info *stdout-handle*
                                                 (sb-alien:addr info)))
          ;; Failed - return default
          (cons 80 24)
          ;; Success - calculate width and height from window rect
          (let* ((window (sb-alien:slot info 'window))
                 (left (sb-alien:slot window 'left))
                 (right (sb-alien:slot window 'right))
                 (top (sb-alien:slot window 'top))
                 (bottom (sb-alien:slot window 'bottom))
                 (width (1+ (- right left)))
                 (height (1+ (- bottom top))))
            (cons width height))))))
