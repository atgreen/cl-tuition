;;; windows-console.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Windows Console API bindings for terminal control

#+win32
(progn
  (in-package #:tuition)

  ;; Load Windows FFI support
  #+sbcl
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (require :sb-alien))

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
    (define-alien-routine ("GetStdHandle" get-std-handle) unsigned-long
      (std-handle long))

    (define-alien-routine ("GetConsoleMode" get-console-mode) int
      (console-handle unsigned-long)
      (mode (* unsigned-long)))

    (define-alien-routine ("SetConsoleMode" set-console-mode) int
      (console-handle unsigned-long)
      (mode unsigned-long))

    (define-alien-type console-screen-buffer-info
        (struct console-screen-buffer-info
                (size (struct coord (x short) (y short)))
                (cursor-position (struct coord (x short) (y short)))
                (attributes unsigned-short)
                (window (struct small-rect
                               (left short)
                               (top short)
                               (right short)
                               (bottom short)))
                (max-window-size (struct coord (x short) (y short)))))

    (define-alien-routine ("GetConsoleScreenBufferInfo" get-console-screen-buffer-info) int
      (console-handle unsigned-long)
      (info (* console-screen-buffer-info))))

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
    (with-alien ((input-mode unsigned-long)
                 (output-mode unsigned-long))
      (when (zerop (get-console-mode *stdin-handle* (addr input-mode)))
        (error "Failed to get console input mode"))
      (when (zerop (get-console-mode *stdout-handle* (addr output-mode)))
        (error "Failed to get console output mode"))

      (setf *original-input-mode* input-mode)
      (setf *original-output-mode* output-mode)

      ;; Set raw input mode
      ;; Disable line input, echo, and processed input
      ;; Enable mouse input and virtual terminal input
      (let ((new-input-mode (logior +enable-mouse-input+
                                   +enable-window-input+
                                   +enable-virtual-terminal-input+
                                   +enable-extended-flags+)))
        (when (zerop (set-console-mode *stdin-handle* new-input-mode))
          (error "Failed to set console input mode")))

      ;; Set raw output mode
      ;; Enable virtual terminal processing for ANSI escape codes
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
    (with-alien ((info console-screen-buffer-info))
      (if (zerop (get-console-screen-buffer-info *stdout-handle* (addr info)))
          ;; Failed - return default
          (cons 80 24)
          ;; Success - calculate width and height from window rect
          (let* ((window (slot info 'window))
                 (left (slot window 'left))
                 (right (slot window 'right))
                 (top (slot window 'top))
                 (bottom (slot window 'bottom))
                 (width (1+ (- right left)))
                 (height (1+ (- bottom top))))
            (cons width height))))))
