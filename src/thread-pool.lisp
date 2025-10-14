;;; thread-pool.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Thread pool for bounded command execution

(in-package #:tuition)

;;; Thread Pool Implementation
;;;
;;; Unlike Go's Bubble Tea which uses lightweight goroutines, Common Lisp threads
;;; are OS threads with ~1-2MB overhead each. This thread pool provides bounded
;;; concurrency for command execution to prevent resource exhaustion.
;;;
;;; Design:
;;; - Fixed number of worker threads (default: 4)
;;; - Commands queued via trivial-channels
;;; - Workers block on queue, exit on shutdown signal
;;; - Graceful shutdown with thread joining

(defstruct thread-pool
  "A bounded pool of worker threads for executing commands."
  (size 4 :type fixnum :read-only t)
  (workers nil :type list)
  (queue nil)  ; trivial-channels:channel
  (running t :type boolean)
  (program nil)  ; Back-reference to program for sending messages
  (lock (bt:make-lock "thread-pool-lock")))

(defun make-pool (size program)
  "Create a thread pool with SIZE worker threads.

  PROGRAM is the program instance that commands will send messages to.
  Recommended sizes:
  - 2-4 for mostly timer-based applications
  - 4-8 for mixed I/O and timer applications
  - 8-16 for I/O-heavy applications

  Too many workers wastes resources; too few may cause head-of-line blocking."
  (let* ((queue (trivial-channels:make-channel))
         (pool (make-thread-pool :size size
                                 :queue queue
                                 :program program
                                 :running t)))
    ;; Start worker threads
    (setf (thread-pool-workers pool)
          (loop for i from 1 to size
                collect (bt:make-thread
                         (lambda () (worker-loop pool i))
                         :name (format nil "tuition-pool-~D" i))))
    pool))

(defun worker-loop (pool worker-id)
  "Main loop for a worker thread.

  Workers block on the command queue and execute commands as they arrive.
  Exit when receiving a nil command (shutdown signal)."
  (declare (ignore worker-id))  ; Reserved for future logging/debugging
  (handler-case
      (loop while (thread-pool-running pool) do
        (let ((cmd (trivial-channels:getmsg (thread-pool-queue pool))))
          (cond
            ;; Shutdown signal
            ((null cmd)
             (return))

            ;; Execute command
            (t
             (handler-case
                 (alexandria:when-let ((msg (funcall cmd)))
                   (send (thread-pool-program pool) msg))
               (error (e)
                 (handle-error :thread-pool e)))))))
    (error (e)
      ;; Outer error handler for catastrophic failures
      (handle-error :thread-pool-worker e))))

(defun submit-command (pool cmd)
  "Submit a command to the thread pool for execution.

  Returns T if the command was queued, NIL if the pool is shutting down."
  (when (and pool (thread-pool-running pool))
    (trivial-channels:sendmsg (thread-pool-queue pool) cmd)
    t))

(defun shutdown-pool (pool)
  "Gracefully shutdown the thread pool.

  Sends shutdown signals to all workers and waits for them to finish.
  This will block until all currently executing commands complete."
  (when pool
    (bt:with-lock-held ((thread-pool-lock pool))
      (setf (thread-pool-running pool) nil))

    ;; Send nil to each worker as shutdown signal
    (loop repeat (thread-pool-size pool)
          do (trivial-channels:sendmsg (thread-pool-queue pool) nil))

    ;; Wait for all workers to exit
    (dolist (worker (thread-pool-workers pool))
      (when (bt:thread-alive-p worker)
        (bt:join-thread worker)))

    ;; Clear worker list
    (setf (thread-pool-workers pool) nil)))

(defun pool-stats (pool)
  "Return statistics about the pool for debugging/monitoring.

  Returns a plist with:
    :size - Number of worker threads
    :alive - Number of currently alive workers
    :running - Whether the pool is accepting new commands"
  (list :size (thread-pool-size pool)
        :alive (count-if #'bt:thread-alive-p (thread-pool-workers pool))
        :running (thread-pool-running pool)))

;;; Integration with existing command system

(defparameter *default-pool-size* 4
  "Default number of worker threads in the command pool.

  Can be overridden via :pool-size option to make-program.")

(defparameter *use-thread-pool* t
  "When true, use thread pool for command execution.
  When false, spawn a new thread for each command (original behavior).

  The thread-per-command behavior matches Go's Bubble Tea but doesn't scale
  well in Common Lisp due to OS thread overhead. Thread pool is recommended
  for production use.")
