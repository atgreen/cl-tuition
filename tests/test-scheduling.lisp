;;;; SPDX-License-Identifier: MIT
(in-package #:tuition-tests)
(in-suite basic-tests)

#+tuition-single-threaded
(test cooperative-deadlines
  (let ((program (make-program nil))
        (fired nil))
    (setf (tuition::program-running program) t)
    (tuition::run-command program
                         (tick 60 (lambda () (push :late fired) nil)))
    (tuition::run-command program
                         (tick 30 (lambda () (push :early fired) nil)))
    (tuition::run-command program (lambda () (push :ready fired) nil))
    (dotimes (i 3) (tuition::process-one-pending-command program))
    (is (equal '(:ready) fired))
    (let ((first-time (caar (tuition::program-deadlines program))))
      (tuition::process-due-commands program (1- first-time))
      (is (not (tuition::process-one-pending-command program)))
      (tuition::process-due-commands program first-time)
      (tuition::process-one-pending-command program)
      (is (equal '(:early :ready) fired)))
    (tuition::process-due-commands
     program (caar (tuition::program-deadlines program)))
    (tuition::process-one-pending-command program)
    (is (equal '(:late :early :ready) fired))))

#+tuition-single-threaded
(test cooperative-sequence
  (let ((program (make-program nil))
        (fired nil))
    (setf (tuition::program-running program) t)
    (tuition::run-command
     program
     (cmd-sequence (tick 60 (lambda () (push :first fired) nil))
                   (lambda () (push :second fired) nil)))
    (tuition::process-one-pending-command program)
    (is (null fired))
    (is (not (tuition::process-one-pending-command program)))
    (tuition::process-due-commands
     program (caar (tuition::program-deadlines program)))
    (tuition::process-one-pending-command program)
    (is (equal '(:first) fired))
    (tuition::process-one-pending-command program)
    (is (equal '(:second :first) fired))
    (is (null (tuition::%channel-get (tuition::msg-channel program))))))

#+tuition-single-threaded
(test cooperative-resume-signal
  (let ((program (make-program nil)))
    (setf (tuition::program-running program) t
          (svref (tuition::program-signal-flags program) 2) t)
    (tuition::process-pending-signals program)
    (is (resume-msg-p
         (tuition::%channel-get (tuition::msg-channel program))))
    (tuition::process-pending-signals program)
    (is (null (tuition::%channel-get (tuition::msg-channel program))))))

#+tuition-single-threaded
(test bundled-timed-components
  (let* ((program (make-program nil))
         (spinner (tui.spinner:make-spinner :fps 60))
         (stopwatch (tui.stopwatch:make-stopwatch :interval 60))
         (timer (tui.timer:make-timer :interval 60)))
    (setf (tuition::program-running program) t)
    (dolist (cmd (list (tui.spinner:spinner-init spinner)
                       (tui.stopwatch:stopwatch-start stopwatch)
                       (tui.timer:timer-start timer)))
      (tuition::run-command program cmd)
      (tuition::process-one-pending-command program))
    (is (= 3 (length (tuition::program-deadlines program))))
    (is (null (tuition::%channel-get (tuition::msg-channel program))))
    (tuition::process-due-commands
     program (car (car (last (tuition::program-deadlines program)))))
    (dotimes (i 3) (tuition::process-one-pending-command program))
    (let ((msg (tuition::%channel-get (tuition::msg-channel program))))
      (is (tui.spinner:spinner-tick-msg-p msg))
      (multiple-value-bind (model cmd)
          (tui.spinner:spinner-update spinner msg)
        (declare (ignore model))
        (tuition::run-command program cmd)
        (tuition::process-one-pending-command program)
        (is (= 1 (length (tuition::program-deadlines program))))))
    (dotimes (i 2)
      (is (tick-msg-p
           (tuition::%channel-get (tuition::msg-channel program)))))))
