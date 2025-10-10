;;;; SPDX-License-Identifier: MIT
;;;; Zone system - mouse zone tracking

(in-package #:tuition)

;;; Zone markers use ANSI escape sequences that are invisible and don't affect
;;; lipgloss width calculations. Format: ESC[<number>z

(defparameter *marker-counter* 1000
  "Counter for generating unique zone markers.")

(defparameter *prefix-counter* 0
  "Counter for generating unique zone prefixes.")

(defclass zone-manager ()
  ((enabled :initform t :accessor zone-manager-enabled)
   (zones :initform (make-hash-table :test 'equal)
          :accessor zone-manager-zones
          :documentation "Map of user ID -> zone-info")
   (ids :initform (make-hash-table :test 'equal)
        :accessor zone-manager-ids
        :documentation "Map of user ID -> generated marker ID")
   (rids :initform (make-hash-table :test 'equal)
         :accessor zone-manager-rids
         :documentation "Map of generated marker ID -> user ID")
   (lock :initform (bordeaux-threads:make-lock "zone-manager-lock")
         :accessor zone-manager-lock))
  (:documentation "Manager for tracking mouse zones in TUI applications."))

(defclass zone-info ()
  ((id :initarg :id :accessor zone-info-id)
   (start-x :initarg :start-x :accessor zone-info-start-x :initform 0)
   (start-y :initarg :start-y :accessor zone-info-start-y :initform 0)
   (end-x :initarg :end-x :accessor zone-info-end-x :initform 0)
   (end-y :initarg :end-y :accessor zone-info-end-y :initform 0))
  (:documentation "Information about a mouse zone's position and bounds."))

;;; Global zone manager
(defvar *zone-manager* nil
  "Global zone manager instance.")

(defun make-zone-manager ()
  "Create a new zone manager."
  (make-instance 'zone-manager))

(defun init-global-zone-manager ()
  "Initialize the global zone manager."
  (unless *zone-manager*
    (setf *zone-manager* (make-zone-manager)))
  *zone-manager*)

(defun zone-enabled-p (&optional (manager *zone-manager*))
  "Check if zone manager is enabled."
  (and manager (zone-manager-enabled manager)))

(defun zone-set-enabled (enabled &optional (manager *zone-manager*))
  "Enable or disable the zone manager."
  (when manager
    (setf (zone-manager-enabled manager) enabled)
    (unless enabled
      ;; Clear all zones when disabling
      (bordeaux-threads:with-lock-held ((zone-manager-lock manager))
        (clrhash (zone-manager-zones manager))))))

(defun zone-new-prefix (&optional (manager *zone-manager*))
  "Generate a unique zone prefix to prevent ID collisions."
  (declare (ignore manager))
  (format nil "zone_~D__" (incf *prefix-counter*)))

(defun zone-mark (id text &optional (manager *zone-manager*))
  "Mark text with zone markers for mouse tracking.
   Returns text wrapped with invisible ANSI markers."
  (if (or (not manager)
          (not (zone-manager-enabled manager))
          (string= id "")
          (string= text ""))
      text
      (let ((gid nil))
        ;; Get or create marker ID
        (bordeaux-threads:with-lock-held ((zone-manager-lock manager))
          (setf gid (gethash id (zone-manager-ids manager)))
          (unless gid
            ;; Generate new marker: ESC[<number>z
            (setf gid (format nil "~C[~Dz" #\Escape (incf *marker-counter*)))
            (setf (gethash id (zone-manager-ids manager)) gid)
            (setf (gethash gid (zone-manager-rids manager)) id)))
        ;; Wrap text with markers
        (format nil "~A~A~A" gid text gid))))

(defun zone-clear (id &optional (manager *zone-manager*))
  "Remove stored zone information for the given ID."
  (when manager
    (bordeaux-threads:with-lock-held ((zone-manager-lock manager))
      (remhash id (zone-manager-zones manager)))))

(defun zone-get (id &optional (manager *zone-manager*))
  "Get zone information for the given ID. Returns nil if not found."
  (when manager
    (bordeaux-threads:with-lock-held ((zone-manager-lock manager))
      (gethash id (zone-manager-zones manager)))))

;;; Zone scanning

(defun zone-scan (text &optional (manager *zone-manager*))
  "Scan text for zone markers, extract zone positions, and return cleaned text.
   This should be called at the root model's view output."
  (if (not manager)
      text
      (let ((zones (make-hash-table :test 'equal))
            (output (make-array (length text) :element-type 'character
                                :adjustable t :fill-pointer 0))
            (x 0)
            (y 0)
            (i 0))

        (loop while (< i (length text))
              do (let ((ch (char text i)))
                   (cond
                     ;; Check for ESC sequences (including zone markers)
                     ((char= ch #\Escape)
                      (if (and (< (1+ i) (length text))
                               (char= (char text (1+ i)) #\[))
                          ;; CSI sequence - check if it's a zone marker
                          (let ((start (+ i 2))
                                (marker-id nil)
                                (found-marker nil))
                            ;; Parse number
                            (loop while (and (< start (length text))
                                             (digit-char-p (char text start)))
                                  do (incf start))
                            ;; Check for 'z' terminator (zone marker)
                            (when (and (< start (length text))
                                       (char= (char text start) #\z))
                              ;; Extract marker ID
                              (setf marker-id (subseq text i (1+ start)))
                              ;; Get user ID from reverse map
                              (let ((user-id (bordeaux-threads:with-lock-held
                                                 ((zone-manager-lock manager))
                                               (gethash marker-id (zone-manager-rids manager)))))
                                (when user-id
                                  (setf found-marker t)
                                  (let ((zone (gethash user-id zones)))
                                    (if zone
                                        ;; End marker - update end position
                                        (progn
                                          (setf (zone-info-end-x zone) (max 0 (1- x)))
                                          (setf (zone-info-end-y zone) y))
                                        ;; Start marker - create new zone info
                                        (setf (gethash user-id zones)
                                              (make-instance 'zone-info
                                                             :id user-id
                                                             :start-x x
                                                             :start-y y)))))))
                            ;; If zone marker, skip it; otherwise copy CSI sequence
                            (if found-marker
                                (setf i (1+ start))
                                ;; Regular CSI sequence - copy without affecting X
                                (progn
                                  (vector-push-extend ch output)
                                  (incf i)
                                  ;; Copy the '['
                                  (when (< i (length text))
                                    (vector-push-extend (char text i) output)
                                    (incf i))
                                  ;; Copy rest of CSI sequence until final byte
                                  (loop while (< i (length text))
                                        for esc-ch = (char text i)
                                        do (vector-push-extend esc-ch output)
                                           (incf i)
                                           ;; Check for final byte (0x40-0x7E)
                                           (when (and (>= (char-code esc-ch) #x40)
                                                      (<= (char-code esc-ch) #x7E))
                                             (return))))))
                          ;; Other escape sequence - just copy the ESC
                          (progn
                            (vector-push-extend ch output)
                            (incf i))))

                     ;; Handle newlines
                     ((char= ch #\Newline)
                      (vector-push-extend ch output)
                      (incf y)
                      (setf x 0)
                      (incf i))

                     ;; Regular printable character
                     (t
                      (vector-push-extend ch output)
                      (incf x)
                      (incf i)))))

        ;; Store zones in manager
        (when (zone-manager-enabled manager)
          (bordeaux-threads:with-lock-held ((zone-manager-lock manager))
            (maphash (lambda (id zone-info)
                       (setf (gethash id (zone-manager-zones manager)) zone-info))
                     zones)))

        ;; Return cleaned output
        (coerce output 'string))))

;;; Zone info methods

(defmethod zone-in-bounds-p ((zone zone-info) (msg mouse-msg))
  "Check if a mouse message is within the bounds of a zone.
   Despite documentation saying mouse coords are 1-based, empirically they
   seem to need different handling. Mouse X is 1-based, but mouse Y appears
   to already match our 0-based line counting after accounting for rendering offset."
  (and zone
       (>= (zone-info-end-x zone) (zone-info-start-x zone))
       (>= (zone-info-end-y zone) (zone-info-start-y zone))
       (let ((mx (1- (mouse-msg-x msg)))  ; X is 1-based, convert to 0-based
             (my (- (mouse-msg-y msg) 2))) ; Y needs -2 offset (empirically determined)
         (and (>= mx (zone-info-start-x zone))
              (>= my (zone-info-start-y zone))
              (<= mx (zone-info-end-x zone))
              (<= my (zone-info-end-y zone))))))

(defmethod zone-in-bounds-p ((zone null) (msg mouse-msg))
  "nil zone is never in bounds."
  nil)

(defun zone-pos (zone msg)
  "Get mouse position relative to zone. Returns (values x y) or (values -1 -1)."
  (if (and zone (zone-in-bounds-p zone msg))
      (values (- (mouse-msg-x msg) (zone-info-start-x zone))
              (- (mouse-msg-y msg) (zone-info-start-y zone)))
      (values -1 -1)))
