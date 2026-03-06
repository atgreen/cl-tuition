;;; compositor.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;;; Advanced compositing system: Layer, Canvas, Compositor
;;;;
;;;; Provides z-ordered layer compositing with hit testing, ported from
;;;; the Lipgloss v2 Layer/Compositor/Canvas system.  Builds on the
;;;; cell-buffer infrastructure from cells.lisp.

(in-package #:tuition)

;;; ---------- Layer ----------

(defclass layer ()
  ((id      :initarg :id :initform "" :accessor layer-id)
   (content :initarg :content :initform "" :accessor layer-content)
   (x       :initarg :x :initform 0 :accessor layer-x)
   (y       :initarg :y :initform 0 :accessor layer-y)
   (z       :initarg :z :initform 0 :accessor layer-z)
   (lwidth  :initform 0 :accessor layer-width)
   (lheight :initform 0 :accessor layer-height)
   (children :initform nil :accessor layer-children))
  (:documentation "A positioned layer of styled content for compositing."))

(defun make-layer (content &rest children)
  "Create a layer with CONTENT and optional CHILDREN layers."
  (let ((layer (make-instance 'layer :content content)))
    ;; Calculate content bounds
    (multiple-value-bind (w h) (size content)
      (setf (layer-width layer) w
            (layer-height layer) h))
    ;; Add children
    (when children
      (layer-add-children layer children))
    layer))

(defun layer-set-id (layer id)
  "Set the layer ID (for hit testing). Returns the layer for chaining."
  (setf (layer-id layer) id)
  layer)

(defun layer-set-x (layer x)
  "Set the layer X position. Returns the layer."
  (setf (layer-x layer) x)
  layer)

(defun layer-set-y (layer y)
  "Set the layer Y position. Returns the layer."
  (setf (layer-y layer) y)
  layer)

(defun layer-set-z (layer z)
  "Set the layer Z index. Returns the layer."
  (setf (layer-z layer) z)
  layer)

(defun layer-add-children (layer children)
  "Add CHILDREN (a list of layers) to LAYER. Recalculates bounds."
  (setf (layer-children layer) (append (layer-children layer) children))
  ;; Recalculate bounds to encompass all children
  (%layer-recalculate-bounds layer)
  layer)

(defun %layer-recalculate-bounds (layer)
  "Recalculate layer bounds to encompass content and all children."
  (let ((max-x (layer-width layer))
        (max-y (layer-height layer)))
    (dolist (child (layer-children layer))
      (let ((child-right (+ (layer-x child) (layer-width child)))
            (child-bottom (+ (layer-y child) (layer-height child))))
        (setf max-x (max max-x child-right))
        (setf max-y (max max-y child-bottom))))
    (setf (layer-width layer) max-x
          (layer-height layer) max-y)))

(defun layer-get-layer (layer id)
  "Recursively search LAYER and its children for a layer with ID."
  (cond
    ((string= (layer-id layer) id) layer)
    (t (dolist (child (layer-children layer))
         (let ((found (layer-get-layer child id)))
           (when found (return found)))))))

(defun layer-max-z (layer)
  "Return the maximum z-index in LAYER's hierarchy."
  (let ((max-z (layer-z layer)))
    (dolist (child (layer-children layer))
      (setf max-z (max max-z (layer-max-z child))))
    max-z))

;;; ---------- Canvas ----------

(defclass canvas ()
  ((buffer :accessor canvas-buffer))
  (:documentation "An offscreen cell buffer for compositing."))

(defun make-canvas (width height)
  "Create a canvas of WIDTH x HEIGHT."
  (let ((c (make-instance 'canvas)))
    (setf (canvas-buffer c) (make-screen-buffer width height))
    c))

(defun canvas-width (canvas)
  "Return the width of CANVAS."
  (screen-buffer-width (canvas-buffer canvas)))

(defun canvas-height (canvas)
  "Return the height of CANVAS."
  (screen-buffer-height (canvas-buffer canvas)))

(defun canvas-cell-at (canvas x y)
  "Return the cell at (X, Y) in CANVAS."
  (screen-buffer-ref (canvas-buffer canvas) x y))

(defun canvas-set-cell (canvas x y cell)
  "Set the cell at (X, Y) in CANVAS."
  (setf (screen-buffer-ref (canvas-buffer canvas) x y) cell))

(defun canvas-compose (canvas layer)
  "Draw LAYER onto CANVAS at the layer's x,y position. Returns CANVAS."
  ;; Parse the layer's content into a temporary buffer
  (let* ((lw (layer-width layer))
         (lh (layer-height layer))
         (cw (canvas-width canvas))
         (ch (canvas-height canvas))
         (ox (layer-x layer))
         (oy (layer-y layer)))
    (when (and (> lw 0) (> lh 0))
      (let ((src-buf (parse-styled-string (layer-content layer) lw lh)))
        ;; Copy non-blank cells from src to canvas
        (dotimes (sy lh)
          (let ((dy (+ oy sy)))
            (when (and (>= dy 0) (< dy ch))
              (dotimes (sx lw)
                (let ((dx (+ ox sx)))
                  (when (and (>= dx 0) (< dx cw))
                    (let ((src-cell (screen-buffer-ref src-buf sx sy)))
                      ;; Only copy non-blank cells (don't overwrite with spaces)
                      (unless (and (char= (cell-char src-cell) #\Space)
                                   (null (cell-fg src-cell))
                                   (null (cell-bg src-cell))
                                   (= (cell-attrs src-cell) 0))
                        (let ((dst-cell (screen-buffer-ref (canvas-buffer canvas) dx dy)))
                          (setf (cell-char dst-cell) (cell-char src-cell)
                                (cell-width dst-cell) (cell-width src-cell)
                                (cell-fg dst-cell) (cell-fg src-cell)
                                (cell-bg dst-cell) (cell-bg src-cell)
                                (cell-attrs dst-cell) (cell-attrs src-cell)
                                (cell-link dst-cell) (cell-link src-cell)))))))))))
        ;; Compose children recursively
        (dolist (child (layer-children layer))
          ;; Children are positioned relative to parent
          (let ((saved-x (layer-x child))
                (saved-y (layer-y child)))
            (setf (layer-x child) (+ ox saved-x)
                  (layer-y child) (+ oy saved-y))
            (canvas-compose canvas child)
            (setf (layer-x child) saved-x
                  (layer-y child) saved-y))))))
  canvas)

(defun canvas-render (canvas)
  "Render CANVAS to an ANSI-styled string."
  (let* ((buf (canvas-buffer canvas))
         (w (screen-buffer-width buf))
         (h (screen-buffer-height buf)))
    (with-output-to-string (s)
      (dotimes (y h)
        (when (> y 0) (write-char #\Newline s))
        (let ((cur-fg nil) (cur-bg nil) (cur-attrs 0))
          (dotimes (x w)
            (let ((cell (screen-buffer-ref buf x y)))
              (unless (= (cell-width cell) 0) ; skip trailing wide-char cells
                ;; Emit SGR if style changed
                (when (or (not (equal (cell-fg cell) cur-fg))
                          (not (equal (cell-bg cell) cur-bg))
                          (/= (cell-attrs cell) cur-attrs))
                  (let ((parts (%sgr-from-cell cell)))
                    (if parts
                        (format s "~C[0;~{~A~^;~}m" #\Escape parts)
                        (format s "~C[0m" #\Escape)))
                  (setf cur-fg (cell-fg cell)
                        cur-bg (cell-bg cell)
                        cur-attrs (cell-attrs cell)))
                (write-char (cell-char cell) s))))
          ;; Reset at end of line if styled
          (when (or cur-fg cur-bg (/= cur-attrs 0))
            (format s "~C[0m" #\Escape)))))))

;;; ---------- Compositor ----------

(defstruct (composite-layer (:constructor %make-composite-layer))
  "Flattened layer with absolute coordinates."
  (layer nil)
  (abs-x 0 :type fixnum)
  (abs-y 0 :type fixnum)
  (bounds-x 0 :type fixnum)
  (bounds-y 0 :type fixnum)
  (bounds-w 0 :type fixnum)
  (bounds-h 0 :type fixnum))

(defclass compositor ()
  ((root   :accessor compositor-root)
   (layers :initform nil :accessor compositor-layers
           :documentation "Flattened list of composite-layers sorted by z")
   (idx    :initform (make-hash-table :test 'equal) :accessor compositor-index
           :documentation "Hash-table: id -> layer")
   (bounds-w :initform 0 :accessor compositor-bounds-w)
   (bounds-h :initform 0 :accessor compositor-bounds-h))
  (:documentation "Manages multiple layers for z-ordered compositing with hit testing."))

(defun make-compositor (&rest layers)
  "Create a compositor with the given LAYERS."
  (let ((comp (make-instance 'compositor)))
    ;; Create a root layer containing all provided layers
    (let ((root (make-instance 'layer :content "")))
      (setf (layer-children root) layers)
      (setf (compositor-root comp) root))
    (%compositor-flatten comp)
    comp))

(defun compositor-add-layers (comp &rest layers)
  "Add LAYERS to the compositor. Returns COMP."
  (setf (layer-children (compositor-root comp))
        (append (layer-children (compositor-root comp)) layers))
  (%compositor-flatten comp)
  comp)

(defun %compositor-flatten (comp)
  "Flatten the layer hierarchy into a sorted list."
  (let ((flat nil)
        (index (make-hash-table :test 'equal))
        (max-x 0) (max-y 0))
    (labels ((walk (layer parent-x parent-y)
               (let ((abs-x (+ parent-x (layer-x layer)))
                     (abs-y (+ parent-y (layer-y layer))))
                 ;; Add this layer
                 (push (%make-composite-layer
                        :layer layer
                        :abs-x abs-x
                        :abs-y abs-y
                        :bounds-x abs-x
                        :bounds-y abs-y
                        :bounds-w (layer-width layer)
                        :bounds-h (layer-height layer))
                       flat)
                 ;; Update bounds
                 (setf max-x (max max-x (+ abs-x (layer-width layer))))
                 (setf max-y (max max-y (+ abs-y (layer-height layer))))
                 ;; Index by ID
                 (when (and (layer-id layer) (not (string= (layer-id layer) "")))
                   (setf (gethash (layer-id layer) index) layer))
                 ;; Walk children
                 (dolist (child (layer-children layer))
                   (walk child abs-x abs-y)))))
      (dolist (child (layer-children (compositor-root comp)))
        (walk child 0 0)))
    ;; Sort by z-index
    (setf (compositor-layers comp)
          (stable-sort (nreverse flat) #'<
                       :key (lambda (cl) (layer-z (composite-layer-layer cl)))))
    (setf (compositor-index comp) index)
    (setf (compositor-bounds-w comp) max-x)
    (setf (compositor-bounds-h comp) max-y)))

(defun compositor-hit (comp x y)
  "Return (values id layer) for the topmost layer at (X, Y), or NIL."
  (let ((layers (compositor-layers comp)))
    ;; Iterate in reverse z-order (topmost first)
    (dolist (cl (reverse layers))
      (let ((layer (composite-layer-layer cl))
            (bx (composite-layer-bounds-x cl))
            (by (composite-layer-bounds-y cl))
            (bw (composite-layer-bounds-w cl))
            (bh (composite-layer-bounds-h cl)))
        (when (and (not (string= (layer-id layer) ""))
                   (>= x bx) (< x (+ bx bw))
                   (>= y by) (< y (+ by bh)))
          (return-from compositor-hit (values (layer-id layer) layer))))))
  nil)

(defun compositor-get-layer (comp id)
  "Look up a layer by ID in the compositor."
  (gethash id (compositor-index comp)))

(defun compositor-render (comp)
  "Render all layers to a string via a canvas."
  (let* ((w (max 1 (compositor-bounds-w comp)))
         (h (max 1 (compositor-bounds-h comp)))
         (canvas (make-canvas w h)))
    ;; Draw layers in z-order (lowest first)
    (dolist (cl (compositor-layers comp))
      (let ((layer (composite-layer-layer cl)))
        (when (> (length (layer-content layer)) 0)
          ;; Temporarily set absolute position for drawing
          (let ((saved-x (layer-x layer))
                (saved-y (layer-y layer)))
            (setf (layer-x layer) (composite-layer-abs-x cl)
                  (layer-y layer) (composite-layer-abs-y cl))
            ;; Draw just this layer's content (not children, they're flattened)
            (let* ((lw (layer-width layer))
                   (lh (layer-height layer))
                   (cw (canvas-width canvas))
                   (ch (canvas-height canvas))
                   (ox (layer-x layer))
                   (oy (layer-y layer)))
              (when (and (> lw 0) (> lh 0))
                (let ((src-buf (parse-styled-string (layer-content layer) lw lh)))
                  (dotimes (sy lh)
                    (let ((dy (+ oy sy)))
                      (when (and (>= dy 0) (< dy ch))
                        (dotimes (sx lw)
                          (let ((dx (+ ox sx)))
                            (when (and (>= dx 0) (< dx cw))
                              (let ((src-cell (screen-buffer-ref src-buf sx sy)))
                                (unless (and (char= (cell-char src-cell) #\Space)
                                             (null (cell-fg src-cell))
                                             (null (cell-bg src-cell))
                                             (= (cell-attrs src-cell) 0))
                                  (let ((dst-cell (screen-buffer-ref (canvas-buffer canvas) dx dy)))
                                    (setf (cell-char dst-cell) (cell-char src-cell)
                                          (cell-width dst-cell) (cell-width src-cell)
                                          (cell-fg dst-cell) (cell-fg src-cell)
                                          (cell-bg dst-cell) (cell-bg src-cell)
                                          (cell-attrs dst-cell) (cell-attrs src-cell)
                                          (cell-link dst-cell) (cell-link src-cell))))))))))))))
            (setf (layer-x layer) saved-x
                  (layer-y layer) saved-y)))))
    (canvas-render canvas)))

(defun compositor-refresh (comp)
  "Re-flatten the layer hierarchy after mutations."
  (%compositor-flatten comp))
