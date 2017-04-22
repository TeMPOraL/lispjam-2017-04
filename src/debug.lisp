(in-package #:lispjam-2017-04-temporal)

;;; Debug utilities

(defvar *debug-draw-vectors* nil "Toggle - draw debug vectors, or not.")
(defvar *debug-draw-perception* nil "Toggle - draw perception, or not.")

(defvar *debug-vectors* '())
(defvar *debug-circles* '())

(defparameter +debug-colors-alist+ `((:desired . ,(p2dg:make-color-4 0 0 0 1))
                                     (:cohesion . ,(p2dg:make-color-4 1 0 0 1))
                                     (:alignment . ,(p2dg:make-color-4 0 1 0 1))
                                     (:separation . ,(p2dg:make-color-4 0 0 1 1))
                                     (:avoid-player . ,(p2dg:make-color-4 1 0 1 1))
                                     (:food-chasing . ,(p2dg:make-color-4 1 1 0 1))
                                     (:wandering . ,(p2dg:make-color-4 0.5 0 0 1))
                                     (:sight-range . ,(p2dg:make-color-4 0 0 0 1))))

(defun clear-debug-markers ()
  "Use it after drawing debug vectors to clear them for next iteration."
  (setf *debug-vectors* '())
  (setf *debug-circles* '()))

(defun ddv (start vector color)
  "Register a debug (velocity) vector for drawing, i.e. a line from `START' to `START' + `VECTOR'."
  (push (list start vector color) *debug-vectors*)
  vector)

(defun ddp (start radius color)
  "Register a debug perception circle for drawing, with center at `START' and of particular `RADIUS'."
  (push (list start radius color) *debug-circles*))

(defun draw-debug-markers ()
  "Call this in rendering loop to draw all registered debug vectors."
  (when *debug-draw-vectors*
   (loop for v in *debug-vectors*
      do (gl:with-pushed-matrix
           (p2dglu:color4 (cdr (assoc (third v) +debug-colors-alist+)))
           (p2dglu:translate2 (first v))
           (gl:with-primitive :lines
             (gl:vertex 0.0 0.0)
             (gl:vertex (p2dm:vec-x (second v)) (p2dm:vec-y (second v)))))))
  
  (when *debug-draw-perception*
    (loop for c in *debug-circles*
       do (gl:with-pushed-matrix
            (p2dglu:color4 (cdr (assoc (third c) +debug-colors-alist+)))
            (p2dglu:translate2 (first c))
            (p2dglu:scale2-uniform (second c))
            (p2dglu:draw-circle-outline)))))

