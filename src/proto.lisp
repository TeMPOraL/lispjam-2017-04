(in-package #:lispjam-2017-04-temporal)

;;; Mechanics prototyping.


;;; Conf

(defparameter +max-boid-steering+ 30.0)
(defparameter +max-boid-speed+ 30.0)
(defparameter +max-boid-separation+ 50)
(defparameter +min-boid-separation+ 20)

(defparameter +boid-perception-range+ 100)


;; Debug
(defvar *debug-draw-vectors* nil "Toggle - draw debug vectors, or not.")
(defvar *debug-vectors* '())

(defparameter +debug-colors-alist+ `((:desired . ,(p2dg:make-color-4 0 0 0 1))
                                     (:cohesion . ,(p2dg:make-color-4 1 0 0 1))
                                     (:alignment . ,(p2dg:make-color-4 0 1 0 1))
                                     (:separation . ,(p2dg:make-color-4 0 0 1 0))))

(defun clear-debug-vectors ()
  (setf *debug-vectors* '()))

(defun ddv (start vector color)
  "Draw debug (velocity) vector, i.e. a line from `START' to `START' + `VECTOR'."
  (push (list start vector color) *debug-vectors*)
  vector)

(defun draw-debug-vectors ()
  (when *debug-draw-vectors*
   (loop for v in *debug-vectors*
      do (gl:with-pushed-matrix
           (p2dglu:color4 (cdr (assoc (third v) +debug-colors-alist+)))
           (p2dglu:translate2 (first v))
           (gl:with-primitive :lines
             (gl:vertex 0.0 0.0)
             (gl:vertex (p2dm:vec-x (second v)) (p2dm:vec-y (second v))))))))


;;; World

(defclass world ()
  ((boids :initform '()
          :initarg :boids
          :accessor boids)

   (food :initform '()
         :initarg :food
         :accessor food)))

(defparameter *world* (make-instance 'world))


(defclass entity ()
  ((position :initarg :position
             :initform (p2dm:make-vector-2d 0.0 0.0)
             :accessor entity-position)
   (velocity :initarg :velocity
             :initform (p2dm:make-vector-2d 0.0 0.0)
             :accessor entity-velocity)
   (color :initarg :color
          :initform (p2dg:make-color-4 0.5 0.5 0.2 1)
          :accessor entity-color)))


;;; Boids

(defclass boid (entity)
  ((behaviours :initarg :boid-behaviours
               :initform '()
               :accessor boid-behaviours)))

(defmethod perceive ((boid boid) (world world))
  (with-slots (position)
      boid
    (make-instance 'world
                   :boids (remove-if (lambda (other)
                                       (> (p2dm:distance-between-vectors position (entity-position other)) +boid-perception-range+))
                                     (boids world))
                   :food (food world))))

(defmethod apply-behaviours ((boid boid) (world world))
  (let ((seen-world (perceive boid world))
        (behaviours (boid-behaviours boid))
        (sum (p2dm:make-vector-2d)))
    (loop for b in behaviours
       do (p2dm:add-to-vector sum (funcall b boid seen-world)))
    sum))

(defmethod print-object ((boid boid) stream)
  (print-unreadable-object (boid stream :type t :identity t)
    (let ((position (slot-value boid 'position)))
     (format stream "(~A ~A)" (p2dm:vec-x position) (p2dm:vec-y position)))))



(defun click-handler (x y)
  (add-boid x y)
  (log:trace (boids *world*)))

(defun update-all-boids (dt)
  (let ((boids (boids *world*)))
    (loop for boid in boids
       do (update-boid boid dt))))

(defun draw-all-boids ()
  (loop for boid in (boids *world*)
     do (draw-boid boid)))


;;; Behavioral code

;;; Behaviour protocol:
;;; IN: current boid, perceived world
;;; OUT: desired *movement direction* (sorta like velocity)
;;;
;;; Let the behaviours reprioritize themselves dynamically.
(defparameter +behaviour-priority-min+ 0.0)
(defparameter +behaviour-priority-max+ 1.0)

(defun static-priority (vector priority)
  (p2dm:vector-of-length vector priority))

(defun lerp-priority (vector factor &optional (min-priority +behaviour-priority-min+) (max-priority +behaviour-priority-max+))
  (p2dm:vector-of-length vector (alexandria:lerp factor min-priority max-priority)))

(defun make-default-boid-behaviours ()
  (list (lambda (boid world)                 ; cohesion
          (com boid (boids world)))
        (lambda (boid world)                 ; alignment
          (align boid (boids world)))
        (lambda (boid world)                 ; separation
          (separate boid (boids world)))))

(defun com (boid all-boids)
  "Center of mass of `BOIDS'."
  (let ((boids-cnt (length all-boids)))
    (if (> boids-cnt 0)
        (static-priority (ddv (entity-position boid)
                              (p2dm:subtract-vectors (p2dm:scaled-vector (reduce #'p2dm:add-vectors all-boids :key #'entity-position)
                                                                         (/ 1.0 boids-cnt))
                                                     (entity-position boid))
                              :cohesion)
                         0.5)
        (p2dm:make-vector-2d))))

(defun align (boid boids)
  "Average velocity vector of `BOIDS'."
  (let ((boids-cnt (length boids)))
    (if (> boids-cnt 0)
        (static-priority (ddv (entity-position boid)
                              (p2dm:scaled-vector (reduce #'p2dm:add-vectors boids :key #'entity-velocity)
                                                  (/ 1.0 boids-cnt))
                              :alignment)
                         0.5)
        (p2dm:make-vector-2d))))

(defun separate (boid all-boids)
  "Average vector away from `BOIDS' that are too close."
  (let* ((too-close (remove-if (lambda (b)
                                 (let ((distance (p2dm:distance-between-vectors (entity-position b)
                                                                                (entity-position boid))))
                                   (or (< distance p2dm:+epsilon+)
                                       (> distance +min-boid-separation+))))
                               all-boids))
         (too-close-cnt (length too-close)))

    (if (> too-close-cnt 0)
        (static-priority (ddv (entity-position boid)
                              (p2dm:subtract-vectors (entity-position boid)
                                                     (p2dm:scaled-vector (reduce #'p2dm:add-vectors too-close :key #'entity-position)
                                                                         (/ 1.0 too-close-cnt)))
                              :separation)
                         1.0)
        (p2dm:make-vector-2d))))



(defun add-boid (x y)
  (push (make-instance 'boid
                       :position (p2dm:make-vector-2d x y)
                       :velocity (p2dm:rotated-vector-2d (p2dm:make-vector-2d 30.0 0.0)
                                                         (p2dm:random-float 0.0 p2dm:+2pi+))
                       :boid-behaviours (make-default-boid-behaviours))
        (boids *world*)))


(defun steering (velocity desired)
  (let ((steering (p2dm:subtract-vectors (p2dm:scaled-vector desired +max-boid-steering+) velocity)))
    (p2dm:clamped-vector steering +max-boid-steering+)))

(defun update-boid (boid dt)
  (with-slots (position velocity)
      boid
    (flet ((apply-force (force)
             (p2dm:add-to-vector velocity (p2dm:scaled-vector force dt))
             (p2dm:clamp-vector velocity +max-boid-speed+)))
      (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))
      (let ((desired (apply-behaviours boid *world*)))
        (ddv position (p2dm:scaled-vector desired 10.0) :desired)
        (apply-force (steering velocity desired))))))

(defun draw-boid (boid)
  (with-slots (position velocity color)
      boid
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:color4 color)
      (p2dglu:rotatez* (- (p2dm:vector-angle-2d velocity) (/ pi 2)))
      (gl:scale 4 8 4)
      (p2dglu:draw-triangle))))
