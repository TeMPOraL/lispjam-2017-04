(in-package #:lispjam-2017-04-temporal)

;;; Mechanics prototyping.


;;; Conf

(defparameter +max-boid-steering+ 30.0)
(defparameter +max-boid-speed+ 30.0)
(defparameter +max-boid-separation+ 50)
(defparameter +min-boid-separation+ 10)


;; Debug
(defvar *debug-vectors* '())

(defun clear-debug-vectors ()
  (setf *debug-vectors* '()))

(defun ddv (start vector)
  (push (cons start vector) *debug-vectors*)
  vector)

(defun draw-debug-vectors ()
  (loop for v in *debug-vectors*
     do (gl:with-pushed-matrix
          (p2dglu:translate2 (car v))
          (gl:with-primitive :lines
            (gl:vertex 0.0 0.0)
            (gl:vertex (p2dm:vec-x (cdr v)) (p2dm:vec-y (cdr v)))))))


;;; World

(defclass world ()
  ((boids :initform '()
          :initarg :boids
          :accessor boids)

   (food :initform '()
         :initarg food
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
          :initform (p2dg:make-color-4 1 0 0 1)
          :accessor entity-color)))


;;; Boids

(defclass boid (entity)
  ((behaviours :initarg :boid-behaviours
               :initform '()
               :accessor boid-behaviours)))

(defmethod perceive ((boid boid) (world world))
  (declare (ignore boid))
  world)

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
(defun make-default-boid-behaviours ()
  (list #+nil(lambda (boid world)                 ; cohesion
          (declare (ignore boid))
          (com (boids world)))
        #+nil(lambda (boid world)                 ; alignment
          (align (boids world)))
        (lambda (boid world)                 ; separation
          (separate boid (boids world)))))

(defun com (boids)
  "Center of mass of `BOIDS'."
  (let ((boids-cnt (length boids)))
    (if (> boids-cnt 0)
        (p2dm:scaled-vector (reduce #'p2dm:add-vectors boids :key #'entity-position)
                            (/ 1.0 boids-cnt))
        (p2dm:make-vector-2d))))

(defun align (boids)
  "Average velocity vector of `BOIDS'."
  (let ((boids-cnt (length boids)))
    (if (> boids-cnt 0)
        (p2dm:scaled-vector (reduce #'p2dm:add-vectors boids :key #'entity-velocity)
                            (/ 1.0 boids-cnt))
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
    (p2dm:subtract-vectors (entity-position boid)
                           (if (> too-close-cnt 0)
                               (ddv (entity-position boid)
                                    (p2dm:scaled-vector (reduce #'p2dm:add-vectors too-close :key #'entity-position)
                                                        (/ 1.0 too-close-cnt)))
                               (p2dm:make-vector-2d)))))



(defun add-boid (x y)
  (push (make-instance 'boid
                       :position (p2dm:make-vector-2d x y)
                       :velocity (p2dm:make-vector-2d 0.0 30.0)
                       :boid-behaviours (make-default-boid-behaviours))
        (boids *world*)))


(defun steering (position velocity target)
  (let* ((desired (p2dm:subtract-vectors target position))
         (steering (p2dm:subtract-vectors desired velocity)))
    (p2dm:clamped-vector steering +max-boid-steering+)))

(defun update-boid (boid dt)
  (with-slots (position velocity)
      boid
    (flet ((apply-force (force)
             (p2dm:add-to-vector velocity (p2dm:scaled-vector force dt))
             (p2dm:clamp-vector velocity +max-boid-speed+)))
      (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))
      (apply-force (steering position velocity (apply-behaviours boid *world*))))))

(defun draw-boid (boid)
  (with-slots (position velocity color)
      boid
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:color4 color)
      (p2dglu:rotatez* (- (p2dm:vector-angle-2d velocity) (/ pi 2)))
      (gl:scale 4 8 4)
      (p2dglu:draw-triangle))))
