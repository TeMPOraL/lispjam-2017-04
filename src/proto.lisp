(in-package #:lispjam-2017-04-temporal)

;;; Mechanics prototyping.


;;; Conf

(defparameter +max-boid-steering+ 30.0)
(defparameter +max-boid-speed+ 30.0)


;;; Boids

(defclass boid ()
  ((position :initarg :position
             :initform (p2dm:make-vector-2d 0.0 0.0)
             :accessor boid-position)
   (velocity :initarg :velocity
             :initform (p2dm:make-vector-2d 0.0 0.0)
             :accessor boid-velocity)
   (color :initarg :color
          :initform (p2dg:make-color-4 1 0 0 1)
          :accessor boid-color)))

(defmethod print-object ((boid boid) stream)
  (print-unreadable-object (boid stream :type t :identity t)
    (let ((position (slot-value boid 'position)))
     (format stream "(~A ~A)" (p2dm:vec-x position) (p2dm:vec-y position)))))

(defparameter *boids* '())



(defun click-handler (x y)
  (add-boid x y)
  (log:info *boids*))

(defun update-all-boids (dt)
  (let ((com (com)))
    (loop for boid in *boids*
       do (update-boid boid dt com))))

(defun draw-all-boids ()
  (loop for boid in *boids*
     do (draw-boid boid)))



(defun add-boid (x y)
  (push (make-instance 'boid
                       :position (p2dm:make-vector-2d x y)
                       :velocity (p2dm:make-vector-2d 0.0 30.0))
        *boids*))

(defun com ()
  "Center of mass of boids."
  (let ((boids-cnt (length *boids*)))
    (if (> boids-cnt 0)
        (p2dm:scaled-vector (reduce #'p2dm:add-vectors *boids* :key #'boid-position)
                            (/ 1.0 boids-cnt))
        (p2dm:make-vector-2d))))

(defun steering (position velocity target)
  (let* ((desired (p2dm:subtract-vectors target position))
         (steering (p2dm:subtract-vectors desired velocity)))
    (p2dm:clamped-vector steering +max-boid-steering+)))

(defun update-boid (boid dt com)
  (with-slots (position velocity)
      boid
    (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))
    (p2dm:add-to-vector velocity (p2dm:scaled-vector (steering position velocity com) dt))
    (p2dm:clamp-vector velocity +max-boid-speed+)))

(defun draw-boid (boid)
  (with-slots (position velocity color)
      boid
    (gl:with-pushed-matrix
      #+nil(p2dglu:rotatez* something)
      (p2dglu:translate2 position)
      (p2dglu:color4 color)
      (p2dglu:scale2-uniform 4)
      (p2dglu:draw-triangle))))
