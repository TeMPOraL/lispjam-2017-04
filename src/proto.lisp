(in-package #:lispjam-2017-04-temporal)

;;; Mechanics prototyping.


;;; Conf

(defparameter +max-boid-steering+ 60.0)
(defparameter +max-boid-speed+ 60.0)
(defparameter +min-boid-separation+ 30)

(defparameter +min-boid-distance-to-game-area-boundary+ 30)

(defparameter +boid-perception-range+ 50)


;;; World

(defclass world ()
  ((boids :initform '()
          :initarg :boids
          :accessor boids)

   (food :initform '()
         :initarg :food
         :accessor food)

   (player :initform (error "Player needs to be specified explicitly.")
           :initarg :player
           :accessor player)))


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


(defclass player (entity)
  ((orientation :initarg :orientation
                :initform 0.0
                :accessor player-orientation)))

(defun make-default-player ()
  (make-instance 'player
                 :position (p2dm:make-vector-2d (/ p2d:*canvas-width* 2.0) (/ p2d:*canvas-height* 2.0))
                 :color (p2dg:make-color-4 1.0 0.0 0.0 1.0)))


(defparameter *world* (make-instance 'world
                                     :player (make-default-player)))


;;; Boids

(defclass boid (entity)
  ((behaviours :initarg :boid-behaviours
               :initform '()
               :accessor boid-behaviours)
   (perception-range :initarg :perception-range
                     :initform +boid-perception-range+
                     :accessor boid-perception-range)))

(defmethod perceive ((boid boid) (world world))
  (with-slots (position perception-range)
      boid
    (flet ((can-see-point (point)       ;TODO expand to limited forward vision
             (< (p2dm:distance-between-vectors position point) perception-range)))
      (ddp position perception-range :sight-range)
      (make-instance 'world
                     :boids (remove-if-not #'can-see-point
                                           (boids world)
                                           :key #'entity-position)
                     :food (food world)
                     :player (when (can-see-point (entity-position (player world)))
                               (player world))))))

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

(defun linear-repulsion (radius distance)
  (float (clamp (/ (- radius distance) radius) 0 1)))

(defun aggressive-repulsion (radius distance)
  ;; TODO figure it out and make it work.
  )

(defun make-default-boid-behaviours ()
  (list (lambda (boid world)                 ; cohesion
          (com boid (boids world)))
        (lambda (boid world)                 ; alignment
          (align boid (boids world)))
        (lambda (boid world)                 ; separation
          (separate boid (boids world)))
        (lambda (boid world)                 ; wall avoidance
          (avoid-walls boid))
        (lambda (boid world)
          (avoid-player boid (player world)) ; player avoidance
          )))

(defun com (boid all-boids)
  "Center of mass of `BOIDS'."
  (let ((boids-cnt (length all-boids)))
    (if (> boids-cnt 0)
        (static-priority (ddv (entity-position boid)
                              (p2dm:subtract-vectors (p2dm:scaled-vector (reduce #'p2dm:add-vectors all-boids :key #'entity-position)
                                                                         (/ 1.0 boids-cnt))
                                                     (entity-position boid))
                              :cohesion)
                         0.2)
        (p2dm:make-vector-2d))))

(defun align (boid boids)
  "Average velocity vector of `BOIDS'."
  (let ((boids-cnt (length boids)))
    (if (> boids-cnt 0)
        (static-priority (ddv (entity-position boid)
                              (p2dm:scaled-vector (reduce #'p2dm:add-vectors boids :key #'entity-velocity)
                                                  (/ 1.0 boids-cnt))
                              :alignment)
                         0.3)
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
        (let* ((too-close-center (p2dm:scaled-vector (reduce #'p2dm:add-vectors too-close :key #'entity-position)
                                                     (/ 1.0 too-close-cnt)))
               (too-close-center-dist (p2dm:distance-between-vectors (entity-position boid)
                                                                     too-close-center))
               (lr (linear-repulsion +min-boid-separation+ too-close-center-dist)))
          (static-priority (ddv (entity-position boid)
                                (p2dm:scaled-vector (p2dm:subtract-vectors (entity-position boid)
                                                                           too-close-center)
                                                    lr)
                                :separation)
                           lr))
        (p2dm:make-vector-2d))))

(defun avoid-walls (boid)
  "Behaviour to limit `BOID' to gameplay area."
  (let* ((position (entity-position boid))
         (xpos (p2dm:vec-x position))
         (ypos (p2dm:vec-y position)))

    (p2dm:make-vector-2d (or
                          (when (< xpos +min-boid-distance-to-game-area-boundary+) 1.0)
                          (when (< (- p2d:*canvas-width* xpos) +min-boid-distance-to-game-area-boundary+) -1.0)
                          0.0)
                         (or
                          (when (< ypos +min-boid-distance-to-game-area-boundary+) 1.0)
                          (when (< (- p2d:*canvas-height* ypos) +min-boid-distance-to-game-area-boundary+) -1.0)
                          0.0))))

(defun avoid-player (boid player)
  "Behaviour to run away from `PLAYER'."
  (or
   (when player
     (let* ((pos (entity-position boid))
            (player-pos (entity-position player))
            (distance (p2dm:distance-between-vectors pos player-pos))
            (lr (linear-repulsion (boid-perception-range boid) distance)))
       (static-priority (ddv pos
                             (p2dm:scaled-vector (p2dm:subtract-vectors pos player-pos)
                                                 lr)
                             :avoid-player)
                        lr)))
   (p2dm:make-vector-2d)))



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

(defun update-player (dt)
  (with-slots (position velocity orientation color)
      (player *world*)

    ;; input

    ;; physics
    (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))
    ;; TODO rotation, stuff.
))

(defun draw-player ()
  (with-slots (position velocity orientation color)
      (player *world*)
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:color4 color)
      (p2dglu:rotatez* orientation)
      (gl:scale 8 10 8)
      (p2dglu:draw-triangle))))



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
    (draw-sheep position (- (p2dm:vector-angle-2d velocity) (/ pi 2)) color)
    #+nil(gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:color4 color)
      (p2dglu:rotatez* (- (p2dm:vector-angle-2d velocity) (/ pi 2)))
      (gl:scale 4 8 4)
      (p2dglu:draw-triangle))))



(defun draw-sheep (position orientation color)
  (gl:with-pushed-matrix
    (p2dglu:color4 color)               ;FIXME blending? how?
    (p2dglu:translate2 position)
    (p2dglu:rotatez* orientation)
    (gl:scale 4 6 4)
    (p2dglu:draw-circle)
    (gl:translate 0 1 0)
    (gl:scale 0.5 0.5 0.5)
    (p2dglu:draw-circle)))
