(in-package #:lispjam-2017-04-temporal)

;;; Mechanics prototyping.


;;; Conf

(defparameter +max-boid-steering+ 60.0)
(defparameter +max-boid-speed+ 60.0)
(defparameter +min-boid-separation+ 30)

(defparameter +min-boid-distance-to-game-area-boundary+ 30)

(defparameter +boid-perception-range+ 50)

(defparameter +player-base-speed+ 30)
(defparameter +player-running-speed+ 60)
(defparameter +player-slow-speed+ 10)
(defparameter +player-angular-speed+ p2dm:+pi+)

(defparameter +sheep-hungry-color+ (p2dg:make-color-4 0 0 1 1))
(defparameter +sheep-full-color+ (p2dg:make-color-4 0 1 0 1))

(defparameter +food-color+ (p2dg:make-color-4 0 0.4 0 1))
(defparameter +food-size+ 2.0)


;;; World

(defclass world ()
  ((boids :initform '()
          :initarg :boids
          :accessor boids)

   (food :initform '()
         :initarg :food
         :accessor food)

   (houses :initform '()
           :initarg :houses
           :accessor houses)

   (obstacles :initform '()
              :initarg :obstacles
              :accessor obstacles)

   (player :initform (error "Player needs to be specified explicitly.")
           :initarg :player
           :accessor player)))

(defclass perceived-world ()
  ((friendlies :initform '()
               :initarg :friendlies
               :accessor friendlies)

   (dangers :initform '()
            :initarg :dangers
            :accessor dangers)

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

(defmethod draw-entity ((entity entity))
  (with-slots (position color)
      entity
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:color4 color)
      (p2dglu:draw-square))))


;;; Food
(defclass food (entity)
  ((food-size :initarg :food-size
              :initform +food-size+
              :accessor food-size))
  (:default-initargs
   :color +food-color+))

(defmethod draw-entity ((food food))
  (with-slots (position color food-size)
      food
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:scale2-uniform food-size)
      (p2dglu:color4 color)
      (p2dglu:draw-square))))


(defclass player (entity)
  ((orientation :initarg :orientation
                :initform 0.0
                :accessor player-orientation)
   (speed :initarg :speed
          :initform 30.0
          :accessor player-speed)
   (angular-speed :initarg :speed
                  :initform p2dm:+pi+
                  :accessor player-angular-speed)))

(defmethod draw-entity ((player player))
  (with-slots (position orientation color)
      player
    (draw-player position orientation color)))

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
      (make-instance 'perceived-world
                     :friendlies (remove-if-not #'can-see-point
                                                (boids world)
                                                :key #'entity-position)
                     :food (remove-if-not #'can-see-point
                                          (food world)
                                          :key #'entity-position)
                     :player (when (can-see-point (entity-position (player world)))
                               (player world))))))

(defmethod apply-behaviours ((boid boid) (world world))
  (let ((seen-world (perceive boid world))
        (behaviours (boid-behaviours boid))
        (sum (p2dm:make-vector-2d)))
    (loop for b in behaviours
       do (p2dm:add-to-vector sum (funcall b boid seen-world)))
    sum))

(defmethod draw-entity ((boid boid))
  (with-slots (position velocity color)
      boid
    (draw-boid position (- (p2dm:vector-angle-2d velocity) (/ pi 2)) color)))

(defmethod print-object ((boid boid) stream)
  (print-unreadable-object (boid stream :type t :identity t)
    (let ((position (slot-value boid 'position)))
      (format stream "(~A ~A)" (p2dm:vec-x position) (p2dm:vec-y position)))))


(defclass sheep (boid)
  ((hunger :initarg :hunger
           :initform 1.0
           :accessor sheep-hunger)
   (blackp :initarg :blackp
           :initform nil
           :accessor sheep-black-p)))

(defmethod draw-entity ((sheep sheep))
  (with-slots (position velocity hunger blackp)
      sheep
    (draw-sheep position (- (p2dm:vector-angle-2d velocity) (/ pi 2)) (p2dg:lerp-color hunger +sheep-full-color+ +sheep-hungry-color+))))


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

(defun aggressive-repulsion (radius distance &optional (factor 10.0))
  ;; y = - 2^(-10(1-x)) + 1
  (let ((x (clamp (/ distance radius) 0 1)))
    (values (float (1+ (- (expt 2 (* (- factor) (- 1 x))))))
            (float x))))

(defun make-default-boid-behaviours ()
  (list (lambda (boid world)                 ; cohesion
          (com boid (friendlies world)))
        (lambda (boid world)                 ; alignment
          (align boid (friendlies world)))
        (lambda (boid world)                 ; separation
          (separate boid (friendlies world)))
        (lambda (boid world)                 ; wall avoidance
          (avoid-walls boid))
        (lambda (boid world)
          (avoid-player boid (player world))) ; player avoidance
        (lambda (boid world)
          (chase-food boid (food world))) ; food chasing
        ))

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
                                   (or (eq boid b)
                                       (> distance +min-boid-separation+))))
                               all-boids))
         (too-close-cnt (length too-close)))

    (if (> too-close-cnt 0)
        (let* ((too-close-center (p2dm:scaled-vector (reduce #'p2dm:add-vectors too-close :key #'entity-position)
                                                     (/ 1.0 too-close-cnt)))
               (too-close-center-dist (p2dm:distance-between-vectors (entity-position boid)
                                                                     too-close-center))
               (lr (aggressive-repulsion +min-boid-separation+ too-close-center-dist)))
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
            (lr (aggressive-repulsion (boid-perception-range boid) distance 6)))
       (static-priority (ddv pos
                             (p2dm:scaled-vector (p2dm:subtract-vectors pos player-pos)
                                                 lr)
                             :avoid-player)
                        lr)))
   (p2dm:make-vector-2d)))

(defun chase-food (boid food)           ;FIXME works only on sheep; ensure no other boids happen to use it.
  "Behaviour to chase `FOOD'."
  (let ((food-cnt (length food)))
    (or
     (when (> food-cnt 0)
       (static-priority (ddv (entity-position boid)
                             (p2dm:subtract-vectors (p2dm:scaled-vector (reduce #'p2dm:add-vectors food :key #'entity-position)
                                                                        (/ 1.0 food-cnt))
                                                    (entity-position boid))
                             :food-chasing)
                        (sheep-hunger boid)))
     (p2dm:make-vector-2d))))


(defun key-pressed-p (scancode)
  (sdl2:keyboard-state-p scancode))

(defun click-handler (x y)
  (add-boid x y)
  (log:trace (boids *world*)))

(defun right-click-handler (x y)
  (add-food x y))

(defun update-world (dt)
  (update-all-boids dt)
  (update-player dt))

(defun draw-world ()
  (draw-all-boids)
  (draw-food)
  (draw-entity (player *world*)))

(defun draw-food ()
  (loop for food in (food *world*)
     do (draw-entity food)))

(defun update-all-boids (dt)
  (let ((boids (boids *world*)))
    (loop for boid in boids
       do (update-boid boid dt))))

(defun draw-all-boids ()
  (loop for boid in (boids *world*)
     do (draw-entity boid)))

(defun update-player (dt)
  (with-slots (position velocity orientation color speed angular-speed)
      (player *world*)

    ;; input
    (setf speed (cond ((key-pressed-p :scancode-up)
                       +player-running-speed+)
                      ((key-pressed-p :scancode-down)
                       +player-slow-speed+)
                      (t
                       +player-base-speed+))
          angular-speed (cond ((key-pressed-p :scancode-left)
                               (+ +player-angular-speed+))
                              ((key-pressed-p :scancode-right)
                               (- +player-angular-speed+))
                              (t
                               0.0)))

    ;; physics
    (incf orientation (* angular-speed dt))
    (setf velocity (p2dm:scaled-vector (p2dm:rotated-vector-2d (p2dm:make-vector-2d 1.0 0.0) orientation)
                                       speed))
    (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))

    ;; clamp position to game boundaries
    (setf (p2dm:vec-x position) (p2dm:clamp (p2dm:vec-x position) 0.0 (float p2d:*canvas-width*))
          (p2dm:vec-y position) (p2dm:clamp (p2dm:vec-y position) 0.0 (float p2d:*canvas-height*)))))



(defun add-boid (x y)
  (push (make-instance 'sheep
                       :position (p2dm:make-vector-2d x y)
                       :velocity (p2dm:rotated-vector-2d (p2dm:make-vector-2d 30.0 0.0)
                                                         (p2dm:random-float 0.0 p2dm:+2pi+))
                       :boid-behaviours (make-default-boid-behaviours))
        (boids *world*)))

(defun add-food (x y)
  (push (make-instance 'food
                       :position (p2dm:make-vector-2d x y))
        (food *world*)))

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



(defun draw-boid (position orientation color)
  (gl:with-pushed-matrix
    (p2dglu:translate2 position)
    (p2dglu:color4 color)
    (p2dglu:rotatez* orientation)
    (gl:scale 4 8 4)
    (p2dglu:draw-triangle)))

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

(defun draw-player (position orientation color)
  (gl:with-pushed-matrix
    (p2dglu:translate2 position)
    (p2dglu:color4 color)
    (p2dglu:rotatez* (- orientation (/ p2dm:+pi+ 2)))
    (gl:scale 7 10 7)
    (p2dglu:draw-triangle)))
