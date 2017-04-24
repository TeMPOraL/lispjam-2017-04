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

(defparameter +sheep-size+ 2.5)
(defparameter +sheep-grazing-cooldown-time+ 2.0)
(defparameter +sheep-eating-speed+ 1.0)

(defparameter +food-color+ (p2dg:make-color-4 0 0.4 0 1))
(defparameter +food-size+ 2.0)

(defparameter +grazing-field-color+ (p2dg:make-color-4 0 0.9 0 0.5))
(defparameter +grazing-field-spawn-cooldown-min+ 5.0)
(defparameter +grazing-field-spawn-cooldown-max+ 14.0)
(defparameter +max-simultaneous-food+ 20)

(defparameter +sheep-house-color+ (p2dg:make-color-4 0.5 0.25 0 1))


;;; World

(defclass world ()
  ((boids :initform '()
          :initarg :boids
          :accessor boids)

   (food :initform '()
         :initarg :food
         :accessor food)

   (grazing-fields :initform '()
                   :initarg :grazing-fields
                   :accessor grazing-fields)

   (houses :initform '()
           :initarg :houses
           :accessor houses)

   (obstacles :initform '()
              :initarg :obstacles
              :accessor obstacles)

   (saved-sheep :initform '()
                :initarg :saved-sheep
                :accessor saved-sheep)

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

(defmethod update-entity ((entity entity) (world  world) dt)
  ;; TODO if needed.
  (values))

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
              :accessor food-size)
   (eatenp :initarg :food-eaten-p
           :initform nil
           :accessor food-eaten-p))
  (:default-initargs
   :color +food-color+))

(defmethod update-entity ((food food) (world world) dt)
  (declare (ignore food world dt))
  (values))

(defmethod draw-entity ((food food))
  (with-slots (position color food-size)
      food
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (p2dglu:scale2-uniform food-size)
      (p2dglu:color4 color)
      (p2dglu:draw-square))))


;;; A field. Basically a rectangle.

(defclass field (entity)
  ((width :initform 100
          :initarg :width
          :accessor field-width)
   (height :initform 100
           :initarg :height
           :accessor field-height)))

(defmethod draw-entity ((field field))
  (with-slots (position color width height)
      field
    (gl:with-pushed-matrix
      (p2dglu:translate2 position)
      (gl:scale (/ width 2) (/ height 2) 1)
      (p2dglu:color4 color)
      (p2dglu:draw-square))))



;;; Grazing field
(defclass grazing-field (field)
  ((food-spawn-cooldown :initform 0.0
                        :initarg :food-spawn-cooldown
                        :accessor grazing-field-food-spawn-cooldown))
  (:default-initargs
   :color +grazing-field-color+))

(defmethod update-entity ((field grazing-field) (world world) dt)
  (with-slots (position width height food-spawn-cooldown)
      field
    (decf food-spawn-cooldown dt)
    (maxf food-spawn-cooldown 0)

    (when (and (<= food-spawn-cooldown 0)
               (< (length (food world)) +max-simultaneous-food+))
      (setf food-spawn-cooldown (p2dm:random-float +grazing-field-spawn-cooldown-min+ +grazing-field-spawn-cooldown-max+))
      (add-food (p2dm:random-float (- (p2dm:vec-x position) (/ width 2)) ; FIXME add-food should be replaced with something using the world parameter
                                   (+ (p2dm:vec-x position) (/ width 2)))
                (p2dm:random-float (- (p2dm:vec-y position) (/ height 2))
                                   (+ (p2dm:vec-y position) (/ height 2)))))))


(defun make-default-grazing-fields ()
  (list (make-instance 'grazing-field
                       :position (p2dm:make-vector-2d 400.0 300.0))
        (make-instance 'grazing-field
                       :position (p2dm:make-vector-2d 500.0 200.0))))


(defclass sheep-house (field)
  ()
  (:default-initargs
   :color +sheep-house-color+))

(defun make-default-sheep-houses ()
  (list (make-instance 'sheep-house
                       :position (p2dm:make-vector-2d 400.0 575.0)
                       :width 100.0
                       :height 50.0)))

(defun entity-in-field-p (entity field)
  ;; NOTE will work only on axis-aligned fields
  (with-slots ((epos position))
      entity
    (with-slots ((fpos position) width height)
        field
      (let ((sx (- (p2dm:vec-x fpos) (/ width 2)))
            (sy (- (p2dm:vec-y fpos) (/ height 2))))
        (and (< sx (p2dm:vec-x epos) (+ sx width))
             (< sy (p2dm:vec-y epos) (+ sy height)))))))


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

(defmethod update-entity ((player player) (world world) dt)
  (declare (ignore world))
  (with-slots (position velocity orientation color speed angular-speed)
      player

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
    (p2dm:clampf (p2dm:vec-x position) 0.0 (float p2d:*canvas-width*))
    (p2dm:clampf (p2dm:vec-y position) 0.0 (float p2d:*canvas-height*))))

(defmethod draw-entity ((player player))
  (with-slots (position orientation color)
      player
    (draw-player position orientation color)))

(defun make-default-player ()
  (make-instance 'player
                 :position (p2dm:make-vector-2d (/ p2d:*canvas-width* 2.0) (/ p2d:*canvas-height* 2.0))
                 :color (p2dg:make-color-4 1.0 0.0 0.0 1.0)))


(defparameter *world* nil)


;;; Boids

(defclass boid (entity)
  ((behaviours :initarg :boid-behaviours
               :initform '()
               :accessor boid-behaviours)
   (perception-range :initarg :perception-range
                     :initform +boid-perception-range+
                     :accessor boid-perception-range)

   ;; supports wandering around behaviour
   (decision :initarg :decision
             :initform (p2dm:make-vector-2d)
             :accessor decision)
   (decision-cooldown :initarg :decision-cooldown
                      :initform (p2dm:random-float 2.0 5.0)      ;FIXME magic
                      :accessor decision-cooldown)))

(defmethod perceive ((boid boid) (world world))
  (with-slots (position perception-range)
      boid
    (flet ((can-see-point (point)       ;TODO expand to limited forward vision
             (< (p2dm:distance-between-vectors position point) perception-range)))
      (ddp position perception-range :sight-range)
      (make-instance 'perceived-world
                     :friendlies (remove-if-not (lambda (other)
                                                  (and (can-see-point (entity-position other))
                                                       (not (eq other boid))))
                                                (boids world))
                     :dangers '()
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

(defun steering (velocity desired)
  (let ((steering (p2dm:subtract-vectors (p2dm:scaled-vector desired +max-boid-steering+) velocity)))
    (p2dm:clamped-vector steering +max-boid-steering+)))

(defmethod update-entity ((boid boid) (world world) dt)
  (with-slots (position velocity decision-cooldown)
      boid
    (flet ((apply-force (force)
             (p2dm:add-to-vector velocity (p2dm:scaled-vector force dt))
             (p2dm:clamp-vector velocity +max-boid-speed+)))
      (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))
      (let ((desired (apply-behaviours boid world)))
        (ddv position (p2dm:scaled-vector desired 10.0) :desired)
        (apply-force (steering velocity desired))))

    (decf decision-cooldown dt)
    (maxf decision-cooldown 0.0)))

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
   (grazing-cooldown :initarg :grazing-cooldown
                     :initform 0.0
                     :accessor sheep-grazing-cooldown)))

(defun sheep-full-p (sheep)
  (<= (sheep-hunger sheep)
      0.0))

(defmethod perceive ((sheep sheep) (world world))
  (if (member sheep (saved-sheep world))
      (make-instance 'perceived-world
                     :friendlies '()
                     :dangers '()
                     :food '()
                     :player nil)
      (call-next-method)))

(defmethod apply-behaviours ((sheep sheep) (world world))
  (declare (ignore world))
  (if (> (sheep-grazing-cooldown sheep) 0)
      (p2dm:make-vector-2d)
      (call-next-method sheep world)))

(defmethod update-entity ((sheep sheep) (world world) dt)
  (decf (sheep-grazing-cooldown sheep) dt)
  (maxf (sheep-grazing-cooldown sheep) 0)
  (call-next-method sheep world dt)
  (when (> (sheep-grazing-cooldown sheep) 0)
    (setf (p2dm:vector-value (entity-velocity sheep)) +sheep-eating-speed+)))

(defmethod draw-entity ((sheep sheep))
  (with-slots (position velocity hunger blackp)
      sheep
    (draw-sheep position (- (p2dm:vector-angle-2d velocity) (/ pi 2)) (p2dg:lerp-color hunger +sheep-full-color+ +sheep-hungry-color+))))

(defun transfer-sheep-to-house (sheep world)
  (push sheep (saved-sheep world))
  (setf (boids world)
        (delete sheep (boids world)))
  (setf (boid-behaviours sheep)
        (make-default-saved-sheep-behaviours)))


;;; Behaviour utilities.

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

(defun find-smallest (list &key (test #'<) (key #'identity))
  "Find minimal element in a `LIST'.

`TEST' is a function of two arguments that provides the definition of 'smaller'.
`KEY' is a function that converts list elements into value that will be compared by `TEST'.

Returns the smallest compared value (as given by `KEY' function) as a second return value."
  (let ((smallest-value (car list))
        (smallest-compare (when list
                            (funcall key (car list)))))
    (dolist (current (cdr list))
      (let ((current-compare (funcall key current)))
        (when (funcall test current-compare smallest-compare)
          (setf smallest-value current
                smallest-compare current-compare))))
    (values smallest-value smallest-compare)))

(defun random-point-ahead (position direction distance-delta radius)
  "Pick a random point that's placed on a circle of `RADIUS' that's `DISTANCE-DELTA' units in a `DIRECTION' from `POSITION'."
  (let ((start-point (p2dm:add-vectors position (p2dm:scaled-vector direction distance-delta))))
    (p2dm:add-vectors start-point
                      (p2dm:vector-of-length (p2dm:rotated-vector-2d (p2dm:make-vector-2d 1.0 0.0) (p2dm:random-float 0.0 p2dm:+2pi+))
                                             radius))))


;;; Behavioral code

;;; Behaviour protocol:
;;; IN: current boid, perceived world
;;; OUT: desired *movement direction* (sorta like velocity)
;;;
;;; Let the behaviours reprioritize themselves dynamically.

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
        (lambda (boid world)
          (wander-around boid (friendlies world))) ; wandering around
        ))

(defun make-default-saved-sheep-behaviours ()
  (list (lambda (boid world)
          (declare (ignore boid world))
          (p2dm:make-vector-2d 0.0 1.0))))

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
                                   (or (> distance +min-boid-separation+))))
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

(defun chase-food (boid food)    ;FIXME works only on sheep; ensure no other boids happen to use it.
  "Behaviour to chase `FOOD'."
  (let* ((food-cnt (length food))
         (boid-position (entity-position boid))
         (closest-food (find-smallest food
                                      :test (lambda (p1 p2)
                                              (< (p2dm:distance-between-vectors-squared p1 boid-position)
                                                 (p2dm:distance-between-vectors-squared p2 boid-position)))
                                      :key #'entity-position)))
    (or
     (when (> food-cnt 0)
       (static-priority (ddv (entity-position boid)
                             (p2dm:subtract-vectors (entity-position closest-food) (entity-position boid))
                             :food-chasing)
                        (sheep-hunger boid)))
     (p2dm:make-vector-2d))))

(defun wander-around (boid other-boids)
  "Behaviour of random wandering for `BOID' when lonely."
  (or
   (when (= 0 (length other-boids))
     (with-slots (decision decision-cooldown)
         boid
       (if (> decision-cooldown 0)
           decision
           (progn
             (setf decision-cooldown (p2dm:random-float 2.0 5.0)) ;; FIXME magic
             (setf decision (static-priority (ddv (entity-position boid)
                                                  (p2dm:subtract-vectors (random-point-ahead (entity-position boid)
                                                                                             (p2dm:normalized-vector (entity-velocity boid))
                                                                                             100.0 50.0)
                                                                         (entity-position boid))
                                                  :wandering)
                                             0.3))))))
   (p2dm:make-vector-2d)))


(defun key-pressed-p (scancode)
  (sdl2:keyboard-state-p scancode))

(defun click-handler (x y)
  (add-boid x y)
  (log:trace (boids *world*)))

(defun right-click-handler (x y)
  (add-food x y))

(defun update-world (dt)
  (update-all-boids *world* dt)
  (update-entity (player *world*) *world* dt)

  (loop for gf in (grazing-fields *world*)
     do (update-entity gf *world* dt))

  (loop for h in (houses *world*)
     do (update-entity h *world* dt))

  (loop for s in (saved-sheep *world*)
     do (update-entity s *world* dt))

  (handle-collisions *world*)
  (remove-eaten-food *world*))

(defun draw-world ()
  (loop for h in (houses *world*)
     do (draw-entity h))

  (loop for gf in (grazing-fields *world*)
     do (draw-entity gf))

  (draw-all-boids)

  (loop for s in (saved-sheep *world*)
     do (draw-entity s))
  
  (draw-food)
  (draw-entity (player *world*)))

(defun draw-food ()
  (loop for food in (food *world*)
     do (draw-entity food)))

(defun update-all-boids (world dt)
  (let ((boids (boids world))
        (saved (saved-sheep world)))
    (loop for boid in boids
       do (update-entity boid world dt))
    (loop for boid in saved
       do (update-entity boid world dt))))

(defun draw-all-boids ()
  (loop for boid in (boids *world*)
     do (draw-entity boid)))

(defun spheres-collide-p (pos1 pos2 r1 r2)
  (< (p2dm:distance-between-vectors-squared pos1 pos2)
     (+ (p2dm:square r1) (p2dm:square r2))))

(defun handle-collisions (world)
  (dolist (sheep (boids world))
    ;; Handle sheep/food collisions.
    (dolist (food (food world))
      (when (and (not (food-eaten-p food))
                 (> (sheep-hunger sheep) 0.0)
                 (spheres-collide-p (entity-position sheep) (entity-position food) +sheep-size+ (food-size food)))
        (setf (sheep-grazing-cooldown sheep) +sheep-grazing-cooldown-time+
              (food-eaten-p food) t)
        (decf (sheep-hunger sheep) 0.5)))  ;FIXME magic
    ;; Handle sheep/houses collisions.
    (dolist (house (houses world))
      (when (and (sheep-full-p sheep)
                 (entity-in-field-p sheep house))
        (transfer-sheep-to-house sheep world)
        (return)                        ; break houses iteration for that sheep
        ))))

(defun remove-eaten-food (world)
  (setf (food world) (delete-if #'food-eaten-p (food world))))



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
