(in-package #:lispjam-2017-04-temporal)

(defvar *default-mono-font* nil)
(defvar *smaller-mono-font* nil)

(defclass ljgame (p2d:game)
  ())



(defmethod p2d:preinit ((game ljgame))
  (setf p2d:*window-title* "#:LISPJAM-2017-04-GAME1000"
        p2d:*use-fixed-timestep* t
        p2d:*window-width* 1280
        p2d:*window-height* 1024
        p2d:*window-resizable* nil))

(defmethod p2d:initialize ((game ljgame))
  (log:info "Initializing game.")

  ;; Graphics defaults
  (gl:clear-color 0.6901961 0.84705883 0.28235295 1.0)

  ;; (sdl2:gl-set-swap-interval 0) <-- use to disable vsync

  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  (gl:enable :line-smooth)
  (gl:hint :line-smooth-hint :nicest)
  (gl:enable :polygon-smooth)
  (gl:hint :polygon-smooth-hint :nicest)
  (gl:enable :texture-2d)

  ;; fonts
  (setf *default-mono-font* (p2dg:get-rendered-font "assets/fonts/VeraMoBd.ttf" :size 16))
  (setf *smaller-mono-font* (p2dg:get-rendered-font "assets/fonts/VeraMono.ttf" :size 12))

  ;; game initialize
  (setf *world* (make-instance 'world
                               :player (make-default-player)
                               :grazing-fields (make-default-grazing-fields)
                               :houses (make-default-sheep-houses))))

(defmethod p2d:deinitialize ((game ljgame))
  ;; TODO
  (log:info "Game deinitialized.")
  )

(defmethod p2d:on-key-event ((game ljgame) key state repeat)
  (macrolet ((on-key-down (scancode &body code)
               `(when (and (eql key-code ,scancode)
                           (sdl2:key-down-p state))
                  ,@code))
             (toggle (what)
               `(setf ,what (not ,what))))
    (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
      (log:trace key state key-code repeat)

      ;; direct keydown events go here
      (on-key-down :scancode-escape
                   (sdl2:push-event :quit))
      (on-key-down :scancode-f5
                   (toggle *debug-draw-vectors*))
      (on-key-down :scancode-f6
                   (toggle *debug-draw-perception*)))))

(defmethod p2d:on-mouse-button-event ((game ljgame) x y button state)
  (multiple-value-bind (rx ry)
      (p2d:window->canvas x y)

    (when (sdl2:keyboard-state-p :scancode-f7)

      ;; NOTE, those are all debugging handlers
      (when (and (= button 1)             ;FIXME DRY, c.f. sdl2:mouse-state-p
                 (= state 1)              ;FIXME DRY, c.f. sdl2:mouse-state
                 )
        (click-handler rx ry))

      (when (and (= button 2)
                 (= state 1))
        (mid-click-handler rx ry))

      (when (and (= button 3)
                 (= state 1))
        (right-click-handler rx ry)))))

(defmethod p2d:on-tick ((game ljgame) dt)
  (update-world dt))

(defmethod p2d:on-idle ((game ljgame) dt)
  ;; TODO
  )

(defmethod p2d:on-render ((game ljgame) dt)
  (draw-world)

  (p2dg:with-color (0 0 0)
    ;; TODO optimize - only re-render when text changed
    (p2dg::draw-text (format nil "Lost: ~D" *sheeps-dead*)
                     :font *default-mono-font*
                     :size 16
                     :x 580
                     :y 580
                     )
    (p2dg::draw-text (format nil "Saved: ~D" *sheeps-saved*)
                     :font *default-mono-font*
                     :size 16
                     :x 680
                     :y 580))

  (draw-debug-markers)
  (clear-debug-markers))



(defun run ()
  (p2d:run :game (make-instance 'ljgame)))

(defun run-with-profiling (&optional (mode :cpu))
  (p2d:run :game (make-instance 'ljgame)
           :profiling-mode mode))
