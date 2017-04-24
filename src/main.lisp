(in-package #:lispjam-2017-04-temporal)


;;; Assets and related

(defvar *default-mono-font* nil)

(defvar *intro-background* nil)
(defvar *game-background* nil)
(defvar *get-ready-background* nil)
(defvar *victory-background* nil)
(defvar *defeat-background* nil)

(defparameter *window->canvas-ar-w* 0 "window/canvas horizontal aspect ratio")
(defparameter *window->canvas-ar-h* 0 "window/canvas vertical aspect ratio")



(defclass ljgame (p2d:game)
  ())


(defun load-assets ()
  ;; fonts
  ;; (setf *main-font-size* (floor (p2d:canvas->window 16 0))) ; a hotfix for font size being specified in real pixels, not canvas pixels
  (setf *default-mono-font* (p2dg:get-rendered-font "assets/fonts/VeraMoBd.ttf" :size 16))

  ;; textures
  (setf *intro-background* (p2dg:get-texture "assets/intro-screen.png")
        *game-background* (p2dg:get-texture "assets/game-background.png")
        *get-ready-background* (p2dg:get-texture "assets/get-ready-screen.png")
        *victory-background* (p2dg:get-texture "assets/victory-screen.png")
        *defeat-background* (p2dg:get-texture "assets/defeat-screen.png")))


(defmethod p2d:preinit ((game ljgame))
  (setf p2d:*window-title* "#:LISPJAM-2017-04-GAME1000"
        p2d:*use-fixed-timestep* t
        p2d:*window-width* 1280
        p2d:*window-height* 1024
        p2d:*window-resizable* nil))

(defmethod p2d:initialize ((game ljgame))
  (log:info "Initializing game.")

  (setf *window->canvas-ar-w* (float (/ p2d:*canvas-width* p2d:*window-width*))
        *window->canvas-ar-h* (float (/ p2d:*canvas-height* p2d:*window-height*)))

  ;; Graphics defaults
  (gl:clear-color 0.6901961 0.84705883 0.28235295 1.0)

  ;; (sdl2:gl-set-swap-interval 0) <-- use to disable vsync

  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  (gl:enable :texture-2d)

  (load-assets)

  (intro/enter))

(defmethod p2d:deinitialize ((game ljgame))
  (p2dg:clear-texture-cache)
  (p2dg:clear-font-cache)
  (log:info "Game deinitialized."))

(defmethod p2d:on-key-event ((game ljgame) key state repeat)
  (when *screen-keyboard-handler*
    (funcall *screen-keyboard-handler* key state repeat)))

(defmethod p2d:on-mouse-button-event ((game ljgame) x y button state)
  (when *screen-mouse-handler*
    (funcall *screen-mouse-handler* x y button state)))

(defmethod p2d:on-tick ((game ljgame) dt)
  (when *screen-update-handler*
    (funcall *screen-update-handler* dt)))

(defmethod p2d:on-idle ((game ljgame) dt)
  ;; Nothing to do.
  )

(defmethod p2d:on-render ((game ljgame) dt)
  (declare (ignore dt))
  (when *screen-render-handler*
    (funcall *screen-render-handler*)))



(defun run ()
  (p2d:run :game (make-instance 'ljgame)))

(defun run-with-profiling (&optional (mode :cpu))
  (p2d:run :game (make-instance 'ljgame)
           :profiling-mode mode))
