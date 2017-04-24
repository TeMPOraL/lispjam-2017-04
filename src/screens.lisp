(in-package #:lispjam-2017-04-temporal)


;;; Config and state

(defparameter +screen-switch-delay+ 1.5)
(defvar *screen-switch-countdown* 0.0)

(defvar *screen-keyboard-handler* nil)
(defvar *screen-mouse-handler* nil)
(defvar *screen-update-handler* nil)
(defvar *screen-render-handler* nil)


;;; Utility macros

(defmacro on-key-down (scancode &body code) ;FIXME bad evil macro, captures `state' and `key-code'.
  `(when (and (eql key-code ,scancode)
              (sdl2:key-down-p state))
     ,@code))

(defmacro toggle (what)
  `(setf ,what (not ,what)))


;;; Intro screen

(defun intro/enter ()
  (log:info "Entering Intro screen.")
  (setf *screen-keyboard-handler* #'intro/keyboard-handler
        *screen-mouse-handler* nil
        *screen-update-handler* #'intro/update
        *screen-render-handler* #'intro/draw)
  
  (setf *current-level* 0))

(defun intro/keyboard-handler (key state repeat)
  (declare (ignore repeat))
  (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
    (on-key-down :scancode-escape
      (sdl2:push-event :quit)))

  ;; on any other key down...
  (when (sdl2:key-down-p state)
    (get-ready/enter)))

(defun intro/update (dt)
  (declare (ignore dt)))

(defun intro/draw ()
  (p2dg:with-color (0 0 0 1)
    (p2dg::draw-text "TODO intro"
                     :font *default-mono-font*
                     :size 16
                     :x 400
                     :y 300)))


;;; Get Ready screen

(defun get-ready/enter ()
  (log:info "Entering Get Ready screen.")
  (setf *screen-switch-countdown* +screen-switch-delay+)

  (incf *current-level*)
  (setf *world* (make-world *current-level*)
        *sheeps-saved* 0
        *sheeps-dead* 0)
  
  (setf *screen-keyboard-handler* nil
        *screen-mouse-handler* nil
        *screen-update-handler* #'get-ready/update
        *screen-render-handler* (lambda ()
                                  (main-game/draw)
                                  (get-ready/draw))))

(defun get-ready/update (dt)
  (decf *screen-switch-countdown* dt)
  (when (< *screen-switch-countdown* 0)
    (main-game/enter)))

(defun get-ready/draw ()
  ;; TODO draw a full-screen dark quad
  (p2dg:with-color (0 0 0 1)
    (p2dg::draw-text "Get ready..."
                     :font *default-mono-font*
                     :size 16
                     :x 400
                     :y 300)))


;;; Main Game screen

(defun main-game/enter ()
  (log:info "Entering Main Game screen.")
  (setf *screen-keyboard-handler* #'main-game/keyboard-handler
        *screen-mouse-handler* #'main-game/mouse-handler
        *screen-update-handler* #'main-game/update
        *screen-render-handler* #'main-game/draw))

(defun main-game/keyboard-handler (key state repeat)
  (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
    (log:trace key state key-code repeat)

    ;; direct keydown events go here
    (on-key-down :scancode-escape
                 (intro/enter))         ;(sdl2:push-event :quit)
    (on-key-down :scancode-f5
                 (toggle *debug-draw-vectors*))
    (on-key-down :scancode-f6
                 (toggle *debug-draw-perception*))))

(defun main-game/mouse-handler (x y button state)
  (multiple-value-bind (rx ry)
      (p2d:window->canvas x y)

    (when (sdl2:keyboard-state-p :scancode-f7)

      ;; NOTE, those are all debugging handlers
      (when (and (= button 1)           ;FIXME DRY, c.f. sdl2:mouse-state-p
                 (= state 1)            ;FIXME DRY, c.f. sdl2:mouse-state
                 )
        (click-handler rx ry))

      (when (and (= button 2)
                 (= state 1))
        (mid-click-handler rx ry))

      (when (and (= button 3)
                 (= state 1))
        (right-click-handler rx ry)))))

(defun main-game/update (dt)
  (update-world dt))

(defun main-game/draw ()
  (draw-world)

  (p2dg:with-color (0 0 0)
    ;; TODO optimize - only re-render when text changed
    (p2dg::draw-text (format nil "Lost: ~D/~D" *sheeps-dead* (ceiling (/ *total-sheep* 2)))
                     :font *default-mono-font*
                     :size 16
                     :x 570
                     :y 580
                     )
    (p2dg::draw-text (format nil "Saved: ~D" *sheeps-saved*)
                     :font *default-mono-font*
                     :size 16
                     :x 680
                     :y 580)
    (p2dg::draw-text (format nil "Level: ~D" *current-level*)
                     :font *default-mono-font*
                     :size 16
                     :x 20
                     :y 580))

  (draw-debug-markers)
  (clear-debug-markers))


;;; Victory screen

(defun victory/enter ()
  (log:info "Entering Victory screen.")
  (setf *screen-switch-countdown* +screen-switch-delay+)
  
  (setf *screen-keyboard-handler* nil
        *screen-mouse-handler* nil
        *screen-update-handler* #'victory/update
        *screen-render-handler* (lambda ()
                                  (main-game/draw)
                                  (victory/draw))))

(defun victory/update (dt)
  (decf *screen-switch-countdown* dt)
  (when (< *screen-switch-countdown* 0)
    (get-ready/enter)))

(defun victory/draw ()
  ;; TODO draw a full-screen dark quad
  (p2dg:with-color (0 0 0 1)
    (p2dg::draw-text "You win."
                     :font *default-mono-font*
                     :size 16
                     :x 400
                     :y 300)))


;;; Defeat screen

(defun defeat/enter ()
  (log:info "Entering Defeat screen.")
  (setf *screen-switch-countdown* +screen-switch-delay+)
  
  (setf *screen-keyboard-handler* nil
        *screen-mouse-handler* nil
        *screen-update-handler* #'defeat/update
        *screen-render-handler* (lambda ()
                                  (main-game/draw)
                                  (defeat/draw))))

(defun defeat/update (dt)
  (decf *screen-switch-countdown* dt)
  (when (< *screen-switch-countdown* 0)
    (intro/enter)))

(defun defeat/draw ()
  ;; TODO draw a full-screen dark quad
  (p2dg:with-color (0 0 0 1)
    (p2dg::draw-text "You lose."
                     :font *default-mono-font*
                     :size 16
                     :x 400
                     :y 300)))
