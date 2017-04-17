(in-package #:lispjam-2017-04-temporal)

(defclass ljgame (p2d:game)
  ())

(defun run ()
  (p2d:run :game (make-instance 'ljgame)))

(defun run-with-profiling (&optional (mode :cpu))
  (p2d:run :game (make-instance 'ljgame)
           :profiling-mode mode))
