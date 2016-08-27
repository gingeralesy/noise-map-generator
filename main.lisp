(in-package #:mapgen)
(in-readtable :qtools)

(defvar *main* NIL)
(defparameter *grays* (make-array '(256)))

(define-widget main (QWidget)
  ((genmap :initform NIL :accessor genmap)))

(define-subwidget (main updater) (q+:make-qtimer main))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-initializer (main setup)
  (dotimes (i (first (array-dimensions *grays*)))
    (with-finalizing ((color (q+:make-qcolor i i i)))
      (setf (elt *grays* i) (q+:rgba color))))
  (setf *main* main
        (q+:window-title main) "Map Generator"
        (q+:single-shot updater) T
        (genmap main) (make-instance 'generated-map))
  (q+:start updater (round (/ 1000 30))))

(define-finalizer (main teardown)
  (v:info :mapgen.main "EXIT")
  (setf *main* NIL))

(define-slot (main tick) ()
  (declare (connected updater (timeout)))
  (let ((start (internal-time-millis)))
    (set-size (genmap main) (ceiling (/ (q+:width main) 16)) (ceiling (/ (q+:height main) 16)))
    (q+:repaint main)
    (q+:start updater (floor (max 0 (- (/ 1000 30)
                                       (- start (internal-time-millis))))))))

(define-override (main paint-event) (ev)
  (with-simple-restart (abort "Abort drawing and continue.")
    (with-finalizing ((painter (q+:make-qpainter main))
                      (bgbrush (q+:make-qbrush background)))
      (setf (q+:style (q+:background painter)) (q+:qt.solid-pattern)
            (q+:color (q+:background painter)) (q+:qt.black)
            (q+:style (q+:brush painter)) (q+:qt.solid-pattern))
      (q+:fill-rect painter (q+:rect main) bgbrush)
      (paint (genmap main) painter))))

(define-override (main wheel-event) (ev)
  (with-simple-restart (abort "Abort wheel event.")
    (scroll (genmap main) (/ (q+:delta ev) 24))))

(defun internal-time-millis ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(defun main (&rest initargs)
  (v:output-here)
  (v:info :mapgen.main "START")
  #+linux (q+:qcoreapplication-set-attribute (q+:qt.aa_x11-init-threads))
  (unwind-protect
       (with-main-window (window (apply #'make-instance 'main initargs)
                          #-darwin :main-thread #-darwin NIL))))
