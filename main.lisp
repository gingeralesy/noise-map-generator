(in-package #:mapgen)
(in-readtable :qtools)

(defvar *main* NIL)
(defparameter *grays* (make-array '(256)))

(define-widget main (QWidget)
  ((surface :initform NIL :accessor surface)
   (objects :initform NIL :accessor objects)))

(define-subwidget (main updater) (q+:make-qtimer main))

(define-subwidget (main background) (q+:make-qcolor 0 0 0))

(define-initializer (main setup)
  (dotimes (i (first (array-dimensions *grays*)))
    (with-finalizing ((color (q+:make-qcolor i i i)))
      (setf (elt *grays* i) (q+:rgba color))))
  (setf *main* main
        (q+:window-title main) "Map Generator"
        (q+:single-shot updater) T
        (surface main) (make-instance 'surface-map)
        (objects main) (make-instance 'object-map))
  (q+:start updater (round (/ 1000 30))))

(define-finalizer (main teardown)
  (v:info :mapgen.main "EXIT")
  (finalize (surface main))
  (finalize (objects main))
  (setf *main* NIL))

(define-slot (main tick) ()
  (declare (connected updater (timeout)))
  (let* ((start (internal-time-millis))
         (tile-size 8)
         (width (ceiling (/ (q+:width main) tile-size)))
         (height (ceiling (/ (q+:height main) tile-size))))
    (set-size (surface main) width height)
    (set-size (objects main) width height)
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
      (paint (surface main) painter)
      (paint (objects main) painter))))

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
