;; This is an optimization for fast drawing of pixels into QImages made by Nicolas Hafner <shinmera@tymoon.eu>
(in-package #:cl+qt)
(in-readtable :qtools)

(defmacro with-stack (stack args &body body)
  `(cffi:with-foreign-object (,stack '(:union qt::StackItem) ,(length args))
     ,@(loop for (val type) in args
             for i from 0
             collect `(setf (cffi:foreign-slot-value (cffi:mem-aptr ,stack '(:union qt::StackItem) ,i)
                                                     '(:union qt::StackItem) ',(find-symbol (string type) :qt))
                            ,val))
     ,@body))

(defun find-qmethod (class name &rest argtypes)
  (qt::map-class-methods-named
   (lambda (method)
     (when (every #'string-equal argtypes (mapcar #'qt::qtype-interned-name (qt::list-qmethod-argument-types method)))
       (return-from find-qmethod method)))
   (etypecase class
     (integer class)
     (string (find-qclass class)))
   name))

(defmacro fast-call (method object stack)
  `(qt::call-class-fun (load-time-value
                        (qt::qclass-trampoline-fun
                         (qt::qmethod-class ,method)))
                       (load-time-value
                        (qt::qmethod-classfn-index ,method))
                       ,object
                       ,stack))

(declaim (inline set-pixel))
(defun set-pixel (image x y rgb)
  (let ((object (qt::qobject-pointer image)))
    (with-stack stack ((object ptr) (x int) (y int) (rgb uint))
      (fast-call (load-time-value (find-qmethod "QImage" "setPixel" 'int 'int 'unsigned\ int)) object stack))))
