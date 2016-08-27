(in-package #:mapgen)
(in-readtable :qtools)

(defclass map-object ()
  ((type-id :initform NIL :accessor type-id)
   (zones :initform NIL :accessor zones)
   (cluster-size :initform NIL :accessor cluster-size)
   (min-distance :initform NIL :accessor min-distance)
   (colour :initform NIL :accessor colour)))

(defmethod initialize-instance :after ((object map-object) &key zones colour
                                                                (cluster-size 10) (min-distance 1))
  (setf (zones object) zones
        (colour object) colour
        (cluster-size object) cluster-size
        (min-distance object) min-distance))

(defmethod finalize ((object map-object))
  (finalize (colour object)))

(defclass object-map (generated-map)
  ((tile-objects :initform NIL :accessor tile-objects)
   (object-zones :initform NIL :accessor object-zones)))

(defmethod initialize-instance :after ((objmap object-map) &key objects)
  (let ((object-zones))
    (for:for ((object in objects))
      (for:for ((zone in (zones object)))
        (push (cons zone object) object-zones)))
    (setf (object-zones objmap) object-zones)))

(defmethod set-size ((objmap object-map) width height)
  (call-next-method)
  (let ((objects))
    (for:for ((object-zone in (object-zones objmap)))
      (push (cdr object-zone) objects))
    (setf (tile-objects objmap) objects)))

(defmethod draw-map ((objmap object-map))
  (let ((map (noise-map objmap)))
    (when map
      (let* ((start (internal-time-millis))
             (width (first (array-dimensions map)))
             (height (second (array-dimensions map)))
             (image (q+:make-qimage width height (q+:qimage.format_argb32)))
             (locations))
        (dotimes (x width)
          (dotimes (y height)
            (let* ((value (aref map x y))
                   (tile-object (tile-object objmap value)))
              (when tile-object
                (let ((cluster-size (cluster-size tile-object)))
                  (when (and (< (count-objects x y (min-distance tile-object) locations) 1)
                             (< (count-objects x y (ceiling (/ cluster-size 2)) locations)
                                cluster-size)
                             (< (random 10) 5))
                    (push (cons x y) locations)
                    (set-pixel image x y (q+:rgb (colour tile-object)))))))))
        (v:log :debug :mapgen "Redrawing the map of size (~a x ~a) took ~a ms."
               width height (- (internal-time-millis) start))
        (when (map-image objmap)
          (finalize (map-image objmap))
          (setf (map-image objmap) NIL))
        (setf (map-image objmap) image
              (redraw objmap) NIL)))))

(defmethod tile-object ((objmap object-map) value)
  (let* ((object)
         (fuzzy (random 6))
         (value (- value fuzzy)))
    (for:for ((i repeat (* fuzzy 2)))
      (until object)
      (setf object (assoc (+ value i) (object-zones objmap))))
    (cdr object)))

(defmethod finalize ((objmap object-map))
  (for:for ((object in (tile-objects objmap)))
    (finalize object))
  (setf (tile-objects objmap) NIL))

(defun count-objects (x y distance locations)
  (let* ((double-min (1+ (* 2 distance)))
         (x (- x distance))
         (y (- y distance))
         (counter 0))
    (for:for ((i repeat (* double-min double-min)))
      (let ((x1 (+ x (mod i double-min)))
            (y1 (+ y (floor (/ i double-min)))))
        (for:for ((loc in locations))
          (when (and (= x1 (car loc)) (= y1 (cdr loc)))
            (incf counter)))))
    counter))
