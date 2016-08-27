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
        (push (list zone object) object-zones)))
    (setf (tile-objects objmap) (make-array (length object-zones) :initial-contents object-zones))))

(defmethod set-size ((objmap object-map) width height)
  (call-next-method)
  (let ((obj-zones))
    (for:for ((object across (tile-objects objmap)))
      (for:for ((zone in (zones object))) ;; FIXME: why in bloody hells does (zones object) say there's no applicaple generic function?
        (push (cons zone object) obj-zones)))
    (setf (object-zones objmap) obj-zones)))

(defmethod draw-map ((objmap object-map))
  (let ((map (noise-map objmap)))
    (when map
      (let* ((start (internal-time-millis))
             (width (first (array-dimensions map)))
             (height (second (array-dimensions map)))
             (image (q+:make-qimage width height (q+:qimage.format_argb32))))
        (dotimes (x width)
          (dotimes (y height)
            (let* ((value (aref map x y))
                   (tile-object (tile-object objmap value))
                   (min-dist (min-distance tile-object))
                   (double-min (* 2 min-dist))
                   (safe)
                   (locations))
              (when tile-object
                (let ((x (- x min-dist))
                      (y (- y min-dist)))
                  (for:for ((i repeat (* double-min double-min)))
                    (until (not safe))
                    (let ((x1 (+ x (mod i double-min)))
                          (y1 (+ y (floor (/ i double-min)))))
                      (for:for ((loc in locations))
                        (until (not safe))
                        (setf safe (or (/= x1 (car loc)) (/= y1 (cdr loc))))))))
                ;; TODO: count nearby objects to ensure cluster size does not grow too big
                ;; TODO: add some randomness to whether or not it applies an object here
                (when safe
                  (push (cons x y) locations)
                  (set-pixel image x y (q+:rgb (colour tile-object))))))))
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
      (setf object (assoc (+ value i) (objects objmap))))
    object))

(defmethod finalize ((objmap object-map))
  (for:for ((object across (tile-objects objmap)))
    (finalize object))
  (setf (tile-objects objmap) NIL))
