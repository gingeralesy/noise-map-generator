(in-package #:mapgen)
(in-readtable :qtools)

(defclass object-map (generated-map)
  ((tile-colours :initform NIL :accessor tile-colours)))

(defmethod initialize-instance :after ((surfmap object-map) &key)
  (setf (tile-colours surfmap) (make-array 4
                                           :initial-contents (list (list 150 (q+:make-qcolor 255 0 0))
                                                                   (list 60 (q+:make-qcolor 255 0 0))
                                                                   (list 170 (q+:make-qcolor 0 0 255))
                                                                   (list 80 (q+:make-qcolor 0 0 255))))))

(defmethod draw-map ((surfmap object-map))
  (let ((map (noise-map surfmap)))
    (when map
      (let* ((start (internal-time-millis))
             (width (first (array-dimensions map)))
             (height (second (array-dimensions map)))
             (image (q+:make-qimage width height (q+:qimage.format_argb32))))
        (dotimes (x width)
          (dotimes (y height)
            (let* ((value (aref map x y))
                   (tile-colour (tile-colour surfmap value)))
              (when tile-colour
                (set-pixel image x y tile-colour)))))
        (v:log :debug :mapgen "Redrawing the map of size (~a x ~a) took ~a ms."
               width height (- (internal-time-millis) start))
        (when (map-image surfmap)
          (finalize (map-image surfmap))
          (setf (map-image surfmap) NIL))
        (setf (map-image surfmap) image
              (redraw surfmap) NIL)))))

(defmethod tile-colour ((surfmap object-map) value)
  (let ((colours (tile-colours surfmap))
        (ret-colour))
    (for:for ((colour-info across colours))
      (until ret-colour)
      (let ((point (first colour-info))
            (colour (second colour-info)))
        (when (<= (abs (- value point)) (random 5))
          (setf ret-colour (q+:rgb colour)))))
    ret-colour))

(defmethod finalize ((surfmap object-map))
  (for:for ((color across (tile-colours surfmap)))
    (finalize (second color)))
  (setf (tile-colours surfmap) NIL))
