(in-package #:mapgen)
(in-readtable :qtools)

(defclass surface-map (generated-map)
  ((tile-colours :initform NIL :accessor tile-colours)))

(defmethod initialize-instance :after ((surfmap surface-map) &key)
  (setf (tile-colours surfmap) (make-array 8
                                           :initial-contents (list
                                                              (q+:make-qcolor 136 168 114)
                                                              (q+:make-qcolor 136 168 114)
                                                              (q+:make-qcolor 136 168 114)
                                                              (q+:make-qcolor 168 199 135)
                                                              (q+:make-qcolor 208 198 140)
                                                              (q+:make-qcolor 191 175 116)
                                                              (q+:make-qcolor 190 169 111)
                                                              (q+:make-qcolor 190 169 111)))))

(defmethod draw-map ((surfmap surface-map))
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
              (set-pixel image x y tile-colour))))
        (v:log :debug :mapgen "Redrawing the map of size (~a x ~a) took ~a ms."
               width height (- (internal-time-millis) start))
        (when (map-image surfmap)
          (finalize (map-image surfmap))
          (setf (map-image surfmap) NIL))
        (setf (map-image surfmap) image
              (redraw surfmap) NIL)))))

(defmethod tile-colour ((surfmap surface-map) value)
  (let ((colours (tile-colours surfmap)))
    (q+:rgb (aref colours (floor (* (/ value 256) (first (array-dimensions colours))))))))

(defmethod finalize ((surfmap surface-map))
  (for:for ((color across (tile-colours surfmap)))
    (finalize color))
  (setf (tile-colours surfmap) NIL))
