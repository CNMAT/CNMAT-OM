;;;===================================================
;;; A STRUCTURE TO HANDLE/VISUALIZE RHYTHMIC FRAMES
;;;===================================================

(in-package :cnmat)

(defclass! rhythmic-frame ()
  ((size :initform 16 :accessor size)
   (pulses :initform '(4 4 4 4) :initarg :pulses :accessor pulses)))

(defmethod initialize-instance :after ((self rhythmic-frame) &rest args)
  (setf (size self) (apply '+ (om-abs (pulses self)))))


(defclass! polyrhythmic-frame ()
  ((voices :initform nil :initarg :voices :accessor voices)))

(defclass! prf (polyrhythmic-frame) ())

(defmethod initialize-instance :after ((self polyrhythmic-frame) &rest args)
  (setf (voices self)
        (loop for voice in (voices self) collect
              (if (listp voice) 
                  (make-instance 'rhythmic-frame :pulses voice)
                voice)))
  self)

(defmethod flat-voices ((self polyrhythmic-frame))
  (flat (mapcar 'flat-voices (voices self))))

(defmethod flat-voices ((self rhythmic-frame)) self)



(defmethod om::draw-mini-view  ((view t) (value rhythmic-frame)) 
  (oa:om-with-focused-view view 
    (draw-rhythmic-line (pulses value) (size value) 10 (- (om::w view) 20) 10 (- (om::h view) 20))))

(defmethod get-r-frame-size ((self rhythmic-frame)) (size self))
(defmethod get-r-frame-size ((self polyrhythmic-frame)) 
  (if (voices self)
    (apply 'max (mapcar 'get-r-frame-size (voices self)))
    1))


(defmethod om::draw-mini-view  ((view t) (value polyrhythmic-frame)) 
  (when (voices value)
    (oa:om-with-focused-view view 
      (let ((maxsize (get-r-frame-size value)))
        (draw-rhythmic-line value maxsize 10 (- (om::w view) 20) 10 (- (om::h view) 20) 0)))))


(defmethod draw-rhythmic-line ((line polyrhythmic-frame) size x w y h i)
  (when (voices line)
    (let ((lineh (/ h (length (voices line)))))
    ;(oa::om-with-line '(2 2)
    ;  (oa::om-draw-line x y (+ x w) y))
      (om::om-with-fg-color nil (oa::om-make-color 0 0 0)   
        (loop for v in (voices line) 
              for i = 0 then (1+ i) do
              (draw-rhythmic-line v size x w (+ y (* i lineh)) lineh i))
        ))))
 
   
(defmethod draw-rhythmic-line ((line rhythmic-frame) size x w y h i)
  (let* ((pulses (pulses line))
         (yy (+ y (/ h 2)))
         (bs (/ w size))
         (plist (om::dx->x 0 (om-abs pulses)))
         (sign t))
    (oa::om-with-fg-color nil (oa::om-make-color 0 0 0)
    (oa::om-draw-line x yy (+ x w) yy)
    (loop for b = 0 then (+ b 1) while (<= b size) do
          (if sign (oa::om-draw-line (+ x (* b bs)) yy (+ x (* b bs)) (- yy 4)))
          (let ((pos (position b plist)))
            (when (and pos (nth pos pulses))
              (if (> (nth pos pulses) 0)
                (progn (setq sign t)
                  (oa::om-fill-ellipse (+ x 0.5 (* b bs)) (- yy 3) 3 3)
                  (oa:om-draw-string (+ x -3 (* b bs)) (- yy 8) 
                                     (om::integer-to-string (nth pos pulses))))
                (setq sign nil))
              )
            )))))


(defmethod! r-substitute ((rhythm rhythmic-frame) val subs) 
  (make-instance 
   'rhythmic-frame
   :pulses (flat (substitute subs val (pulses rhythm)))))

(defmethod! r-substitute ((rhythm polyrhythmic-frame) val subs) 
   (make-instance 
    'prf 
    :voices (mapcar #'(lambda (r) (r-substitute r val subs)) (voices rhythm))
    ))

