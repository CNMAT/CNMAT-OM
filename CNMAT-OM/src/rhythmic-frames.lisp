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
  ((voices :initform nil :initarg :voices :accessor voices)
   ;(elements-per-voice :initform 2 :accessor elements-per-voice)
   ))

(defclass! prf (polyrhythmic-frame) ())

(defmethod initialize-instance :after ((self polyrhythmic-frame) &rest args)
  (setf (voices self)
        (loop for voice in (voices self) collect
              (if (listp voice) (make-instance 'rhythmic-frame :pulses voice)
                voice)))
  self)

(defmethod om::draw-mini-view  ((view t) (value rhythmic-frame)) 
  (oa:om-with-focused-view view 
    (draw-rhythmic-line (pulses value) (size value) 10 (- (om::w view) 20) 10 (- (om::h view) 20))))

(defmethod om::draw-mini-view  ((view t) (value polyrhythmic-frame)) 
  (when (voices value)
    (oa:om-with-focused-view view 
      (let ((maxsize (apply 'max (mapcar 'size (voices value))))
            (lineh (/ (- (om::h view) 20) (length (voices value)))))
        (loop for v in (voices value) 
              for i = 0 then (1+ i) do
              ;(om-api:om-with-fg-color view 
              ;    (if (= (mod i (- (elements-per-voice value) 0)) 0)
              ;        (om::om-make-color 1 0 0)
              ;        (om::om-make-color 0 0 1))
                (draw-rhythmic-line (pulses v) maxsize 10 (- (om::w view) 20) (+ 10 (* i lineh)) lineh))))))


(defun draw-rhythmic-line (pulses size x w y h)
  (let ((yy (+ y h -2))
        (bs (/ w size))
        (plist (om::dx->x 0 (om-abs pulses)))
        (sign t))
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
            ))))


(defmethod! r-substitute ((rhythm rhythmic-frame) val subs) 
  (make-instance 
   'rhythmic-frame
   :pulses (flat (substitute subs val (pulses rhythm)))))

(defmethod! r-substitute ((rhythm polyrhythmic-frame) val subs) 
   (make-instance 
    'prf 
    :voices (mapcar #'(lambda (r) (r-substitute r val subs)) (voices rhythm))
    ))

