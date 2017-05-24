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


(defmethod draw-rhythmic-line ((line polyrhythmic-frame) size x w y h i &optional selection)
  (when (voices line)
    (let ((lineh (/ h (length (voices line)))))
    ;(oa::om-with-line '(2 2)
    ;  (oa::om-draw-line x y (+ x w) y))
      (om::om-with-fg-color nil (oa::om-make-color 0 0 0)   
        (loop for v in (voices line) 
              for i = 0 then (1+ i) do
              (draw-rhythmic-line v size x w (+ y (* i lineh)) lineh i selection))
        ))))
 

(defun beat-graphic-positions (size w x)
  (let ((bs (/ w size)))
    (loop for i from 0 to size collect (+ x (* i bs)))))
         
(defun accent-beats (pulses)
  (om::dx->x 0 (om::om-abs pulses)))

(defmethod draw-rhythmic-line ((line rhythmic-frame) size x w y h i &optional selection)
  (let* ((pulses (pulses line))
         (pulses-pos (beat-graphic-positions size w x))
         (yy (+ y (/ h 2)))
         (plist (accent-beats pulses))
         (sign t))
    (oa::om-with-fg-color nil (oa::om-make-color 0 0 0)
      (oa::om-draw-line x yy (+ x (* w (/ (size line) size))) yy)
      (loop for b = 0 then (+ b 1) 
            for bx in pulses-pos do
            ;(if sign (oa::om-draw-line bx yy bx (- yy 4)))
            (let* ((pos (position b plist))
                   (beat (and pos (nth pos pulses))))
              (if beat
                (om::om-with-fg-color nil 
                    (if (find pos selection) oa::*om-dark-red-color* oa::*om-black-color*)
                  (if (> beat 0)
                      (progn (setq sign t)
                        (oa::om-fill-ellipse (+ 0.5 bx) (- yy 3) 3 3)
                        (oa:om-draw-string (- bx 4) (- yy 8) (om::integer-to-string beat)))
                    (progn (setq sign nil)
                      (oa::om-draw-ellipse (+ 0.5 bx) (+ yy 4) 3 3)
                      (oa:om-draw-string (- bx 8) (- yy 3) (om::integer-to-string beat)))
                    ))
                (when sign 
                  (oa::om-draw-line bx yy bx (- yy 4)))
                )))
      )))



;;;Matt's code for rfi starts here

(defun combinations-subs-children (number)

  (let ((all-allowable-numbers (arithm-ser 1 number 1))
      (pre-final-output '())
      (final-output '()))

    (push (cnmat::q-combi all-allowable-numbers number nil 2) pre-final-output)

    ;;;this cuts out just the single digit allowable number you start with
    (setf pre-final-output (list (butlast (car pre-final-output))))

    (loop for combination in (flat pre-final-output 1) do
      (push (list number combination) final-output))
    final-output
    )
)

(defun filter-disallowed-children (mylist disallowed)

  (let (( output '()))
    (loop for combination in mylist do
    ;;;keep the combination only if it has none of the disallowed in it

      (if (not (om::x-intersect (second combination) disallowed))
          (push combination output)))
    (reverse output)
    )
)



(defun build-children-prf (rhythm combination)

    (r-substitute rhythm (list (first combination)) (list (second combination)) 1)
)



(defun combine-children-prf (mypolys)

  (let ((holdvoices '()))

    (loop for myprf in mypolys do
      (push (cnmat::voices myprf) holdvoices))

    (make-instance
     'cnmat::prf
     :voices (flat (mat-trans (reverse holdvoices)))
    )
   )   
)

(defun combine-original+children (original-prf children-prf num-voices) 

    (let ((orig-prf-voices (cnmat::voices original-prf))
     
       (children-prf-voices  (group-list (cnmat::voices children-prf) (repeat-n (/ (length (cnmat::voices children-prf)) num-voices ) num-voices) 'linear))

      (all-voices '()))

     (loop for orig-voice in orig-prf-voices 
         for child-prf in children-prf-voices do
             (push (list orig-voice child-prf) all-voices))

     (flat (reverse all-voices))
    )
)


(defmethod! r-substitute-children ((rhythm polyrhythmic-frame) children-desired remove-values)

  :doc "Generate all children substitutions for a desired duration or set of durations..
"
  :icon 1

  (let* ((my-combinations (flat (mapcar (lambda (x)  (combinations-subs-children x)) children-desired) 1))
      (filtered-combinations (filter-disallowed-children my-combinations remove-values))
      (children-prfs (mapcar (lambda (x) (build-children-prf rhythm x)) filtered-combinations))
      (full-prf (combine-children-prf children-prfs))
      (original+children (combine-original+children rhythm full-prf (length (cnmat::voices rhythm) ))))

      original+children
  )

)



(defmethod! r-substitute ((rhythm rhythmic-frame) val subs &optional (mode 0)) 

  :doc "Replaces a rhythmic value with subset values (substitutions) that sum to the original value.
"
  :icon 1

    (cond ((= mode 1) (r-diminutions rhythm val subs))

      (t
       (let ((substitutions (local-substitute subs val (pulses rhythm))))

         (print 'substitutions)
         (print substitutions)

         (make-instance
          'rhythmic-frame

          :pulses  (flat substitutions)))))
)



(defmethod! r-substitute ((rhythm polyrhythmic-frame) val subs &optional (mode 0)) 

    (cond ((= mode 1) (r-diminutions rhythm val subs))
      (t

        (if (eq 0 (car (first subs)))

            (make-instance 
             'prf 
             :voices (mapcar #'(lambda (r s) (r-substitute r val (cdr s))) (voices rhythm) subs)
             )
          ;;Otherwise, you send the same subs list in for all the voices

        (make-instance 
            'prf 
            :voices (mapcar #'(lambda (r) (r-substitute r val subs)) (voices rhythm))
             ))))
)


(defun rests-positive-helper (mylist)
  (let* ((minus-list '())
        (positive-list '())
        (final-list '())
        (a-list '()))
    (print mylist)

    ;; check for negative number as last element.  In this case at that number (a rest) to the last attack
    ;;so that you dont get a separate attack for the last rest.
    (setf positive-list mylist)

    (loop for elem in (reverse mylist)
      until (plusp elem) 
      do 
      (push elem minus-list)
      (setf positive-list (butlast positive-list)))

    (push (flat (list (butlast positive-list) (+ (reduce #'+ (om* -1 minus-list)) (first (last positive-list))))) a-list)

    (loop for elem in (flat a-list) do
      (push (abs elem) final-list))
    (reverse final-list)
    )
)

(defun remove-rest-values-helper (durations-list intervals-list)

   (let ((final-list '())
      (pre-final-list '()))

    ;get rid of intervals corresponding with rests
    (loop for duration in durations-list
      for interval in intervals-list do
      (if (> duration 0) (push interval final-list)))
    (reverse final-list))

)

(defun r-merge-helper (rhythm myvoices)

  (cond ((listp (first myvoices))
         (mapcar (lambda (x) (r-merge-helper rhythm x)) myvoices))
       
        (t
         (print 'got-here)
         (make-instance
          'polyrhythmic-frame
          :voices (posn-match (voices rhythm) myvoices))))
)

(defun r-merge-helper2 (rhythm myvoices)
   (r-merge (r-merge-helper rhythm myvoices ))
)


(defmethod! r-merge ((rhythm polyrhythmic-frame) &optional myvoices me)

  :doc "R-merge collapes the attacks of all rhythmic frames within a polyrhythmic frame into one rhythmic frame."
  :icon 1

  (cond 
      (myvoices 
       (cond ((listp (first myvoices))
              (mapcar (lambda (x) (r-merge rhythm (flat x) me)) myvoices))
             (t
              (r-merge (r-merge-helper rhythm myvoices ) nil me))))

      (t 
       (let* ((positive-rhythms  (mapcar (lambda (x) (rests-positive-helper x)) (r-duration-list (voices rhythm))) )
              (intervals (mapcar (lambda (x) (dx->x 0 x)) positive-rhythms))
              (intervals-butlast (mapcar (lambda (x) (butlast x)) intervals))
              (intervals-remove-rests (mapcar (lambda (x y)  (remove-rest-values-helper x y)) positive-rhythms intervals-butlast))
              )

        ;;;if the input to myvoices was a list then output one way.  Otherwise output it as a list.
        
        (cond (me
               (progn (print 'nah)
               (x->dx (sort  (remove-dup (flat (list intervals-remove-rests (reduce #'+ (first positive-rhythms)) )) 'eq 1)'<))))

             (t
             (list (x->dx (sort  (remove-dup (flat (list intervals-remove-rests (reduce #'+ (first positive-rhythms)) )) 'eq 1)'<))))

         ))))
)

(defun r-tatum-mapping-helper (thing)

  (let ((final-list '()))
    (loop for elem in thing do
      (cond ((eql elem 1) (push '(1 (4)) final-list))
            ((eql elem 2) (push '(1 (8)) final-list))
            ((eql elem 3) (push '(1 (12)) final-list))
            ((eql elem 4) (push '(1 (16)) final-list))
            ((eql elem 5) (push '(1 (20)) final-list))
            ((eql elem 6) (push '(1 (24)) final-list))
            ((eql elem 8) (push '(1 (32)) final-list))
            ((eql elem 10) (push '(1 (40)) final-list))
            ((eql elem 12) (push '(2 (6)) final-list))
            ((eql elem 15) (push '(1 (2)) final-list))
            ((eql elem 20) (push '(1 (1)) final-list))
            (t (push 'nil final-list)))
            )

    (list (list (reverse final-list))))
)

(defmethod! r-tatum-mapping ((mything list))

  :doc "Maps numbers to tatums according to a key."
  :icon 1

   (flat (mapcar (lambda (x) (r-tatum-mapping-helper x)) mything) 2)
  
)

