;;;==================================
;;; ROTATIONS->POLY2
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun prep-pitches (pitches durations-list)
 
  (let ((this-duration-sum (reduce #'+ (flat durations-list))))

   (flat (repeat-n (flat pitches) (ceiling this-duration-sum (length pitches))))
    )
)

(defun prep-pitches-pulse (pitches durations-list)
 
  (let* ((this-duration-sum (reduce #'+ (flat durations-list)))
        (these-pitches (flat (repeat-n (flat pitches) (ceiling this-duration-sum (length pitches))))))
    

    (flat (mapcar (lambda (x y) (repeat-n x y)) these-pitches (flat durations-list)))



    )
    
)

(defun prep-pitches-rest (preped-pitches durations-list)
 
  (let ((attacks (butlast (dx->x 0 (flat durations-list)))))
  
    (mapcar (lambda (x) (nth x preped-pitches)) attacks)
    )
    
)

(defun prep-group-rhythms (durations-list prepared-tatums)
 
  (let ((this-duration-sum (reduce #'+ (flat durations-list))))

    (ceiling this-duration-sum (length (flat prepared-tatums)))
    )
)

(defun prep-rhythms-rest (grouped-list durations-list)
 
  (let* ((neg-rhythms (om* grouped-list -1))
        (flat-durations-list (butlast(dx->x 0 (flat durations-list))))
        (rhythm-values (mapcar (lambda (x) (nth x  (flat neg-rhythms))) flat-durations-list))
        (multiplied-rhythm-values (om* rhythm-values -1)))

    (subs-posn (flat neg-rhythms) flat-durations-list (print multiplied-rhythm-values))

    )
)



(defun group-list-rhythms (prepared-tatums durations-list)
  (let ( (list-to-group (print (flat (repeat-n (flat prepared-tatums) (prep-group-rhythms durations-list prepared-tatums)))))
        (segmentation (flat durations-list)))
  (om::group-list list-to-group segmentation ::linear )
 )
)



(defun group-list-rhythms-pulse (prepared-tatums durations-list)
  (let ( (list-to-group (print (flat (repeat-n (flat prepared-tatums) (prep-group-rhythms durations-list prepared-tatums)))))
        (add-input (reduce #'+ (flat durations-list))))
  (om::subseq list-to-group 0 add-input )
 )
)



(defun subdivide-helper (pulse)

  (cond ((< pulse 4) 1)
        (t (/ pulse 4)))
)

(defun subdivide-tatums (tatum)

  (let ((n-times (first (flat tatum)))
        (self (repeat-n (/ 1 (second (flat tatum))) (subdivide-helper (second (flat tatum))))))

  (flat (repeat-n self n-times ))
  )
)


(defun prep-tatums (tatum-list durations-list)

  (flat (mapcar (lambda (x) (subdivide-tatums x))  tatum-list))
 
)


(defun make-voice2 (meter durations-list tatum new-pitches tempo)
  (let* ((prepared-tatums (prep-tatums tatum durations-list))
           (grouped-list (print (group-list-rhythms prepared-tatums durations-list)))
           (preped-pitches (print (prep-pitches new-pitches durations-list)))
           (final-rhythms (print (mapcar (lambda (x) (reduce #'+ x))  grouped-list)) ))
    (make-instance 'voice 
                   :tree (mktree final-rhythms meter)
                   :chords preped-pitches 
                   :tempo tempo 
                   )
    )
  )

(defun make-voice-pulse2 (meter durations-list tatum new-pitches tempo)
  (let* ((prepared-tatums (prep-tatums tatum durations-list))
           (grouped-list (print (group-list-rhythms-pulse prepared-tatums durations-list)))
           (preped-pitches (print (prep-pitches-pulse new-pitches durations-list)))
           (final-rhythms (print grouped-list)) )
    (make-instance 'voice 
                   :tree (mktree final-rhythms meter)
                   :chords preped-pitches 
                   :tempo tempo 
                   )
    )
  )

(defun make-voice-and-rests2 (meter durations-list tatum new-pitches tempo)
  (let* ((prepared-tatums (prep-tatums tatum durations-list))
           (grouped-list (print (group-list-rhythms-pulse prepared-tatums durations-list)))
           (preped-pitches (print (prep-pitches-pulse new-pitches durations-list)))
           (final-pitches (print (prep-pitches-rest preped-pitches durations-list)))
           (final-rhythms-pulse (print (prep-rhythms-rest grouped-list durations-list))) 
           )
    (make-instance 'voice 
                   :tree (mktree final-rhythms-pulse meter)
                   :chords final-pitches 
                   :tempo tempo 
                   )
    )
  )

(defun prep-rests (durations-list tatum)
  (let ((hold-durs (om* durations-list tatum)))
    (mapcar (lambda (x) (flat (list tatum (om* (om- x tatum) -1))))  hold-durs)
    )
    ;;(mapcar (lambda (x) (rest-helper x tatum)) tatum-durs)
)

(defun prep-meter (meter)

  (if  (equal (second meter) nil)
      (flat (car meter))
    meter)
)

  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! rotations->poly2 ( (durations-list list) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) (((1(16)) (2(20)))) ((6100)) 110 0)
  :doc "Converts a rotation list into music notation."
  
  (let (( preped-meter (prep-meter meter)))



    
  (case mode

    (0 (mapcar (lambda (x y z) (make-voice2 preped-meter x y z tempo)) durations-list tatum pitches)
       )
    (1 (mapcar (lambda (x y z) (make-voice-and-rests2 preped-meter x y z tempo)) durations-list tatum pitches)
       )

    (2 (mapcar (lambda (x y z) (make-voice-pulse2 preped-meter x y z tempo)) durations-list tatum pitches)
       )
    )
  )
  

)


;;;this receives from rfi object
(om::defmethod! rotations->poly2 ( (durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (rotations->poly2 durations meter tatum pitches tempo mode)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;