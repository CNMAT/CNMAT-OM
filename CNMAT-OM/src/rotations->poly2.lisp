;;;==================================
;;; ROTATIONS->POLY2
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun prep-pitches (pitches durations-list)
 
  (let* ((abs-durations (mapcar #'abs durations-list))
        (this-duration-sum (reduce #'+ (flat abs-durations))))
       ; (this-duration-sum (reduce #'+ (flat durations-list))))


   (flat (repeat-n (flat pitches) (ceiling this-duration-sum (length pitches))))
    )
)

(defun prep-pitches-pulse (pitches durations-list)
 
  (let* ((abs-durations (mapcar #'abs durations-list))
         ;(this-duration-sum (reduce #'+ (flat durations-list)))
         (this-duration-sum (reduce #'+ (flat abs-durations)))

        (these-pitches (flat (repeat-n (flat pitches) (ceiling this-duration-sum (length pitches))))))
    

    ;(flat (mapcar (lambda (x y) (repeat-n x y)) these-pitches (flat durations-list)))
    (flat (mapcar (lambda (x y) (repeat-n x y)) these-pitches (flat abs-durations)))




    )
    
)

(defun prep-pitches-rest (preped-pitches durations-list)
 
  (let* ((abs-durations (mapcar #'abs durations-list))
        (attacks (butlast (dx->x 0 (flat abs-durations)))))
        ;(attacks (butlast (dx->x 0 (flat durations-list)))))

  
    (mapcar (lambda (x) (nth x preped-pitches)) attacks)
    )
    
)

(defun prep-group-rhythms (durations-list prepared-tatums)
 
  (let* ((abs-durations (mapcar (lambda (x) (abs x)) durations-list))
         
        (this-duration-sum (reduce #'+  (flat abs-durations))))
        ;(this-duration-sum (reduce #'+  (flat durations-list))))
    (print 'this-duration-sum)
    (print this-duration-sum)


    (ceiling this-duration-sum (length (flat prepared-tatums)))
    )
)

(defun prep-rhythms-rest (grouped-list durations-list)
 
  (let* ((abs-rhythms (mapcar #'abs grouped-list))
         (abs-durations-list (mapcar #'abs durations-list))
         (neg-rhythms (om* abs-rhythms -1))
         ;(neg-rhythms (om* grouped-list -1))
        (flat-durations-list (butlast(dx->x 0 (flat abs-durations-list))))
        ;(flat-durations-list (butlast(dx->x 0 (flat durations-list))))

        (rhythm-values (mapcar (lambda (x) (nth x  (flat neg-rhythms))) flat-durations-list))
        (multiplied-rhythm-values (om* rhythm-values -1)))
   
    ;(print 'durations-list)
    ;(print durations-list)
    
     (print 'subs-posn)
     (print(subs-posn (flat neg-rhythms) flat-durations-list (print multiplied-rhythm-values)))

    )
)

(defun return-pos-neg (elements-list durations-list)
  (let ((hold-list '()))
  (loop for elem in elements-list
        for duration in durations-list 
        do
        (if (> duration 0) (push elem hold-list) (push (om::om* -1 elem) hold-list))
        )

  (reverse hold-list)
)

)


(defun group-list-rhythms (prepared-tatums durations-list)
  (let* ( (list-to-group (print (flat (repeat-n (flat prepared-tatums)  (prep-group-rhythms durations-list prepared-tatums)))))
         (abs-durations (mapcar #'abs durations-list))
        (segmentation (flat durations-list))
        (abs-segmentation (flat abs-durations))
        (grouped-list  (om::group-list list-to-group abs-segmentation ::linear ))
        (grouped-list-with-rests (return-pos-neg grouped-list durations-list)))
        

   ;(print 'list-to-group)
  ; (print list-to-group)
  ; (print 'segmentation)
  ; (print segmentation)
   ;(print 'grouped-list-with-rests)
  ; (print grouped-list-with-rests)
   
  ;(om::group-list list-to-group segmentation ::linear )
   grouped-list-with-rests
 
 )
)



(defun group-list-rhythms-pulse (prepared-tatums durations-list)
  (let* ( (list-to-group (print (flat (repeat-n (flat prepared-tatums) (prep-group-rhythms durations-list prepared-tatums)))))
         (abs-duration-list (mapcar #'abs durations-list))
        ;(add-input (reduce #'+ (flat durations-list))))
        (add-input (reduce #'+ (flat abs-duration-list))))


  ;(print 'list-to-group)
  ;(print list-to-group)
  ;(print 'add-input)
  ;(print add-input)

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
    (print 'durations-list)
    (print durations-list)

    (make-instance 'voice 
                   :tree (mktree final-rhythms meter)
                   :chords preped-pitches 
                   :tempo tempo 
                   )
    )
  )

(defun last-pulsed-rhythms (grouped-list durations-list)
  (let* ((abs-durations-list (mapcar #'abs durations-list))
        (abs-dx (dx->x 0 abs-durations-list))
        (abs-dur-subseq (mapcar (lambda (x y) (subseq grouped-list x y))  abs-dx (cdr abs-dx)))
        (last-pulsed-rhythms '()))

    (print 'durations-list)
    (print durations-list)

    (loop for duration in durations-list
          for list-subseq in abs-dur-subseq do
          (if (< duration 0) 
              (push (om::om* -1 list-subseq) last-pulsed-rhythms) 
              ;(push 'blah last-pulsed-rhythms) 

              (push list-subseq last-pulsed-rhythms))
          )
    
    (print 'last-pulsed-rhythms)
    (print (reverse last-pulsed-rhythms))
    (flat (reverse last-pulsed-rhythms))
    )
)

(defun make-voice-pulse2 (meter durations-list tatum new-pitches tempo)
  (let* ((prepared-tatums (prep-tatums tatum durations-list))
           (grouped-list (print (group-list-rhythms-pulse prepared-tatums durations-list)))
           (preped-pitches (print (prep-pitches-pulse new-pitches durations-list)))
           (final-rhythms (last-pulsed-rhythms grouped-list durations-list))) 

    (print 'grouped-list)
    (print grouped-list)
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
           (last-final-rhythms (last-rests durations-list final-rhythms-pulse))
           )
    (print 'final-rhythms-pulse)
    (print final-rhythms-pulse)

    (make-instance 'voice 
                   ;:tree (mktree final-rhythms-pulse meter)
                   :tree (mktree last-final-rhythms meter)
                   :chords final-pitches 
                   :tempo tempo 
                   )
    )
  )

(defun last-rests (durations-list final-rhythms-pulse)
;get those last rests in if you use negative numbers in your durations-list
  (let* ((last-final-rhythms final-rhythms-pulse)
         ;(neg-final-rhythms-pulse (om::om* final-rhythms-pulse -1))
        (abs-durations-list (mapcar #'abs durations-list))
        (negs (dx->x 0 abs-durations-list)))
    


    (loop for duration in durations-list 
          for neg in (om::butlast negs) do
          (if (<  duration 0) 
            (setf (nth neg last-final-rhythms) (- (nth neg final-rhythms-pulse)))
            ;(setf last-final-rhythms (subs-posn last-final-rhythms neg (- (nth neg final-rhythms-pulse))))
            ))


    (print 'negs)
    (print negs)
    (print 'durations-list)
    (print durations-list)
    (print 'final-rhythms-pulse)
    (print final-rhythms-pulse)
    (print 'last-final-rhythms)
    (print last-final-rhythms)

    last-final-rhythms
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

  (print 'durations)
  (print durations)
    (rotations->poly2 durations meter tatum pitches tempo mode))

)

;;;make mode 1 2 3 for sustain, rest, pulses
;;;