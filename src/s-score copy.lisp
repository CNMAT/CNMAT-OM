;;;==================================
;;; S-SCORE
;;;==================================






;;;==================================
;;; S-COMBINE-POLYS
;;;==================================

(in-package :cnmat)



(om::defmethod! s-combine-polys ((poly1 poly) (poly2 poly))

  :icon 7
  :indoc '("a poly object" "a second poly object")
  :outdoc '("Joins (concatenates) one poly to another. Outputs a poly object.") 
  :initvals '('(nil) '(nil))
  :doc "Joins one poly to another. Use this when the concat object won't work, i.e. when segments of music don't end tidly at the end of a bar. Joins voices in the poly according to these rules: If the last rhythm is a rest then this last rest is deleted and the new voice is joined snug with the last pitch.Takes tempo and legato from the first voice. Outputs a poly object."

  (let* ((combined-voices (mapcar (lambda (x y) (s-combine-voices x y)) (om::voices poly1) (om::voices poly2))))

  (make-instance 'poly
                 :voices combined-voices)
  )

)



;;;==================================
;;; S-CUTS
;;;==================================

(in-package :cnmat)


(defun cuts-rests (duration tatum)

     (flat (list tatum (om* (om- duration tatum) -1)))

)

(defun make-voice-cuts-rests (meter durations-list tatum new-pitches tempo)
  (let* ((flat-pitches (flat new-pitches))
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (cuts-events (mapcar (lambda (x) (cuts-rests x tatum)) (om* durations-list tatum))))
    ;;return a list with both voices
       (list
    ;;this voice is the running version
         (make-instance 'voice 
                   :tree (mktree (repeat-n tatum (length flat-pitches)) meter)
                   :chords flat-pitches 
                   :tempo tempo 
                   )
    ;;; this voice is the cut in

          (make-instance 'voice 
                   :tree (mktree (remove 0 (flat cuts-events)) meter)
                   :chords cuts-pitches 
                   :tempo tempo 
                   )
       
        )
    )
)



(defun make-voice-cuts (meter durations-list tatum new-pitches tempo)
  (let* ((flat-pitches (flat new-pitches))
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list)))))
    ;;return a list with both voices
       (list
    ;;this voice is the running version
         (make-instance 'voice 
                   :tree (mktree (repeat-n tatum (length flat-pitches)) meter)
                   :chords flat-pitches 
                   :tempo tempo 
                   )
    ;;; this voice is the cut in

          (make-instance 'voice 
                   :tree (mktree (om* durations-list tatum) meter)
                   :chords cuts-pitches 
                   :tempo tempo 
                   )
       
        )
    )
)

(defun check-pitchlist-vs-rhythmlist (rhythm-list pitch-list)

;;For each voice, if the sum of rhythm-list > len(pitchlist) then repeat the pitchlist until it
;;fills the length of the rhythm list

;;output new pitchlist for the processing

  (let* ((sum-rhythms (reduce '+ rhythm-list))
        (num-events (om-round (om/ sum-rhythms (length (flat pitch-list))))))
    
        (flat (repeat-n (flat pitch-list) num-events))
   )

)


;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-cuts ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 110 0)
  :doc "Converts a rotation list into music notation using the ."
  
  (let* (( flat-rhythm-lists (mapcar (lambda (x) (flat x)) durations-list))
        (flat-pitch-lists (mapcar (lambda (x) (flat x)) pitches))
        (prepped-pitches (mapcar (lambda (x y) (check-pitchlist-vs-rhythmlist x y)) flat-rhythm-lists flat-pitch-lists)))

    
  (case mode

    (0 (flat (mapcar (lambda (x y) (make-voice-cuts meter x tatum y tempo)) flat-rhythm-lists  prepped-pitches) 1)
       )
    (1 (flat (mapcar (lambda (x y) (make-voice-cuts-rests meter x tatum y tempo)) flat-rhythm-lists  prepped-pitches) 1)
       )
       
       )
  )
)


;;;this receives from rfi object
(om::defmethod! s-cuts ( (durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (make-voice-cuts durations meter tatum pitches tempo mode)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;





;;;==================================
;;; S-CUTS2
;;;==================================

(in-package :cnmat)


(defun cuts-rests (duration tatum)

     (flat (list tatum (om* (om- duration tatum) -1)))

)

(defun pulse-pitches (flat-pitches durations-list)

  ;; get the right pitches at the specific index called for
  (let ((pitch-at-index (posn-match flat-pitches  (dx->x 0 durations-list))))

  ;;return the right repeated pitches for the pulse voice
  (flat (mapcar (lambda (x y) (repeat-n x y)) pitch-at-index durations-list))
  )

)




;;;;
;;;;
;;;;

(defun make-voice-cuts-pulse2 (meter durations-list tatum new-pitches tempo generated-rhythms generated-tatums)
  (let* ((flat-pitches (flat new-pitches))
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (cuts-events (mapcar (lambda (x) (cuts-rests x tatum)) (om* durations-list tatum)))
         ;;match attacks with the correct tatum in the tatum list
         (attack-durations (posn-match generated-tatums (butlast (dx->x 0 (om-  durations-list 0)))))
         ;;now add the rests back in to account for the time between attacks
         (rest-durations  (om* -1 (mapcar (lambda (x y) (- x y)) generated-rhythms attack-durations)))
         ;;this then created the final list of attacks with rests for the rests version of the cut-ins patch
         (attacks+rests-durations (remove 0 (flat (mapcar (lambda (x y) (list x y)) attack-durations rest-durations)))) 
         (pulsing-pitches (pulse-pitches flat-pitches durations-list)) )



       (list
    ;;this voice is the running version
         (make-instance 'voice 
                   :tree (mktree generated-tatums meter)
                   :chords flat-pitches 
                   :tempo tempo 
                   )
    ;;; this voice is the cut in

          (make-instance 'voice 
                   ;;use the tatums as the pulse rhythms
                   :tree (mktree generated-tatums meter)
                   :chords pulsing-pitches
                   :tempo tempo 
                   )
       
        )
    )
)




(defun make-voice-cuts-rests2 (meter durations-list tatum new-pitches tempo generated-rhythms generated-tatums)
  (let* ((flat-pitches (flat new-pitches))
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (cuts-events (mapcar (lambda (x) (cuts-rests x tatum)) (om* durations-list tatum)))
         ;;match attacks with the correct tatum in the tatum list
         (attack-durations (posn-match generated-tatums (butlast (dx->x 0 (om-  durations-list 0)))))
         ;;now add the rests back in to account for the time between attacks
         (rest-durations  (om* -1 (mapcar (lambda (x y) (- x y)) generated-rhythms attack-durations)))
         ;;this then created the final list of attacks with rests for the rests version of the cut-ins patch
         (attacks+rests-durations (remove 0 (flat (mapcar (lambda (x y) (list x y)) attack-durations rest-durations)))) )

       (list
    ;;this voice is the running version
         (make-instance 'voice 
                   :tree (mktree generated-tatums meter)
                   :chords flat-pitches 
                   :tempo tempo 
                   )
    ;;; this voice is the cut in

          (make-instance 'voice 
                   :tree (mktree attacks+rests-durations meter)
                   :chords cuts-pitches 
                   :tempo tempo 
                   )
       
        )
    )
)



(defun make-voice-cuts2 (meter durations-list tatum new-pitches tempo generated-rhythms generated-tatums)
  (let* ((flat-pitches (flat new-pitches))
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list)))))
    ;;return a list with both voices
       (list
    ;;this voice is the running version
         (make-instance 'voice 
                   :tree (mktree generated-tatums meter)
                   :chords flat-pitches 
                   :tempo tempo 
                   )
    ;;; this voice is the cut in

          (make-instance 'voice 
                   :tree (mktree generated-rhythms meter)
                   :chords cuts-pitches 
                   :tempo tempo 
                   )
       
        )
    )
)

(defun check-pitchlist-vs-rhythmlist (rhythm-list pitch-list)

;;For each voice, if the sum of rhythm-list > len(pitchlist) then repeat the pitchlist until it
;;fills the length of the rhythm list

;;output new pitchlist for the processing

  (let* ((sum-rhythms (reduce '+ rhythm-list))
        (num-events (om-round (om/ sum-rhythms (length (flat pitch-list))))))
    
        (flat (repeat-n (flat pitch-list) num-events))
   )

)

(defun prep-tatum-helper (tatum-list-elem)

  (let* (( elem (flat tatum-list-elem))
         (num-times  (first elem))
         (self (repeat-n (om/ 1 (second elem)) (om/ (second elem) 4))))  
    
  (flat (repeat-n self num-times ))
   )
)


(defun prep-tatum (tatum-list)
;; map through each voice
     (flat (mapcar (lambda (x) (prep-tatum-helper x)) tatum-list))

)

(defun generate-rhythms (durations prepped-tatums)
  (let* (( n-times (ceiling  (reduce '+ (flat durations)) (length prepped-tatums)))
         (self prepped-tatums)
         (list-to-group (flat (repeat-n self n-times)))
         (grouped-list (group-list list-to-group (flat durations) 'linear)))

    (mapcar (lambda (x) (reduce '+ x)) grouped-list)

  )
)

(defun generate-tatums (durations prepped-tatums)
  (let* (( n-times (ceiling  (reduce '+ (flat durations)) (length prepped-tatums)))
         (self prepped-tatums))
        
    (flat (repeat-n self n-times))
  )
)
  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-cuts2 ( (durations-list list) (meter list) (tatum-list list) (pitches list) (tempo integer) &optional (mode 0))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "list of lists of tatums for each voice" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) (((1 (16)))) ((6100)) 110 0)
  :doc "Converts a rotation list into music notation using the ."
  
  (let* (( flat-rhythm-lists (mapcar (lambda (x) (flat x)) durations-list))
        (flat-pitch-lists (mapcar (lambda (x) (flat x)) pitches))
        (prepped-pitches (mapcar (lambda (x y) (check-pitchlist-vs-rhythmlist x y)) flat-rhythm-lists flat-pitch-lists))
        (prepped-tatums (mapcar (lambda (x) (prep-tatum x)) tatum-list))
        (generated-rhythms (mapcar (lambda (x y) (generate-rhythms x y)) durations-list prepped-tatums))
        (generated-tatums (mapcar (lambda (x y) (generate-tatums x y)) durations-list prepped-tatums)))

    
  (case mode

    (0 (flat (mapcar (lambda (x y z a) (make-voice-cuts2 meter x tatum-list y tempo z a)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums) 1)
       )
    (1 (flat (mapcar (lambda (x y z a) (make-voice-cuts-rests2 meter x tatum-list y tempo z a)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums) 1)
       )

    (2 (flat (mapcar (lambda (x y z a) (make-voice-cuts-pulse2 meter x tatum-list y tempo z a)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums) 1)
       )
       
    )
  )
  

)


;;;this receives from rfi object
(om::defmethod! s-cuts2 ( (durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (make-voice-cuts durations meter tatum pitches tempo mode)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;


;;;==================================
;;; S-POLY
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun make-voice (meter durations-list tatum pitches tempo)
  (let ((tatum-durs (print (om::om* (print durations-list) tatum) )))
    (make-instance 'voice 
                   :tree (mktree (flat tatum-durs) meter)
                   :chords pitches 
                   :tempo tempo 
                   )
    ;;tatum-durs
    )
  )

(defun make-voice-and-rests (meter durations-list tatum pitches tempo)
  (let ((rhythm-list (print (prep-rests (print durations-list) tatum) )))
    (make-instance 'voice 
                   :tree (mktree (remove 0 (flat rhythm-list)) meter)
                   :chords pitches 
                   :tempo tempo 
                   )
    ;;tatum-durs
    )
  )

(defun prep-rests (durations-list tatum)
  (let ((hold-durs (om::om* durations-list tatum)))
    (mapcar (lambda (x) (flat (list tatum (om::om* (om- x tatum) -1))))  hold-durs)
    )
    ;;(mapcar (lambda (x) (rest-helper x tatum)) tatum-durs)
)



  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-poly ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 110 0)
  :doc "Converts a rotation list into music notation."
  
  (case mode

    (0 (mapcar (lambda (x y) (make-voice meter x tatum y tempo)) durations-list pitches )
       )
    (1 (mapcar (lambda (x y) (make-voice-and-rests meter x tatum y tempo)) durations-list pitches )
       )
    )

)


;;;this receives from rfi object
(om::defmethod! s-poly ( (durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (s-poly durations meter tatum pitches tempo mode)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;





;;;==================================
;;; S-POLY->VOICE
;;;==================================

;;;simple conversion from poly to voice


(in-package :cnmat)



(defun poly->chordseq (mypoly)
;;turn a poly object into a chordseq before turning the chordseq into a voice
  (let ((new-chord-seq (make-instance 'chord-seq)))
   (om::objFromObjs mypoly new-chord-seq))

)



(om::defmethod! s-poly->voice ((mypoly poly) &optional (mode 0) )
  :icon 2
  :indoc '("A poly object--make sure that it is locked." "mode:0 = notes truncated on following attacks; mode: 1 = notes sustained for original durations.")
  :outdoc '("A voice object") 
  :initvals '('(nil) 0)
  :doc "Does a simple conversion from a poly to a voice.  Be sure to lock the input poly object.  Mode 0 truncates note durations on the following attack.  Mode 1 sustains notes through attacks using the original durations. In OM6.10.1 mode 1 requires a special patch to remedy a bug"

  (case mode
    (0  (let ((my-chordseq ( poly->chordseq mypoly))
          (my-tempo-map (get-tempomap (nth 0 (om::voices mypoly)))))

      (cseq+tempo->voice my-chordseq my-tempo-map)
    
      )
     )
     
    (1 (reduce 'merger (om::voices mypoly))
     )
  )


)
  



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
(om::defmethod! s-poly2 ( (durations-list list) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))

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
(om::defmethod! s-poly2 ((durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (flat-voices durations-list))))

  (print 'durations)
  (print durations)
    (s-poly2 durations meter tatum pitches tempo mode))

)

;;;make mode 1 2 3 for sustain, rest, pulses
;;;


;;;==================================
;;; S-VOICE
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun make-voice (meter durations-list tatum pitches tempo)
  (let ((tatum-durs (print (om::om*  durations-list tatum) )))
    (make-instance 'voice 
                   :tree (mktree (flat tatum-durs) meter)
                   :chords pitches 
                   :tempo tempo 
                   )
    ;;tatum-durs
    )
  )


(defun make-poly (myvoices)
    (make-instance 'poly
                   :voices myvoices
                   )
)

(defun make-chord-seq (mypoly)
   (let ((new-chord-seq (make-instance 'chord-seq)))
   (om::objFromObjs mypoly new-chord-seq))
)

(defun reduce-to-one-voice (my-chordseq my-tempo-map)
 
    (cseq+tempo->voice my-chordseq my-tempo-map)
)

(defun get-tempo-map (myvoice)
 
    (get-tempomap myvoice)
)

;;;;WORK ON REDUCE TO ONE VOICE NOW
  
;;;making the sustain version first
(om::defmethod! s-voice ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer")
  :outdoc '("a voice") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 100)
  :doc "Converts a rotation list into music notation."

  (let ((my-chordseq (make-chord-seq (make-poly (mapcar (lambda (x y) (make-voice meter x tatum y tempo)) durations-list pitches ))))
        (my-tempo-map (get-tempo-map (make-voice meter (first durations-list) tatum (first pitches) tempo))))
    
    (reduce-to-one-voice my-chordseq my-tempo-map)

    )
)


;;;making the sustain version first
(om::defmethod! s-voice ( (durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (rotations->voice durations meter tatum pitches tempo)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;







