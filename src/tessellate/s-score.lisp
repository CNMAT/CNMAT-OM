;;;==================================
;;; S-SCORE
;;;==================================


;;;==================================
;;; S-COMBINE-POLYS
;;;==================================

(in-package :cnmat)



(om::defmethod! s-combine-polys ((poly1 poly) (poly2 poly))

  :icon 1
  :indoc '("a poly object" "a second poly object")
  :outdoc '("Joins (concatenates) one poly to another. Outputs a poly object.") 
  :initvals '('(nil) '(nil))
  :doc "Combine poly scores. Use this when the concat object won't work, i.e. when segments of music don't end tidly at the end of a bar. Joins voices in the poly according to these rules: If the last rhythm is a rest then this last rest is deleted and the new voice is joined snug with the last pitch.Takes tempo and legato from the first voice. Outputs a poly object."

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
  (let* (
         (flat-pitches new-pitches)
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (cuts-events (mapcar (lambda (x) (cuts-rests x tatum)) (om* durations-list tatum))))
       ;;;return a list with both voices
       (list
       ;;;this voice is the running version
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
  (let* (
         (flat-pitches  new-pitches)
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

  ;;;For each voice, if the sum of rhythm-list > len(pitchlist) then repeat the pitchlist until it
  ;;;fills the length of the rhythm list

  ;;;output new pitchlist for the processing

  (let* ((sum-rhythms (reduce '+ rhythm-list))
        (num-events (om-round (om/ sum-rhythms (length pitch-list)))))
    
        (flat (repeat-n pitch-list  num-events) 1)

   )

)


;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-cuts ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))

  :icon 1
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 110 0)
  :doc "Cuts-style score creation."
  
  (let* (( flat-rhythm-lists (mapcar (lambda (x) (flat x)) durations-list))
        (flat-pitch-lists (mapcar (lambda (x) (flat x)) pitches))
        (prepped-pitches (mapcar (lambda (x y) (check-pitchlist-vs-rhythmlist x y)) flat-rhythm-lists pitches)))

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
  (flat (mapcar (lambda (x y) (repeat-n x y)) pitch-at-index durations-list) 1)
  )

)


(defun pulse-rest-helper (rhythms)

    (let (output '())

     (loop for rhythm in rhythms do
      (cond ((> rhythm 0) (push (repeat-n 1 rhythm) output))
            (t (push (repeat-n -1 (abs rhythm)) output))))

     (reverse (flat output))

    )
)



(defun make-voice-cuts-pulse2 (meter durations-list tatum new-pitches tempo generated-rhythms generated-tatums final-generated-rhythms)
  (let* (
         (flat-pitches  new-pitches)
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (cuts-events (mapcar (lambda (x) (cuts-rests x tatum)) (om* durations-list tatum)))
         ;;match attacks with the correct tatum in the tatum list
         (attack-durations (posn-match generated-tatums (butlast (dx->x 0 (om-  durations-list 0)))))
         ;;now add the rests back in to account for the time between attacks
         (rest-durations  (om* -1 (mapcar (lambda (x y) (- x y)) generated-rhythms attack-durations)))
         ;;this then created the final list of attacks with rests for the rests version of the cut-ins patch
         (attacks+rests-durations (remove 0 (flat (mapcar (lambda (x y) (list x y)) attack-durations rest-durations)))) 
         (pulsing-pitches (pulse-pitches flat-pitches durations-list)) 
         (pulse-rests (pulse-rest-helper final-generated-rhythms))
         (final-pulse-rhythm '())
         (final-pulsing-pitches '()))

    (loop for pulse-rest in pulse-rests
          for tatum in generated-tatums 
          for pitch in pulsing-pitches do
          (cond ((< pulse-rest 0) (push (* -1 tatum) final-pulse-rhythm))
                (t (push tatum final-pulse-rhythm)))
          (if (> pulse-rest 0) (push pitch final-pulsing-pitches)))
          
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
                   :tree (mktree (reverse final-pulse-rhythm) meter)
                   :chords (reverse final-pulsing-pitches)
                   :tempo tempo 
                   )
       
        )
    )
)




(defun make-voice-cuts-rests2 (meter durations-list tatum new-pitches tempo generated-rhythms generated-tatums final-generated-rhythms)
  (let* (
         ;(flat-pitches (flat new-pitches))
         (flat-pitches new-pitches)
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (cuts-events (mapcar (lambda (x) (cuts-rests x tatum)) (om* durations-list tatum)))
         ;;match attacks with the correct tatum in the tatum list
         (attack-durations (posn-match generated-tatums (butlast (dx->x 0 (om-  durations-list 0)))))
         ;;now add the rests back in to account for the time between attacks
         (rest-durations  (om* -1 (mapcar (lambda (x y) (- x y)) generated-rhythms attack-durations)))
         ;;this then created the final list of attacks with rests for the rests version of the cut-ins patch
         (attacks+rests-durations (remove 0 (flat (mapcar (lambda (x y) (list x y)) attack-durations rest-durations)))) 
         (final-cuts-pitches '())
         (final-attacks+rests-durations '()))

    (loop for duration in final-generated-rhythms 
          for pitch in cuts-pitches 
          for i from 0 to (length final-generated-rhythms) do
              (if (< 0 duration) (push pitch final-cuts-pitches))
              (if (> 0 duration) (setf (nth (* i 2) attacks+rests-durations) (* (nth (* i 2) attacks+rests-durations) -1)))
              
          )
         

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
                   :chords (reverse final-cuts-pitches)
                   :tempo tempo 
                   )
       
        )
    )
)



(defun make-voice-cuts2 (meter durations-list tatum new-pitches tempo generated-rhythms generated-tatums  final-generated-rhythms)
  (let* (
         
         (flat-pitches  new-pitches)
         (cuts-pitches (remove 'nil (posn-match flat-pitches (dx->x 0 durations-list))))
         (final-cuts-pitches '())

)


    (loop for duration in final-generated-rhythms 
          for pitch in cuts-pitches do
          (if (< 0 duration) (push pitch final-cuts-pitches)))

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
                   :tree (mktree final-generated-rhythms meter) 
                   :chords (reverse final-cuts-pitches)
                   :tempo tempo 
                   )
        )
    )
)

(defun check-pitchlist-vs-rhythmlist (rhythm-list pitch-list)

    ;;;For each voice, if the sum of rhythm-list > len(pitchlist) then repeat the pitchlist until it
    ;;;fills the length of the rhythm list
    ;;;output new pitchlist for the processing

  (let* ((sum-rhythms (reduce '+ rhythm-list))
        (num-events (om-round (om/ sum-rhythms (length  pitch-list))))
        )

         (flat (repeat-n  pitch-list num-events ) 1))

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

(defun check-for-rests (durations-list generated-rhythms)
   ;if the duration was originally a rest then make it a rest again.
  (let ((pre-final-rhythms '())
        (final-rhythms '()))
    (loop for list1voice in durations-list
          for list2voice in generated-rhythms do
      
      (loop for r-list1voice in list1voice
            for r-list2voice in list2voice do
            
            (cond ((> 0 r-list1voice) (push (* -1 r-list2voice) pre-final-rhythms))
                  (t (push r-list2voice pre-final-rhythms))))
      (push (reverse pre-final-rhythms) final-rhythms)
      (setf pre-final-rhythms '()))

    (reverse final-rhythms))
)

(defun abs-rhythms (rhythms)

  (print 'abs-rhythms)
  (print (mapcar (lambda (x) (abs x)) rhythms))

)

  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-cuts2 ( (durations-list list) (meter list) (tatum-list list) (pitches list) (tempo integer) &optional (mode 0))

  :icon 1
  :indoc '("a list for the meter" "a list of lists for durations" "list of lists of tatums for each voice" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) (((1 (16)))) ((6100)) 110 0)
  :doc "Cuts-style score creation using a changeable tatum list."
  
  (let* (( flat-rhythm-lists (mapcar (lambda (x) (abs-rhythms (flat x))) durations-list))
        (prepped-pitches (mapcar (lambda (x y) (check-pitchlist-vs-rhythmlist x y)) flat-rhythm-lists pitches))
        (prepped-tatums (mapcar (lambda (x) (prep-tatum x)) tatum-list))
        (generated-rhythms (mapcar (lambda (x y) (generate-rhythms x y))  flat-rhythm-lists prepped-tatums))
        (generated-tatums (mapcar (lambda (x y) (generate-tatums x y)) flat-rhythm-lists prepped-tatums))
        (final-generated-rhythms (check-for-rests durations-list generated-rhythms)))

  (case mode

    (0 (flat (mapcar (lambda (x y z a b) (make-voice-cuts2 meter x tatum-list y tempo z a b)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums final-generated-rhythms) 1)
       )
    (1 (flat (mapcar (lambda (x y z a b) (make-voice-cuts-rests2 meter x tatum-list y tempo z a b)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums final-generated-rhythms) 1)
       )
    (2 (flat (mapcar (lambda (x y z a b) (make-voice-cuts-pulse2 meter x tatum-list y tempo z a b)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums durations-list) 1)
       )
       
    )
  )
  

)


;;;this receives from rfi object
(om::defmethod! s-cuts2 ( (durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (make-voice-cuts durations meter tatum pitches tempo mode)))




;;;==================================
;;; S-POLY
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun make-voice (meter durations-list tatum pitches tempo)
  (let ((tatum-durs (om::om* durations-list tatum) ))
    (make-instance 'voice 
                   :tree (mktree (flat tatum-durs) meter)
                   :chords pitches 
                   :tempo tempo 
                   )
    )
  )

(defun make-voice-and-rests (meter durations-list tatum pitches tempo)
  (let ((rhythm-list (prep-rests durations-list tatum)))
    (make-instance 'voice 
                   :tree (mktree (remove 0 (flat rhythm-list)) meter)
                   :chords pitches 
                   :tempo tempo 
                   ))
)

(defun prep-rests (durations-list tatum)
  (let ((hold-durs (om::om* durations-list tatum)))
    (mapcar (lambda (x) (flat (list tatum (om::om* (om- x tatum) -1))))  hold-durs))
)

(defun sublist-pitch-processing (rhythms pitches)
  (let ((final-list '())
      (current-list '())
      (absolute-final-list '()))

    (loop for voice-rhythm in rhythms for voice-pitch in pitches do
      (setq current-list '())
      
      (cond ((listp (first voice-rhythm))
      (loop for sub-rhythm in voice-rhythm for sub-voice-pitch in voice-pitch do
            (push (first-n (flat (repeat-n sub-voice-pitch (length sub-rhythm)) 1) (length sub-rhythm)) current-list)))
      (t (push voice-pitch current-list)))

      (push  (reverse current-list)  final-list))

    (setq absolute-final-list (reverse  (mapcar (lambda (x) (flat x 1)) final-list)))
  
    absolute-final-list)
)


  
;;;this receives list of durations lists (output from get-rotations)
(om::defmethod! s-poly ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))

  :icon 1
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 110 0)
  :doc "Creates scores from input (for use with a static tatum)."

  ;;;create pitches for sublist versions, modes 2 & 3
  (let ((sublist-pitches  (sublist-pitch-processing durations-list pitches)))

  (case mode

    (0 (mapcar (lambda (x y) (make-voice meter x tatum y tempo)) durations-list pitches )
       )
    (1 (mapcar (lambda (x y) (make-voice-and-rests meter x tatum y tempo)) durations-list pitches)
       )
    (2 (mapcar (lambda (x y) (make-voice meter x tatum y tempo)) durations-list sublist-pitches)
       )
    (3 (mapcar (lambda (x y) (make-voice-and-rests meter (flat x) tatum y tempo)) durations-list sublist-pitches)
       )

    )

  )
)


;;;this receives from rfi object
(om::defmethod! s-poly ( (durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (s-poly durations meter tatum pitches tempo mode)))


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
  :icon 1
  :indoc '("A poly object--make sure that it is locked." "mode:0 = notes truncated on following attacks; mode: 1 = notes sustained for original durations.")
  :outdoc '("A voice object") 
  :initvals '('(nil) 0)
  :doc "Score conversion from poly to voice. Mode 0 truncates note durations on the following attack.  Mode 1 sustains notes through attacks using the original durations. In OM6.10.1 mode 1 requires a special patch to remedy a bug"

  (case mode
    (0  (let ((my-chordseq ( poly->chordseq mypoly))
          (my-tempo-map (get-tempomap (nth 0 (om::voices mypoly)))))

      (cseq+tempo->voice my-chordseq my-tempo-map))
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

    (flat (repeat-n  pitches (ceiling this-duration-sum (length pitches))) 1))
)

(defun prep-pitches-pulse (pitches durations-list)
 
  (let* ((abs-durations (mapcar #'abs durations-list))
         (this-duration-sum (reduce #'+ (flat abs-durations)))
         (these-pitches (flat (repeat-n  pitches (ceiling this-duration-sum (length pitches))) 1)))
      (flat (mapcar (lambda (x y) (repeat-n x y)) these-pitches (flat abs-durations)) 1)) 
)

(defun prep-pitches-rest (preped-pitches durations-list)
 
  (let* ((abs-durations (mapcar #'abs durations-list))
        (attacks (butlast (dx->x 0 (flat abs-durations)))))
  
    (mapcar (lambda (x) (nth x preped-pitches)) attacks))   
)

(defun prep-group-rhythms (durations-list prepared-tatums)
 
  (let* ((abs-durations (mapcar (lambda (x) (abs x)) durations-list))
        (this-duration-sum (reduce #'+  (flat abs-durations))))
    (ceiling this-duration-sum (length (flat prepared-tatums))))
)

(defun prep-rhythms-rest (grouped-list durations-list)
 
  (let* ((abs-rhythms (mapcar #'abs grouped-list))
        (abs-durations-list (mapcar #'abs durations-list))
        (neg-rhythms (om* abs-rhythms -1))
        (flat-durations-list (butlast(dx->x 0 (flat abs-durations-list))))
        (rhythm-values (mapcar (lambda (x) (nth x  (flat neg-rhythms))) flat-durations-list))
        (multiplied-rhythm-values (om* rhythm-values -1)))
        (subs-posn (flat neg-rhythms) flat-durations-list multiplied-rhythm-values))
)

(defun return-pos-neg (elements-list durations-list)
  (let ((hold-list '()))
  (loop for elem in elements-list
        for duration in durations-list 
        do
        (if (> duration 0) (push elem hold-list) (push (om::om* -1 elem) hold-list))
        )

  (reverse hold-list))

)


(defun group-list-rhythms (prepared-tatums durations-list)
  (let* ((list-to-group  (flat (repeat-n (flat prepared-tatums)  (prep-group-rhythms durations-list prepared-tatums))))
         (abs-durations (mapcar #'abs durations-list))
        (segmentation (flat durations-list))
        (abs-segmentation (flat abs-durations))
        (grouped-list  (om::group-list list-to-group abs-segmentation ::linear ))
        (grouped-list-with-rests (return-pos-neg grouped-list durations-list)))
        
   grouped-list-with-rests
 
 )
)



(defun group-list-rhythms-pulse (prepared-tatums durations-list)
  (let* ( (list-to-group (flat (repeat-n (flat prepared-tatums) (prep-group-rhythms durations-list prepared-tatums))))
         (abs-duration-list (mapcar #'abs durations-list))
        (add-input (reduce #'+ (flat abs-duration-list))))
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
           (grouped-list (group-list-rhythms prepared-tatums durations-list))
           (preped-pitches (prep-pitches new-pitches durations-list))
           (final-rhythms (mapcar (lambda (x) (reduce #'+ x))  grouped-list)))
  
    (make-instance 'voice 
                   :tree (mktree final-rhythms meter)
                   :chords preped-pitches 
                   :tempo tempo 
                   ))
)

(defun last-pulsed-rhythms (grouped-list durations-list)
  (let* ((abs-durations-list (mapcar #'abs durations-list))
        (abs-dx (dx->x 0 abs-durations-list))
        (abs-dur-subseq (mapcar (lambda (x y) (subseq grouped-list x y))  abs-dx (cdr abs-dx)))
        (last-pulsed-rhythms '()))

    (loop for duration in durations-list
          for list-subseq in abs-dur-subseq do
          (if (< duration 0) 
              (push (om::om* -1 list-subseq) last-pulsed-rhythms) 

              (push list-subseq last-pulsed-rhythms))
          )
    (flat (reverse last-pulsed-rhythms))
    )
)

(defun make-voice-pulse2 (meter durations-list tatum new-pitches tempo)
  (let* ((prepared-tatums (prep-tatums tatum durations-list))
           (grouped-list (group-list-rhythms-pulse prepared-tatums durations-list))
           (preped-pitches (prep-pitches-pulse new-pitches durations-list))
           (final-rhythms (last-pulsed-rhythms grouped-list durations-list))) 

    (make-instance 'voice 
                   :tree (mktree final-rhythms meter)
                   :chords preped-pitches 
                   :tempo tempo 
                   ))
)

(defun make-voice-and-rests2 (meter durations-list tatum new-pitches tempo)
  (let* ((prepared-tatums (prep-tatums tatum durations-list))
           (grouped-list (group-list-rhythms-pulse prepared-tatums durations-list))
           (preped-pitches (prep-pitches-pulse new-pitches durations-list))
           (final-pitches (prep-pitches-rest preped-pitches durations-list))
           (final-rhythms-pulse (prep-rhythms-rest grouped-list durations-list))
           (last-final-rhythms (last-rests durations-list final-rhythms-pulse))
           )
    (make-instance 'voice 
                   :tree (mktree last-final-rhythms meter)
                   :chords final-pitches 
                   :tempo tempo 
                   )
    )
 )

(defun last-rests (durations-list final-rhythms-pulse)
  ;get those last rests in if you use negative numbers in your durations-list
  (let* ((last-final-rhythms final-rhythms-pulse)
        (abs-durations-list (mapcar #'abs durations-list))
        (negs (dx->x 0 abs-durations-list)))
    (loop for duration in durations-list 
          for neg in (om::butlast negs) do
          (if (<  duration 0) 
            (setf (nth neg last-final-rhythms) (- (nth neg final-rhythms-pulse)))
            ))

    last-final-rhythms)
)

(defun prep-rests (durations-list tatum)
  (let ((hold-durs (om* durations-list tatum)))
    (mapcar (lambda (x) (flat (list tatum (om* (om- x tatum) -1))))  hold-durs)
    )
)

(defun prep-meter (meter)
  (if  (equal (second meter) nil)
      (flat (car meter))
    meter)
)




;;;THIS IS WHERE rotations->poly2 was added.  This may be old/unused.

;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! rotations->poly2 ( (durations-list list) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))

  :icon 1
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
(om::defmethod! rotations->poly2 ((durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (flat-voices durations-list))))

    (s-poly2 durations meter tatum pitches tempo mode))

)


(defun flat-by-voice (mylist)
  (let ((final-list '()))
    (loop for elem in mylist do
     (push (flat elem) final-list))
    (reverse final-list)
    )
)

  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-poly2 ( (durations-list list) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))

  :icon 1
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer" "mode: 0 = sustain mode output; 1 = rests mode output")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) (((1(16)) (2(20)))) ((6100)) 110 0)
  :doc "Creates scores from input (for use with a changing tatum)."
  
  (let (( preped-meter (prep-meter meter))
        (my-durations-list (flat-by-voice durations-list))
        (sublist-pitches  (sublist-pitch-processing durations-list pitches)))
  (case mode

    (0 (mapcar (lambda (x y z) (make-voice2 preped-meter x y z tempo)) my-durations-list tatum pitches)
       )
    (1 (mapcar (lambda (x y z) (make-voice-and-rests2 preped-meter x y z tempo)) my-durations-list tatum pitches)
       )
    (2 (mapcar (lambda (x y z) (make-voice-pulse2 preped-meter x y z tempo)) my-durations-list tatum pitches)
       )
    ;;sustain sublist version
    (3 (mapcar (lambda (x y z) (make-voice2 preped-meter x y z tempo)) my-durations-list tatum sublist-pitches)
       )
    ;;rests sublist version
    (4 (mapcar (lambda (x y z) (make-voice-and-rests2 preped-meter x y z tempo)) my-durations-list tatum sublist-pitches)
       )
    ;;pulse sublist version
    (5 (mapcar (lambda (x y z) (make-voice-pulse2 preped-meter x y z tempo)) my-durations-list tatum sublist-pitches)
       )
    )
  )
  

)


;;;this receives from rfi object
(om::defmethod! s-poly2 ((durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let (
        (durations (mapcar 'pulses (voices durations-list))))

    (s-poly2 durations meter tatum pitches tempo mode)
    )

)

(om::defmethod! s-poly ( (durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (s-poly durations meter tatum pitches tempo mode))
)





;;;==================================
;;; S-VOICE
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun make-voice (meter durations-list tatum pitches tempo)
  (let ((tatum-durs (om::om*  durations-list tatum) ))
    (make-instance 'voice 
                   :tree (mktree (flat tatum-durs) meter)
                   :chords pitches 
                   :tempo tempo 
                   ))
)


(defun make-poly (myvoices)
    (make-instance 'poly
                   :voices myvoices)
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

  
;;;making the sustain version first
(om::defmethod! s-voice ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer))

  :icon 1
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







