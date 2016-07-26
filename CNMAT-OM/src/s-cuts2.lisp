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

    ;;(generated-rhythms-with-rests (flat (mapcar (lambda (x) (cuts-rests x tatum)) generated-rhythms)))
    ;;return a list with both voices
    ;; (print generated-tatums)
    ;;(print generated-rhythms)
    ;;this gets list of attacks 
    ;; (print attack-durations)
    ;;(print rest-durations) 
    ;;(print attacks+rests-durations)
    ;;(print (flat (mapcar (lambda (x y) (list x y)) attack-durations rest-durations)))


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

    (print flat-rhythm-lists)
    (print flat-pitch-lists)
    (print prepped-pitches)
    (print generated-tatums)
    (print generated-rhythms)

    
  (case mode

    (0 (flat (mapcar (lambda (x y z a) (make-voice-cuts2 meter x tatum-list y tempo z a)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums) 1)
       )
    (1 (flat (mapcar (lambda (x y z a) (make-voice-cuts-rests2 meter x tatum-list y tempo z a)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums) 1)
       )

    (2 (flat (mapcar (lambda (x y z a) (make-voice-cuts-pulse2 meter x tatum-list y tempo z a)) flat-rhythm-lists  prepped-pitches generated-rhythms generated-tatums) 1)
       )
       

   ;; (2 (print pitches)
      ;; )
    )
  )
  

)


;;;this receives from rfi object
(om::defmethod! s-cuts2 ( (durations-list prf) (meter list) (tatum list) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (make-voice-cuts durations meter tatum pitches tempo mode)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;