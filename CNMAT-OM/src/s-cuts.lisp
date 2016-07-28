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