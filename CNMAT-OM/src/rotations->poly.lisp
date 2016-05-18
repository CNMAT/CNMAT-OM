;;;==================================
;;; ROTATIONS->POLY
;;;==================================

(in-package :cnmat)

;;;player for the rotations of a rhythm

(defun make-voice (meter durations-list tatum pitches tempo)
  (let ((tatum-durs (print (om* (print durations-list) tatum) )))
    (make-instance 'voice 
                   :tree (mktree (flat tatum-durs) meter)
                   :chords pitches 
                   :tempo tempo 
                   )
    ;;tatum-durs
    )
  )

  
;;;making the sustain version first
(om::defmethod! rotations->poly ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer")
  :outdoc '("a poly") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 110)
  :doc "Converts a rotation list into music notation."

  (mapcar (lambda (x y) (make-voice meter x tatum y tempo)) durations-list pitches )

)


;;;making the sustain version first
(om::defmethod! rotations->poly ( (durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (rotations->poly durations meter tatum pitches tempo)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;