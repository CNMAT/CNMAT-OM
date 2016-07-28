;;;==================================
;;; S-POLY
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
    )
  )

(defun make-voice-and-rests (meter durations-list tatum pitches tempo)
  (let ((rhythm-list (print (prep-rests  durations-list tatum) )))
    (make-instance 'voice 
                   :tree (mktree (remove 0 (flat rhythm-list)) meter)
                   :chords pitches 
                   :tempo tempo 
                   )
    )
  )

(defun prep-rests (durations-list tatum)
  (let ((hold-durs (om* durations-list tatum)))
    (mapcar (lambda (x) (flat (list tatum (om* (om- x tatum) -1))))  hold-durs)
    )
)



  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! s-poly ((durations-list list) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))

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
(om::defmethod! s-poly ((durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (s-poly durations meter tatum pitches tempo mode)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;