;;;==================================
;;; ROTATIONS->VOICE
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
(om::defmethod! rotations->voice ( (durations-list list) (meter list) (tatum number) (pitches list) (tempo integer))

  :icon 2
  :indoc '("a list for the meter" "a list of lists for durations" "a tatum specified as a fraction" "a list of lists of pitches" "a tempo as integer")
  :outdoc '("a voice") 
  :initvals '( ((1 5 7 10)) (4 4) 1/16 ((6100)) 100)
  :doc "Converts a rotation list into music notation."

  (let ((my-chordseq (make-chord-seq (make-poly (print (mapcar (lambda (x y) (make-voice meter x tatum y tempo)) durations-list pitches )))))
        (my-tempo-map (get-tempo-map (make-voice meter (first durations-list) tatum (first pitches) tempo))))
    
    (reduce-to-one-voice my-chordseq my-tempo-map)

    )
)


;;;making the sustain version first
(om::defmethod! rotations->voice ( (durations-list prf) (meter list) (tatum number) (pitches list) (tempo integer))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (rotations->voice durations meter tatum pitches tempo)))

;;;make mode 1 2 3 for sustain, rest, pulses
;;;