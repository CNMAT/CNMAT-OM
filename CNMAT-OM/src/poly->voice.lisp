;;;==================================
;;; POLY->VOICE
;;;==================================

;;;simple conversion from poly to voice


(in-package :cnmat)



(defun poly->chordseq (mypoly)
;;turn a poly object into a chordseq before turning the chordseq into a voice
  (let ((new-chord-seq (make-instance 'chord-seq)))
   (om::objFromObjs mypoly new-chord-seq))

)



(om::defmethod! poly->voice ((mypoly poly) &optional (mode 0) )
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
  


