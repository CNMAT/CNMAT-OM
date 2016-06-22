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



(om::defmethod! poly->voice ((mypoly poly))
  :icon 2
  :indoc '("A poly object--make sure that it is locked.")
  :outdoc '("A voice object") 
  :initvals '(nil)
  :doc "Does a simple conversion from a poly to a voice.  Be sure to lock the input poly object"
  (let ((my-chordseq ( poly->chordseq mypoly))
        (my-tempo-map (get-tempomap (nth 0 (om::voices mypoly)))))

    (cseq+tempo->voice my-chordseq my-tempo-map)
    
    )

)
  


