;;;==================================
;;; U-UTILITIES
;;;==================================

(in-package :cnmat)

(defun info-get-pitches (chord-seq)

  (let ((final-pitches '())
        (mypitches (om::lmidic chord-seq)))

     (loop for pitch in mypitches do
      (cond 
           ((< (length pitch) 2) (push  pitch final-pitches))
           (t (push (list pitch) final-pitches))))

     (reverse (flat final-pitches 1))
     )
  )

(om::defmethod! u-info ( (my-thing voice) )

  :icon 7
  :indoc '("a poly or a voice" )
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice") 
  :initvals '(nil)
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

  (let ((rhythms '())
        (pitches '())
        (output '())
        )

   ; get the rhythms
   (push (tree2ratio (om::tree my-thing)) rhythms)

   ;get pitch
   (push (info-get-pitches  (make-instance 'chord-seq
                                           :lmidic (om::chords my-thing))) pitches)

   ;send out everything plus a last element that is the number of rhythm elements
   (push (list (flat pitches 1) (flat rhythms 1) (list (length (flat rhythms 1)))) output)

   (flat output 1)

   )
)


(om::defmethod! u-info ( (my-thing poly) )

  :icon 7
  :indoc '("a poly or a voice" )
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice") 
  :initvals '(nil)
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

  (let* ((stuff (mapcar (lambda (x) (u-info x)) (om::voices my-thing)))
        (organized (mat-trans stuff)))
        
        organized
   )
)


