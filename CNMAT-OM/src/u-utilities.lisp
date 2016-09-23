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

(om::defmethod! u-score-lists ( (my-thing voice) &optional (this-tatum 1/16))

  :icon 7
  :indoc '("a poly or a voice" "an optional tatum" )
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice" "and another thing") 
  :initvals '(nil)
  :numouts 5
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

  (let ((rhythms '())
        (pitches '())
        (meters '())
        (durations '())
        (output '())
        )

   ; get the rhythms
   (push (tree2ratio (om::tree my-thing)) rhythms)

   ; get the durations
   (push (om/ (tree2ratio (om::tree my-thing)) this-tatum) durations)

   ;get pitch
   (push (info-get-pitches  (make-instance 'chord-seq
                                           :lmidic (om::chords my-thing))) pitches)

   (push (om::get-signatures (om::tree my-thing)) meters)

   ;send out everything plus a last element that is the number of rhythm elements
   (push (list (flat pitches 1) (flat rhythms 1) (list (length (flat rhythms 1)))) output)

   ;(flat output 1)
   (values meters pitches rhythms durations (list (list (length (flat rhythms 1)))))


   )
)


(om::defmethod! u-score-lists ( (my-thing poly)  &optional (this-tatum 1/16) )

  :icon 7
  :indoc '("a poly or a voice" "an optional tatum" )
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice") 
  :initvals '(nil)
  :numouts 5
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

 ; (let* ((stuff (mapcar (lambda (x) (u-info x)) (om::voices my-thing)))
 ;      (organized (mat-trans stuff)))
        
    ;(print 'stuff)
    ;(print stuff)

       ;(values (first organized) organized (third organized))

  (let* ((voices (om::voices my-thing))
         (rhythms '())
         (durations '())
         (pitches '())
         (meters '())
         (num-elems '())
         (output '())
         )
    
    (print 'voices)
    (print voices)

   ;get rhythms
   (loop for voice in voices do
         (push (tree2ratio (om::tree voice)) rhythms))
   
   (loop for voice in voices do
         (push (om/ (tree2ratio (om::tree voice)) this-tatum) durations))

   ;get pitch
   (loop for voice in voices do
         (push (info-get-pitches  (make-instance 'chord-seq
                                           :lmidic (om::chords voice))) pitches))

   (loop for voice in voices do
         (push (om::get-signatures (om::tree voice)) meters))

   ;send out everything plus a last element that is the number of rhythm elements
   (loop for rhythm in rhythms do
         (push (list (length (flat rhythm 1))) num-elems))

   ;(flat output 1)
   (values (reverse meters) (reverse pitches)  (reverse rhythms) (reverse durations) (reverse num-elems))


   )
)


