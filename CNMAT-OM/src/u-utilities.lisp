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

(defun info-get-rhythms-and-tatums (rhythms)

 (let* ((output-rhythms '())
       (abs-beat '())
       (pre-output-tatums '())
       (output-tatums '()))

;get down to the internal beat subdivisions and list them as tatums

;takes us down to the list of measures
       (loop for group in (flat (rest rhythms) 1) do
             (loop for subdivision in (rest group) do
                   ;takes us down to each individual measure
                   (loop for beat in subdivision do
                         ;if it is a full beat grab the rhythms
                         (cond ((listp beat) (push (reverse (flat (cdr beat) 1)) output-rhythms))
                               ;otherwise, grab the rest and put it in list, too
                               (t (push (list beat) output-rhythms)))
                         ;put a sum into abs-beat for the absolute value duration number of subdivisions in a beat
                         (cond ((listp beat) (progn (push (mapcar #'abs (flat (cdr beat) 1)) abs-beat)
                                                   ;put the abs total for the beat into output tatums)
                                                    (push (reduce #'+  (flat abs-beat)) pre-output-tatums)))
                               ;it's a rest, so push 1 thing in
                               ;(t (push (abs beat) pre-output-tatums)))
                               (t (push  (repeat-n 1 (abs beat)) pre-output-tatums)))
                         ;reset abs-beat
                         (setf abs-beat '())
                         )))

       ;(print 'pre-output-tatums)
       ;(print pre-output-tatums)
       ;now convert the pre-output tatums list into a correct tatums list for output
       (loop for elem in (flat pre-output-tatums) do
             (cond ((= elem 1) (push '(1 (4)) output-tatums))  ;quarters
                   ((= elem 2) (push '(1 (8)) output-tatums))  ;eighths
                   ((= elem 3) (push '(1 (12)) output-tatums)) ;triplets
                   ((= elem 4) (push '(1 (16)) output-tatums)) ;sixteenths
                   ((= elem 5) (push '(1 (20)) output-tatums)) ;quintuplets
                   ((= elem 6) (push '(1 (24)) output-tatums)) ;sextuplets
                   ((= elem 6) (push '(1 (28)) output-tatums)) ;septuplets
                   ((= elem 8) (push '(1 (32)) output-tatums))
                   (t nil))) ;thirty-secods

       ;output the tatums
       (list (reverse (flat output-rhythms))  output-tatums)
  )

)

(om::defmethod! u-score-lists ( (my-thing voice))

  :icon 7
  :indoc '("a poly or a voice")
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice" "and another thing") 
  :initvals '(nil)
  :numouts 5
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

  (let ((rhythms '())
        (pitches '())
        (meters '())
        (tatums '())
        (output '())
        )

   ; get the rhythms & tatums
   ;(push (tree2ratio (om::tree my-thing)) rhythms)
   (push (first (info-get-rhythms-and-tatums (om::tree my-thing))) rhythms)
   (push (second (info-get-rhythms-and-tatums (om::tree my-thing))) tatums)

   ; get the durations
   ;(push (om/ (tree2ratio (om::tree my-thing)) this-tatum) durations)

   ;get pitch
   (push (info-get-pitches  (make-instance 'chord-seq
                                           :lmidic (om::chords my-thing))) pitches)

   (push (om::get-signatures (om::tree my-thing)) meters)

   ;send out everything plus a last element that is the number of rhythm elements
   (push (list (flat pitches 1) (flat rhythms 1) (list (length (flat rhythms 1)))) output)

   ;(flat output 1)
   (values (flat meters 1) pitches rhythms tatums (list (list (length (flat rhythms 1)))))


   )
)


(om::defmethod! u-score-lists ( (my-thing poly)  )

  :icon 7
  :indoc '("a poly or a voice" )
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
         (tatums '())
         (pitches '())
         (meters '())
         (num-elems '())
         (output '())
         )
    
    (print 'voices)
    (print voices)

   ;get rhythms
   (loop for voice in voices do
         (push (first (info-get-rhythms-and-tatums (om::tree voice))) rhythms)
         (push (second (info-get-rhythms-and-tatums (om::tree voice))) tatums))

(print 'rhythms)
(print rhythms)
   
   ;(loop for voice in voices do
   ;      (push (om/ (tree2ratio (om::tree voice)) this-tatum) durations))

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
   (values (reverse (flat meters 1)) (reverse pitches)  (reverse rhythms) (reverse tatums) (reverse num-elems))


   )
)


