;;;==================================
;;; R-RHYTHM
;;;==================================


(in-package :cnmat)


(defun sort-by-first-elem (elem)
    (car (car elem))
)



(om::defmethod! rand-from-list ((mylist list) (num-return-vals number))

  :icon 7
  :indoc '("a list with or without random weights" "desired length of output sequence")
  :outdoc '("a sequence of random elements of n-length from a list") 
  :initvals '((7100 (7200 0.9) 7300 7400 7800) 20)
  :doc "Returns a sequence of random elements of n-length from a list. Provide probability weights as desired for any element in the list.Format as follows (element probability) where the sum of the probabilities is < 1.0. Any elements without designated p robabilities will be assigned equal probabilities from the remainder, adding to 1.0"
  
  (let* ((built-weights (build-random-weights mylist))
         (output-list '())
         (final-list '()))
 
      (loop for x from 1 to num-return-vals do
         ( push (build-sequence built-weights) output-list))
   
      (reverse output-list)

      (loop for elem in output-list do
         (if (eq 'nil (cdr elem)) (push (car elem) final-list) (push  elem final-list) ))

      (list (reverse final-list)))

)





;;;==================================
;;; R-SCATTER-ATTACKS
;;;==================================


(defun get-random-voices (rhythmlist numvoices)

  (let ((final-list '())
        (current-voice (random numvoices))
        (prev-voice nil))

    ;;;make sure it is not the same voice as the last time
    
    (loop for i from 0 to (length rhythmlist) do
          (setf current-voice (random numvoices))
          (loop while (eq current-voice prev-voice) do
                  (setf current-voice (random numvoices)))
          (setf prev-voice current-voice)
          (push current-voice final-list))
  
    final-list)

)



;;randomly scatter the attacks of a rhythmic sequence over n-voices

(om::defmethod! r-scatter-attacks ((rhythmlist list) (numvoices number))

  :icon 6
  :indoc '("a list of lists" )
  :outdoc '("returns rhythm lists combined by voice") 
  :initvals '((1/16 1/16 1/8 1/16 1/16 1/16 1/8) (nil))
  :doc "Scatters the attacks of a rhythm sequence randomly across a specified number of voices."

  (let* ((final-list '())
        (random-voice 0)
        (sequence (get-random-voices rhythmlist numvoices)))

  ;create an empty list for every voice
  (loop for i from 0 to (- numvoices 1) do
        (push '() final-list))

  ;assign either a rest or an attack for every element in rhythmlist
  (loop for attack in rhythmlist
        for j from 0 to (length rhythmlist) do
        (setf random-voice (nth j sequence))
        ;(print random-voice)
        (loop for i from 0 to (- numvoices 1) do
              (if (eq i random-voice) 
                  (push attack (nth i final-list))
                  (push (* -1 attack) (nth i final-list)))))

  (mapcar #'reverse final-list))
 
)



;;;==================================
;;; R-COMBINE-LISTS
;;;==================================
;;;R-combine-lists is deprecated

(in-package :cnmat)

(defun rhythm-append (lista listb)
  (flat (list lista listb))
)


(om::defmethod! r-combine-lists ((my-list list) &rest rest-list)

  :icon 6
  :indoc '("a list of lists" )
  :outdoc '("returns rhythm lists combined by voice") 
  :initvals '(((1 2 3 4) (5 6 7 8)) (nil))
  :doc "This object appends rhythm lists across an arbitrary number of voices and converts them into one (long) list of lists by voice. Add as many inputs to the combine-rhythm-lists object as desired for lists. (Use shift-alt right-arrow to add inputs and shift-alt left-arrow to subtract inputs). Requires formatting rhythms by voice as lists of lists. Each list of lists must be the same length (i.e. same number of voices)."

  (let ((big-list (mat-trans   (x-append (list my-list) rest-list))))
    (mapcar (lambda (x) (reduce 'rhythm-append x)) big-list)
  ) 

)


;;;==================================
;;; R-DIMINUTIONS
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(defun substitute-processing (mylist)

   (let ((final-list '())
      (collection '()))

  ;;;we just want the substitute vales to appear in the final lists
  ;;;if it is a list then just add it to final-list
  ;;;otherwise put it in collection and output it
  ;;;as a rest when we need it
  
     (loop for elem in mylist do
      (if (listp elem)
          (progn (push (reduce #'+ collection) final-list)
                 (setq collection '())
                 (push (reverse elem) final-list))
        
          (push (* -1 elem) collection)))

  ;;;get any leftover elements
  (push collection final-list)

  ;;;output the list in the right order
      (remove 0 (remove 'nil (reverse (flat final-list))))
 )
)


(defun local-substitute (subs val mylist)
  (let ((final-list mylist ))
    (loop for mysub in subs 
          for value in val do
          (setq final-list (substitute mysub value final-list)))
  
    final-list)
)


(defmethod! r-duration-list ((myvoices list)) 
:doc "Returns the durations of all rhythmic frame voices. Resulting lists may be."
:icon 5
    (mapcar #'(lambda (r)  (cnmat::pulses r))  myvoices)
)





(defmethod! r-diminutions ((rhythm rhythmic-frame) val subs ) 

       (let ((substitutions (local-substitute subs val (pulses rhythm))))
         (make-instance
          'rhythmic-frame
          :pulses  (substitute-processing substitutions)))
)


(defmethod! r-diminutions ((rhythm polyrhythmic-frame) val subs) 
   
  ;;;If the first element of the subs is 0 then you know it is the special case
  ;;;where the subs need to be sent in separately for each voice.
  ;;;In this case, remove the voice designatory, e.g. "0", before sending it on

  :doc "Replaces a rhythmic value with subset values (diminutions) that sum to the original value."
  :icon 5

           (if (eq 0 (car (first subs)))

               (make-instance 
                'prf 
                :voices (mapcar #'(lambda (r s) (r-diminutions r val (cdr s))) (voices rhythm) subs)
                )
           ;;;Otherwise, you send the same subs list in for all the voices
            (make-instance 
            'prf 
            :voices (mapcar #'(lambda (r) (r-diminutions r val subs)) (voices rhythm))
             )
            )
 
)



;;;==================================
;;; RHYTHMIC-FRAME-INTERLEAVE
;;;==================================

(in-package :cnmat)


(defmethod r-interleave (rhythm-parent   &rest rest )
  :icon 3
  :initvals '(nil) 
  :indoc '("first element" "second element" "additional elements")
  :doc "Interleaves a parent and a child polyrhythmic frame.."
  (let* ((my-stuff (mapcar (lambda (x) (list x)) rest))
        (all-stuff (flat (mat-trans (cons rhythm-parent rest)))))

  ;this print call below is necessary--do not delete it!
  (print all-stuff)
  )
)




(defmethod r-interleave2 (rhythm-parent &rest rest)
  :initvals '(nil) 
  :indoc '("first element" "second element" "additional elements")
  :icon 1
  :doc "Interleaves rhythmic frames."
  (loop for group in (mat-trans (cons rhythm-parent rest))
        collect (make-instance 'prf :voices group))
  )



;;;==================================
;;; R-RETRO-CANON?
;;;==================================

(in-package :cnmat)

;;;return the retrograde of the list

(defun get-retrograde (my-list)
  (let ((final-list '(nil)))
       (loop for elem in my-list do
             (push elem final-list)
        )
    (remove nil final-list)
    )
)

  
(om::defmethod! r-retro-canon? ((my-list list))
  :icon 7
  :indoc '("Tests canon-query on list combined with its retrograde")
  :outdoc '("If true, returns list. If false, returns nil") 
  :initvals '((1 2 3 4 5))
  :doc "Tests canon-query on a list combined with its retrograde. If true, returns combined list. If false, returns nil."

  (let* ((retrograde-list (get-retrograde my-list))
         (combo-list (list my-list retrograde-list )))
    
    ;;now test the canon query on the list and its retrograde
        (if (eq (cnmat::q-canon combo-list) 't) 
            combo-list
            'nil))
)
