;;;==================================
;;; R-RHYTHM
;;;==================================




;;;==================================
;;; R-SCATTER-ATTACKS
;;;==================================

;;randomly scatter the attacks of a rhythmic sequence over n-voices

(om::defmethod! r-scatter-attacks ((rhythmlist list) (numvoices number))

  :icon 6
  :indoc '("a list of lists" )
  :outdoc '("returns rhythm lists combined by voice") 
  :initvals '((1/16 1/16 1/8 1/16 1/16 1/16 1/8) (nil))
  :doc "Scatter the attacks of a rhythmic sequence randomly over n-voices"

  (let* ((final-list '())
        (random-voice 0))

  ;create an empty list for every voice
  (loop for i from 0 to (- numvoices 1) do
        (push '() final-list))

  ;assign either a rest or an attack for every element in rhythmlist

  (loop for attack in rhythmlist do
        (setf random-voice (random numvoices))
        (print random-voice)
        (loop for i from 0 to (- numvoices 1) do
              (if (eq i random-voice) 
                  (push attack (nth i final-list))
                  (push (* -1 attack) (nth i final-list)))))

  (mapcar 'reverse final-list)

  )
 
)

;;;==================================
;;; R-COMBINE-RHYTHM-LISTS
;;;==================================

(in-package :cnmat)

(defun rhythm-append (lista listb)
  
  ;;;or simply use x-append here

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

  ;we just want the substitute vales to appear in the final lists
  ;if it is a list then just add it to final-list
  ;otherwise put it in collection and aoutput it
  ; as a rest when we need it
  
  (loop for elem in mylist do
      (if (listp elem)
          (progn (push (reduce #'+ collection) final-list)
                 (setq collection '())
                 (push (reverse elem) final-list))
        
          (push (* -1 elem) collection)))

  ;get any leftover elements
  (push collection final-list)

  ;output the list in the right order
   (remove 0 (remove 'nil (reverse (flat final-list))))

 )

)




(defmethod! r-diminutions ((rhythm rhythmic-frame) val subs) 
  (let ((substitutions (substitute subs val (pulses rhythm))))

   'rhythmic-frame
   :pulses  (substitute-processing substitutions))
)
  


(defmethod! r-diminutions ((rhythm polyrhythmic-frame) val subs) 
   (make-instance 
    'prf 
    :voices (mapcar #'(lambda (r) (r-diminutions r val subs)) (voices rhythm))
    ))



;;;==================================
;;; RHYTHMIC-FRAME-INTERLEAVE
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function
(defmethod r-interleave (rhythm-parent   &rest rest )
  :initvals '(nil) 
  :indoc '("first element" "second element" "additional elements")
  :icon 1
  :doc "Interleaves rhythmic frames."
  (let* ((my-stuff (mapcar (lambda (x) (list x)) rest))
        (all-stuff (flat (mat-trans (cons rhythm-parent rest)))))
  
  ;(print 'all-stuff)
  ;this print call below is necessary--dont delete!
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
            'nil)
   )

)
