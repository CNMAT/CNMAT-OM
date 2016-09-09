;;;==================================
;;; P-PITCH
;;;==================================


;;;==================================
;;; P-HARMONY-FROM-ANALYSIS
;;;==================================

(in-package :cnmat)


(om::defmethod! p-harmony-from-analysis (( mychordseq chord-seq) &optional (numpitches 0))

  :icon 2
  :indoc '("a chor-seq" "an optional argument specifying the numnber of pitches to return based on highest partial amplitude")
  :outdoc '("lists") 
  :initvals '( '(nil) 0)
  :doc "Returns a chord object notating partials from the analysis.  Option argument returns n-pitches from the partials based on highest amplitudes."
 (let* ((pitches  (flat (om::lmidic mychordseq)))
            (velocities  (flat (om::lvel mychordseq)))
            (pairs (mapcar (lambda (x y) (list x y)) pitches velocities))
            (sorted-list  (sort pairs #'> :key #'second))
            (final-pairs '()))

   (if (eq numpitches 0)
    
        ;gather all pitches and velocities and make a big harmonic field


       (make-instance 'chord
                      :lmidic pitches
                      :lvel velocities)
   

   ; otherwise, return n-pitches based on velocities
  

          (progn (loop for i from 0 to (- numpitches 1) do
                (push (nth i sorted-list) final-pairs))
          
            (make-instance 'chord
                      :lmidic (mapcar (lambda (x) (first x)) final-pairs)
                      :lvel (mapcar (lambda (x) (second x)) final-pairs)))
 
   )


)



)












;;;==================================
;;; P-BANDS-COLLECTION
;;;==================================

(in-package :cnmat)

(defun random-pitch-from-bands (lower-limit upper-limit)

;;return a random pitch from upper and lower bounds
;; add the random value returned to the lower of the two pitches 

(om+ (min lower-limit upper-limit) (om* (round (om/ (random (abs(- lower-limit upper-limit))) 100)) 100))

)


(defun scaled-bpf-samples (my-bpf attacks-list)

  (let* ((sum-of-attacks (reduce '+ attacks-list))
         (x-scaled-bpf (bpf-scale my-bpf :x1 0 :x2 sum-of-attacks))
         (attack-intervals (butlast (dx->x 0 attacks-list))))

    (mapcar (lambda (x) (x-transfer x-scaled-bpf x)) attack-intervals)

    )

)


(defun sample-from-bands (bpf-lib attacks-voice)

  (let 

    ;;for each bpf sample it for the number of attacks
    ;;this is where I need to change the code
    ;;if ever desired, revert to previous version by switching out the next two lines of code\

    ((bpf-samps (mapcar (lambda (x) (scaled-bpf-samples x attacks-voice)) bpf-lib)))

    (mapcar (lambda (x y) (random-pitch-from-bands x y)) (nth 0 bpf-samps) (nth 1 bpf-samps))
    )

)


(defun get-pitch-from-collection-by-voice (voice-pitches pitch-collection)

  (mapcar (lambda (x) (find-closest-pitch-from-collection x pitch-collection)) voice-pitches)

)

(defun find-closest-pitch-from-collection (pitch pitch-collection)

     (let* ((differences (mapcar (lambda (x y) (list (abs (- pitch x)) y)) pitch-collection pitch-collection))
      (smallest-difference (nth 0 differences)))

        (mapcar (lambda (x) (if (<= (nth 0 x) (nth 0 smallest-difference)) (setq smallest-difference x))) differences)

       (nth 1 smallest-difference)



     )


)

  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-bands-collection ( (bpf-lib list) (attacks-voices list) (pitch-collection list) &optional (mode 0))

  :icon 2
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" " a pitch collection from which to draw pitches" "mode: 0 = n/a")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil ((1 2 3 4)(2 3 4 5))  (6000 6600 7200 7500))
  :doc "Returns random pitch samples from a collection provided. The pitches sampled are determed following the contour of the bpf pitch bands provided "
  
  (case mode

    (0 (let ((prelim-sample-pitches (mapcar (lambda (x) (sample-from-bands bpf-lib x))  attacks-voices)))
         
         (mapcar (lambda (x) (get-pitch-from-collection-by-voice x (flat pitch-collection))) prelim-sample-pitches)
         )
       )

       )


)


;;;this receives from rfi object
(om::defmethod! p-map1 ( (durations-list prf) (mapping-list list) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (pitch-mapper1 durations mapping-list mode)))

;;;


;;;==================================
;;; P-BANDS-PITCHCLASS
;;;==================================

(in-package :cnmat)


(defun get-pitchclasses-helper (pitch-samples)

      (mapcar (lambda (x) (* (round (/ x 100)) 100)) pitch-samples)  

)

(defun get-pitchclasses-helper2 (rounded-results)

       (mapcar (lambda (x) (list (rem (/ x 100) 12) x)) rounded-results)

)

(defun get-pitchclasses (pitch-samples)
     
   (let ((rounded-results (mapcar (lambda (x) (get-pitchclasses-helper x)) pitch-samples)))

         (mapcar (lambda (x) (get-pitchclasses-helper2 x)) rounded-results)

         )

)


(defun get-allowable-pitches-helper (pitch-and-pitchclasses allowable-pitchclasses)

  (loop for elem in pitch-and-pitchclasses
           if (member (nth 0 elem) allowable-pitchclasses)
           return elem)

)


(defun check-pitches (allowable-pitchclasses pitch-samples)
  (let* ((pitch-and-pitchclasses (get-pitchclasses pitch-samples))
       (list-of-allowable-pitches (mapcar (lambda (x) (get-allowable-pitches-helper x allowable-pitchclasses)) pitch-and-pitchclasses)))

    (mapcar (lambda (x) (nth 1 x)) list-of-allowable-pitches)

  )

)

(defun get-sample-from-bpfs (lower-limit upper-limit)

;;rounding pitches here to nearest semitone
(om+ (min lower-limit upper-limit) (om* (round (om/ (random (abs(- lower-limit upper-limit))) 100)) 100))

)

(defun random-pitch-from-bands-pitchclass (lower-limit upper-limit)

;;return a random pitch from upper and lower bounds
;; add the random value returned to the lower of the two pitches 
;;return 100 choices for each pitch to try and get one that conforms to pitchclass requirements

;;not-used--(repeat-n (get-sample-from-bpfs lower-limit upper-limit) 100)

     (loop for i from 1 to 100
           collect (get-sample-from-bpfs lower-limit upper-limit))

)

(defun scaled-bpf-samples (my-bpf attacks-list)

  (let* ((sum-of-attacks (reduce '+ attacks-list))
         (x-scaled-bpf (bpf-scale my-bpf :x1 0 :x2 sum-of-attacks))
         (attack-intervals (butlast (dx->x 0 attacks-list))))

    (mapcar (lambda (x) (x-transfer x-scaled-bpf x)) attack-intervals)

    )

)


(defun sample-from-bands-pitchclass (bpf-lib attacks-voice)

;;for each bpf sample it for the number of attacks
  (let 
    ;;this is where I need to change the code--changed
    ;;if ever desired, revert to previous version by switching out the next two lines of code\

    ((bpf-samps (mapcar (lambda (x) (scaled-bpf-samples x attacks-voice)) bpf-lib)))

    (mapcar (lambda (x y) (random-pitch-from-bands-pitchclass x y)) (nth 0 bpf-samps) (nth 1 bpf-samps))
    
    )

)



  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-bands-pitchclass ( (bpf-lib list) (attacks-voices list) (allowable-pitchclasses list) &optional (mode 0))

  :icon 2
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" " a collection of allowable pitchclasses" "mode: 0 = n/a")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil ((1 2 3 4)(2 3 4 5))  (0 1 5))
  :doc "Returns random pitch samples that conforms to a list of allowable pitchclasses. The pitches sampled are determed following the contour of the bpf pitch bands provided "
  
  (case mode

    (0 (let* ((bpf-pitch-samples (mapcar (lambda (x) (sample-from-bands-pitchclass bpf-lib x))  attacks-voices))
              (checked-bpf-pitch-samples (mapcar (lambda (x) (check-pitches allowable-pitchclasses x)) bpf-pitch-samples)))
         
         checked-bpf-pitch-samples
         
         )
       )

      )


)


;;;==================================
;;; P-BANDS-REGISTER
;;;==================================

(in-package :cnmat)

(defun scaled-bpf-samples-registration (my-bpf pitches)

  
  (let ((hold-samples (om::bpf-sample my-bpf 'nil 'nil (length pitches))))
    ;round them
    (om::om* (om::om-round (om::om/ hold-samples 100.00)) 100)

    )

)


(defun scaled-bpf-samples (my-bpf attacks-list)

  (let* ((sum-of-attacks (reduce '+ attacks-list))
         (x-scaled-bpf (bpf-scale my-bpf :x1 0 :x2 sum-of-attacks))
         (attack-intervals (butlast (dx->x 0 attacks-list))))

    (mapcar (lambda (x) (x-transfer x-scaled-bpf x)) attack-intervals)

    )

)


(defun get-registrated-pitches-useless (pitch)
  ;make series below and above by octave to add to note
  ;this function could be more precise....
  (let* ((series (om::om* (arithm-ser 0 20 1) 1200))
         (hi-pitches (om::om+ series pitch))
         ;;dont include given pitch this time
         (low-pitches (om::om- series (- pitch 1200))
         (all-pitches (append hi-pitches low-pitches)))

  all-pitches
  ))
)

(defun get-registrated-pitches (pitch)
  ;make series below and above by octave to add to note
  ;this function could be more precise....
  (let* (
         (hi-pitches (arithm-ser pitch 12800 1200))
         (low-pitches (arithm-ser (- pitch 1200) 100 -1200))
         (all-pitches (append hi-pitches low-pitches)))

  all-pitches
  )
)


(defun get-possible-registrated-pitches (organized-bpf-samples all-possible-pitches)

;;collect any registration of the original pitch
;;if it falls within the sampled bpfband
( loop for elem in all-possible-pitches
       collect (if (and (>= elem (nth 0 organized-bpf-samples))
                        (<= elem (nth 1 organized-bpf-samples))) elem))

)


;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-bands-register ( (bpf-lib list) (pitches list) &optional (mode 0))

  :icon 2
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" "mode: 0 = n/a; mode=1 for multiple pitch list input")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil '(6000 6000 6000 6000)  0)
  :doc "Returns random pitch samples that conforms to a list of allowable pitchclasses. The pitches sampled are determed following the contour of the bpf pitch bands provided. Optional Mode=1 will take in multiple pitchlists."
  
  (case mode

    (0 
        (let* ((bpf-samples (mapcar (lambda (x) (scaled-bpf-samples-registration x pitches)) bpf-lib))
               ;;put these bpf samples into high-low pairs
              (organized-bpf-samples (mat-trans bpf-samples))
              ;;now get all of the possibly relevant pitches
              (all-possible-pitches (mapcar (lambda (x) (get-registrated-pitches x )) pitches))
              (pitches-fitting-registration (mapcar (lambda (x y) (get-possible-registrated-pitches x y)) organized-bpf-samples all-possible-pitches))
              (right-pitches-no-nils (mapcar (lambda (x) (remove nil x)) pitches-fitting-registration)))

          ;;now return a random pitch from the pool of possible pitches for each pitch 
          ;;originally provided in the pitches list

          (mapcar (lambda (x)  (nth (om-random 0 (- (length x) 1)) x)) right-pitches-no-nils)
          )

          )

    (1
     ;this mode deals with many-voiced-rhythm-lists
     (mapcar (lambda (x) (p-bands-register bpf-lib x)) pitches)
     
     )
    )
)



;;;==================================
;;; P-INVERSION
;;;==================================

(in-package :cnmat)



(defun list-inversion (inversion-element elements-list)

 (mapcar (lambda (x) (helper-inversion inversion-element x)) (flat elements-list))

)


(defun helper-inversion (inversion element)

  (cond ((> element inversion) (- inversion  (- element inversion) ))
      (t (+ (- inversion element) inversion)))
)




(om::defmethod! p-inversion ((pitchlist list) (inversion-element integer))

  :icon 7
  :indoc '("a list of pitch lists" "a pitch to invert around")
  :outdoc '("Returns a list of lists of pitches inverted around a given pitch") 
  :initvals '(((7000 7100 8000) (6000 6800 7000)) 7100)
  :doc "Returns a list of lists of elements from the source lists chosen by index number"

  
  (list (mapcar (lambda (x) (list-inversion inversion-element x)) pitchlist))

  
)





;;;==================================
;;; P-MAKE-BANDS
;;;==================================

(in-package :cnmat)

(defun make-bpfs ( my-chord-seq separate-into-two-pitch-lists)

  (list
       (om::om-make-bpf 'bpf 
                       (butlast (om::lonset my-chord-seq)) ;;;x
                       (nth 0 separate-into-two-pitch-lists) ;;;y
                       2 ;;;number of decimals
                      )

        (om::om-make-bpf 'bpf 
                       (butlast (om::lonset my-chord-seq)) ;;;x
                       (nth 1 separate-into-two-pitch-lists) ;;;y
                       2 ;;;number of decimals
                      )
    )
)

(om::defmethod! p-make-bands ((my-chord-seq chord-seq) &optional (mode 0))
  :icon 7
  :indoc '("a chord-seq" "mode: 0 = n/a")
  :initvals '((nil) 0)
  :outdoc '("Returns a bpf lib.")
  :doc "Returns a bpf lib for the pitch band to be sampled. Describes a band of pitches. Takes in a chord-seq with  three of more dyads. Returns a bpf lib describing a pitch band that can be sample across a series of attacks. Make-bands should be used in conjunction with objects pitch->bands-pitchclass or pitch->bands-collection objects. See examples: 5b pitch->bands-collection or 5c pitch-bands->pitchclass"

  (case mode 
    ;;;sum of the lists of lists per voice
    (0 
     
        (let* ((sorted-pitch-lists (mapcar (lambda (x) (sort x  '<)) (om::lmidic my-chord-seq)))
               (separate-into-two-pitch-lists (mat-trans sorted-pitch-lists)))
              
               
               (om::make-instance 'bpf-lib 
                       :bpf-list (make-bpfs my-chord-seq separate-into-two-pitch-lists) ;;;bpf-lists
                      )
           
          )
     )

    )
)




;;;==================================
;;; P-MAP1
;;;==================================

(in-package :cnmat)

;;;mapping pitches onto the rhythmic lists in three different ways

(defun random-from-range-pitch-map (rhythm-list key-list)

;;do a one to one pitch mapping for each list of rhythms
;;go through each element in rhythmlist 
;;test the elemtn until you find a match in the key-list
;;then return the corresponding value

   (flat 
        (loop for attack in rhythm-list
              collect (loop for elem in key-list
                    if (eq attack (car (car elem))) 

;; now make a random choice to return from the elements 
;; from the range from one note to the other 
;;(nth (random (length my-list)) my-list)

                          collect (nth 
                             (random (length (arithm-ser (car (nth 1 elem)) (car (cdr (nth 1 elem))) 100))) 
                                 (arithm-ser (car (nth 1 elem)) (car (cdr (nth 1 elem))) 100))))

    )
  
)



(defun random-from-set-pitch-map-probs (rhythm-list key-list)

;;do a one to one pitch mapping for each list of rhythms
;;go through each element in rhythmlist 
;;test the elemtn until you find a match in the key-list
;;then return the corresponding value

   (flat 
      (loop for attack in rhythm-list
          collect (loop for elem in key-list
                    if (eq attack (car (car elem))) 

                    ;; now make 1 random choice from the elements to return using the probabilities

		  collect (o-list-rand (nth 1 elem) 1))


       )
    )
  
)

(defun prep-mapping-list (mapping-list numattacks)
  (let* ((mylist (mapcar (lambda (x) ( flat (repeat-n (cdr x) numattacks))) mapping-list))
        (mylist-with-keys (mapcar (lambda (x y) (list (car x) y)) mapping-list mylist)))
    
    
    
    mylist-with-keys

    )

)

(defun one-one-pitch-map (rhythm-list key-list)

;;do a one to one pitch mapping for each list of rhythms
;;go through each element in rhythmlist 
;;test the elemtn until you find a match in the key-list
;;then return the corresponding value

   (flat 

    (loop for attack in rhythm-list
          collect (loop for elem in key-list
                        if (eq attack (car (car elem))) 
                        collect (car (cdr elem))) )
  )
  
)

(defun one-one-pitch-map-multiples (rhythm-list key-list)

;;do a one to one pitch mapping for each list of rhythms
;;go through each element in rhythmlist 
;;test the elemtn until you find a match in the key-list
;;then return the corresponding value

 (flat 

    (loop for attack in rhythm-list
          collect (loop for elem in key-list
                        if (eq attack (car (car elem))) 
                        collect (pop (car (cdr elem)))
                        ;;moves you to the next mapping number
                        
                        ))
  )
)

;;THIS CHECKING CODE DOESNT WORK YET
(defun check-pitch-map (rotations-list mapped-pitch-list)

;;if there are more attacks than mapped pitches send out an error message

           (if (eq (length (flat rotations-list)) (length (flat mapped-pitch-list)))
                 mapped-pitch-list
                 'error-in-mapping
            )
      
)


  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-map1 ( (durations-list list) (mapping-list list) &optional (mode 0))

  :icon 2
  :indoc '("a list of rhythm lists" "a list of mappings in midics" "mode: 0 = 1-1 mapping; 1 = map to random choice from set; 2 = map to random choice in a range of choices. (Use two midic pitches to designate the bottom and top of range")
  :outdoc '("a list of pitch mappings to use with") 
  :initvals '( ((3 4 5) (4 5 3) (5 3 4)) (((3) (6000)) ((4) (6100)) ((5) (6200)))  0)
  :doc "Mapping in three different ways rotations->poly2 object"
  
  (case mode

    (0 (let* ((prepped-mapping-list (prep-mapping-list mapping-list (length (car durations-list))))
             (resultant-pitches (mapcar (lambda (x) (one-one-pitch-map-multiples x  (clone prepped-mapping-list))) durations-list))
             )

             resultant-pitches

         )
       )
    ;;this mode not used--regular random choice no weight--can go back to this if needed
    (3 (let ((resultant-pitches (mapcar (lambda (x) (random-from-set-pitch-map x mapping-list)) durations-list)))
         
         (check-pitch-map durations-list resultant-pitches)
         )
       )
    (1 (let ((resultant-pitches (mapcar (lambda (x) (random-from-set-pitch-map-probs x mapping-list)) durations-list)))
         
         (check-pitch-map durations-list resultant-pitches)
         )
       )
    (2 (let ((resultant-pitches (mapcar (lambda (x) (random-from-range-pitch-map x mapping-list)) durations-list)))
         
         (check-pitch-map durations-list resultant-pitches)
         )
       )

   
    )

)


;;;this receives from rfi object
(om::defmethod! p-map1 ( (durations-list prf) (mapping-list list) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (pitch-mapper1 durations mapping-list mode)))

;;;




;;;==================================
;;; P-MAP2
;;;==================================

(in-package :cnmat)

;;;mapping pitches onto the rhythmic lists in three different ways


(defun get-random-pitch (pitch-collection durations-list)
  (loop for attack in durations-list
        collect (nth (random (length pitch-collection)) pitch-collection)
      )


)

(defun get-random-pitch-from-range (pitch-collection durations-list)
  (loop for attack in durations-list
        collect (nth 
                             (random (length (arithm-ser (car pitch-collection) (car (cdr pitch-collection)) 100))) 
                                 (arithm-ser (car pitch-collection) (car (cdr pitch-collection)) 100)))
     
)
  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-map2 ( (durations-list list) (pitch-collection list) &optional (mode 0))

  :icon 2
  :indoc '("a list of rhythm lists (list of lists)" "a list (or list of lists) for pitch collections" "mode: 0 = random choice from a pitch collection; 1 = random choice from a range of pitches")
  :outdoc '("a list of pitch mappings to use with") 
  :initvals '( ((3 4 5) (4 5 3) (5 3 4)) (7200 7300 7500 7700)  0)
  :doc "Pitch mapping for each attack in each voice using random choice from a provided pitch collection.  In mode 1, random pitch selection from individual pitch collections, one for each voice.  In this case, provide a list of lists for wach voice pitch collection"
  
  (case mode

    (0 
             (let ((resultant-pitches (mapcar (lambda (x) (o-list-rand pitch-collection (length x))) durations-list)))
             (flat resultant-pitches 1)
             )
     )
          
     
(2 

         (cond ((listp (car pitch-collection))
               ;;this is the version when there are a list of list of pitch-collections
               (let ((resultant-pitches (mapcar (lambda (x y) (get-random-pitch-from-range x y)) pitch-collection durations-list)))
             resultant-pitches
             ))
             (t ;this is the version when there is just one shared pitch collection
             (let ((resultant-pitches (mapcar (lambda (x) (get-random-pitch-from-range pitch-collection x)) durations-list)))
             resultant-pitches
             ))
          )
     )

(1

         (cond ((listp (car pitch-collection))
               ;;this is the version when there are a list of list of pitch-collections
               (let* ( (length-durations (mapcar (lambda (x) (length x)) durations-list))
                       (resultant-pitches (mapcar (lambda (x y) (o-list-rand x y))  pitch-collection length-durations )))


             (flat resultant-pitches 1)
             ))
             (t nil
             )
          )
     )


    )

)


;;;this receives from rfi object
(om::defmethod! p-map1 ( (durations-list prf) (mapping-list list) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (pitch-mapper1 durations mapping-list mode)))

;;;










