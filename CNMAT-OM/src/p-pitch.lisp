;;;==================================
;;; P-PITCH
;;;==================================



;;;==================================
;;; Q-PERMUTATIONS+CANON
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(defun canon-growth-test (mylist)

;; iteratively add elements from the list and include them if they pass the canon-query test
  (let* ((outputlist '()))
    (loop for elem in mylist do
          (let* ((test (cons elem outputlist)))
              (if (q-canon test) (push elem outputlist))
            ))

;return the list in the order that elements were added
  (reverse outputlist)
  )
)
  

(om::defmethod! q-permutations+canon ((mylist list) )

  :icon 2
  :indoc '("a list of lists" "optional mode argument")
  :outdoc '("lists that pass the canon query") 
  :initvals '((1 2 3 4 5) )
  :doc "From the original lists it builds lists of lists based on a iterative operations.  The operation takes element a and compares with with element b.  If there are no onset overlaps, then it adds b to the list and moves on to the next element C. and so on.  This process is repeated for each successive element (list) of the original input, i.e. for every rotation of the original input list.  (It is not done for every possible permutation of the original list.)  Grouped output lists should have no synchronous onsets except for the first."


  (let* ((mylist-rotations (print (get-rotations mylist)))
         (final-list (mapcar (lambda (x) (canon-growth-test x)) mylist-rotations)))
               
      
;return the final list
    final-list
    )



)



;;;==================================
;;; Q-PERMUTE+REMOVE-DUP-ROTATIONS
;;;==================================

(in-package :cnmat)


(defun test-canon (mylist)
  (let ((canon (list (first mylist))))
    (loop for elem in (cdr mylist) do
          (if (canon-query (cons elem canon))
              (push elem canon)))
    (reverse canon)))

(defun is-rotation? (l1 l2)
  (let ((ok nil))
    (loop for i from 0 to (1- (length l1))
          while (not ok)
          do (setf ok (equal (rotate l1 i) l2)))
    ok))


(om::defmethod! q-permute+remove-dup-rotations ((main-list list))

  :icon 3
  :indoc '("a list of lists")
  :outdoc '("Tests all permutations of a list, checking it for duplicates in the form of rotations.") 
  :initvals '((1 2 3))
  :menuins '((1 (("index list" 0) )))
  :doc "Tests all permutations of a list, checking it for duplicates in the form of rotations."
  
  (let ((results (remove-duplicates (permutations main-list) :test #'is-rotation?)))
    (mapcar (lambda (x) (reverse x)) results)

    )

)









;;;==================================
;;; Q-PERMUTATIONS+CANON-UTILITY
;;;==================================

(in-package :cnmat)

;;default: return result from permutations+canon-growth excluding all rotations. 
;;mode1: return result from permutations+canon-growth-all sorted by fewest overlaps
;;mode2: return result from permutations+canon-growth-all sorted by fewest overlaps and excluding rotations


(om::defmethod! q-permutations+canon-utility ((mylist list) &optional (mode 0))

  :icon 2
  :indoc '("a list of lists" "a mode argument")
  :outdoc '("lists") 
  :initvals '( '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) 0)
  :doc "Utility for the permutations+canon-growth and permutations+canon-growth-all objects. Default: return result from permutations+canon-growth excluding all rotations. Mode1: return result from permutations+canon-growth-all sorted by fewest overlaps. Mode2: return result from permutations+canon-growth-all sorted by fewest overlaps and excluding rotations "


(case mode
    
    (0 ; DEFAULT
    ;now return the final-list

 (let* ((final-list '()))
    (loop for elem in mylist do
            (unless (find  elem final-list :test #'cnmat::is-rotation?)
              (push elem final-list)))
               
      
;return the final list
    (reverse final-list)
    )

   )


    
 

 (1 ; CASE 1 return all sorted by fewest overlaps

 
(let* ((reversed-list (mapcar (lambda (x) (reverse x)) mylist))
       (sorted-list  (sort reversed-list #'< :key #'car))
       ;re-order the output list and send it out
       (pre-final-list   (mapcar (lambda (x) (reverse x)) sorted-list)))
       ;remove duplicates
  (remove-dup pre-final-list 'eq 2)
)
   )

(2 ; CASE 2 return all sorted by fewest overlaps and excluding rotations

(let* ((reversed-list (mapcar (lambda (x) (reverse x)) mylist))
       (sorted-list  (sort reversed-list #'< :key #'car))
       ;re-order the output list and send it out
       (pre-final-list   (mapcar (lambda (x) (reverse x)) sorted-list))
       ;remove duplicates
  
      ( mylist_x (remove-dup pre-final-list 'eq 2))
      (final-list '()))

    (loop for elem in mylist_x do
            (unless (find  elem final-list :test #'cnmat::is-rotation-special?)
              (push elem final-list)))
               
      
;return the final list
    (reverse final-list)
    )



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
         
        (print "new version")
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

  ;;(print "aaah!!!")
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
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" "mode: 0 = n/a")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil '(6000 6000 6000 6000)  0)
  :doc "Returns random pitch samples that conforms to a list of allowable pitchclasses. The pitches sampled are determed following the contour of the bpf pitch bands provided "
  
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
    
    ;(print 'mylist-with-keys)
    ;(print mylist-with-keys)
    
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










