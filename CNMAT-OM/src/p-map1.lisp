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