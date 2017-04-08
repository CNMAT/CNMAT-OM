;;;==================================
;;; O-OPERATIONS
;;;==================================

(in-package :cnmat)








;;;==================================
;;; O-LIST-LENGTHS
;;;==================================


;;this is in process and not being used yet

(om::defmethod! o-list-lengths ((my-list list) &rest rest-lists)

  :icon 6
  :indoc '("a list of lists or more lists of lists" )
  :outdoc '("compare lists of sublists") 
  :initvals '(((1 2 3 4) (5 6 7 8)) (nil))
  :doc "Compare the lengths of lists of lists"

  (let ((all-lists (append my-list  rest-lists )))
        (final-list '())
        (current-list '()))

    (print all-lists)

   (loop for subcategory in all-lists
    for i from 0 to (length all-lists) do
       (loop for a-list in subcategory do
           (loop for elem in a-list do
                 (push (length elem) current-list)))
     (push (list 'list i (reverse current-list)) final-list))
   
  
  final-list
  )
   



;;;==================================
;;; O-COUNT-TRUNC
;;;==================================
;;;A COMPLETE LISTING OF 0-OPERATIONS CODE


;;;COUNT-TRUNC
;;;truncate a list if it is too long

(defun list-lengths (my-list)
  (length my-list)
)

(defun list-truncate (my-list list-length length-desired)
  (cond ((<= list-length length-desired) (subseq my-list 0 list-length))
        (t (subseq my-list 0 length-desired))
        )
)

(om::defmethod! o-count-trunc ((my-list list) (length-desired integer))

  :icon 2
  :indoc '("a  list of lists" "the desired length in elements of each list")
  :outdoc '("list of lists with elements of the desired length") 
  :initvals '(((1 2 3)) 2)
  :doc "As necessary, truncates all lists to the length of desired elements"

  (let ((lengths (mapcar #'list-lengths my-list)))
    (mapcar (lambda (x y) (list-truncate x y length-desired)) my-list lengths)

    )
)




;;;==================================
;;; O-CRIBS
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(om::defmethod! get-rotations ((durations list))
  :icon 2
  :indoc '("a list of lists of values")
  :outdoc '("list of rotations (circular permutations)") 
  :initvals '((1 2 3))
  :doc "Computes and returns the list of all rotations (circular permutations) of a given list <durations>."
  (loop for i from 0 to (1- (length durations)) collect (om::rotate durations i)))
  

(om::defmethod! o-cribs ((my-list list))

  :icon 2
  :indoc '("a list of lists of values")
  :outdoc '("list of rotations (circular permutations) for each list") 
  :initvals '(((1 2 3) (4 5 6)))
  :doc "Computes and returns the list of all rotations (circular permutations) of a given group of lists <durations lists>."

  (mapcar #'get-rotations my-list)

)



;;;==================================
;;; O-EQUAL-PROB
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function


  

(om::defmethod! o-equal-prob ((my-list list))

  :icon 2
  :indoc '("a list of elements")
  :outdoc '("a list of elements and a list of equal probabilities = 1") 
  :initvals '((55 56 57 58 59))
  :doc "Takes in a list of elements and returns the original list plus a list of equal probabilities for each element with a total sum of 1.0"

  (list (flat my-list)
        (let ((my-length (length (flat my-list))))
          (om-scale/sum (repeat-n (/ 1 my-length) my-length) 1.0)
          )
       
   )
)





;;;==================================
;;; O-INDEX
;;;==================================

(in-package :cnmat)



(defun get-position (source-list index-list)

  (mapcar #'posn-match source-list index-list)
)



(om::defmethod! o-index ((source list) (index-lists list) &optional (mode 0))

  :icon 7
  :indoc '("a list of lists" "a list of lists" "mode: 0 = multiple source lists with multiple index lists; 1 = single source list with multiple index lists")
  :outdoc '("Returns a list of lists of elements from the source lists chosen by index number") 
  :initvals '(((3 4 5 1 2)) ((0 1 2 3 4)) 0)
  :menuins '((2 (("multiple source lists with index list" 0) ("single source list, multiple index lists" 1) )))
  :doc "Returns a list of lists of elements from the source lists chosen by index number"
  
  (case mode
    (0  (mapcar (lambda (x) (get-position x (flat index-lists 1))) source)
     )
     
    (1 (list (mapcar (lambda (x) (posn-match (flat source) x)) (flat index-lists 1)))
     )
  )

)






;;;==================================
;;; O-LIST-RAND
;;;==================================

(in-package :cnmat)


(defun build-random-weights (mylist)

  (let* ((start-list mylist)
      (final-list '())
      (given-probabilities-sum 'nil)
      (remaining-probabilities 'nil)
      (caluculated-probabilities '())
      (all-probabilities '())
      (dxs '()))




;;put the elements with probability.  Test to check if there is a chord or not. If the number is a float, then it is a
      (mapcar (lambda (x) (if (and (listp x) (floatp (second x))) (progn (push x final-list ) (setq start-list (remove x start-list))))) mylist)


      
;;now add up the probabilities that were given originally
      (setq given-probabilities-sum (reduce '+ (mapcar (lambda (x) (second x)) final-list)) )
     
;;now subtract 1-given-probabilities sum and divide the result by number
;;of elements left over, i.e. ones without given probabilities

      (setq remaining-probabilities (/ (- 1.0  given-probabilities-sum) (round (length start-list))) )

;;assign remaining-probabilities to the remaining elements in start-list

      (setq calculated-probabilities (mapcar (lambda (x) (list x remaining-probabilities)) start-list))

;;add calculated probabilities to the final list
      (mapcar (lambda (x) (push x final-list)) calculated-probabilities)


;;now bundle the isolated pitches in final-list as needed for the sorting to follow
      (loop for i from 0 to (- (length final-list) 1) do
            (if (not (listp (car (nth i final-list)))) (setf (nth i final-list) (list (list (car (nth i final-list))) (flat (cdr (nth i final-list)))))))
            


(defun sort-by-first-elem (elem)
  
(car (car elem))

)


;;now push in all the probabilities together in pitch order low to high
      
      (setq final-list (sort final-list #'< :key #'sort-by-first-elem))





;;make a list of all the probabilities
(loop for elem in final-list do
      (if (listp (second elem)) (push (car (cdr elem)) all-probabilities) (push (second elem) all-probabilities)))

;restore correct ordering of probabilities
(setq all-probabilities (flat (reverse all-probabilities)))



;;now add dx->x values for each list in the final-list
      (setq dxs (cdr (dx->x 0 all-probabilities)))


;;now add those to the final-list and output for random choice

      (mapcar (lambda (x y)  (append x (list y)))  final-list dxs)

   )
)




(defun build-sequence ( mylist)

  (let* ((randnum (random 1.0))
  ;;go through and get all values that have probability less than
  ;;the given randnum
     (prelim-results (mapcar (lambda (x) (if (<= randnum (third x)) (first x))) mylist))
     ;;(secondary-results (mapcar (lambda (x) (remove nil x)) prelim-results);;) 
     )

;;then just return the first one that is not a nil

      (first (remove nil prelim-results))
  
     )

)



(om::defmethod! o-list-rand ((mylist list) (num-return-vals number))

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

  (list (reverse final-list))


)

)




;;;==================================
;;; O-LIST-REPEAT
;;;==================================

(in-package :cnmat)



(defun repeater (my-list no-times)

      (mapcar (lambda (x)   (flat (repeat-n x no-times))) my-list) 
)



(om::defmethod! o-list-repeat ((mylist list) (desired-length integer)  &optional (mode 0))

  :icon 6
  :indoc '("a list of lists" "an integer number for number of repetitions")
  :outdoc '("mode=0 returns a repeated list preserving the original list structure; mode=1 returns a repeated list preserving the original list structure and tailored to desired length input") 
  :initvals '(((1 2 3)(34 45)) 5 0)
  :doc "Repeats a given list n-times while preserving the original list structure"

  (case mode 

    ;;this is simple list repeat
    (0
    (repeater mylist desired-length)
    )

    ;;this is the "tailored" version of list-repeat
    (1

     (let* ((repeats-needed (mapcar (lambda (x) (ceiling (/ desired-length (length x)))) mylist))
     ; (make-repeats (mapcar (lambda (x y) (flat (repeat-n x y) 1)) mylist repeats-needed))
            (make-repeats (mapcar (lambda (x y)  (repeat-n x y) ) mylist repeats-needed))

            (pre-final-lengths (mapcar (lambda (x) (length (flat x))) make-repeats))
            (list-pre-group '())
            (list-groupings '())
            (final-list '())
            (final-lengths '())
            (very-final-list '())
            (hold '())
            (no-nils-very-final-list '()))

  ;(list make-repeats final-lengths)

       (loop for currlist in make-repeats do
             (loop for elem in currlist do
                   (push (length elem) list-pre-group))
             (push (reverse list-pre-group) list-groupings)
             (setf list-pre-group '()))

       (setf list-groupings (reverse list-groupings))


      ;; (print 'list-groupings)
      ;; (print list-groupings)
        
       (loop for currlist in make-repeats
             for currlen in pre-final-lengths do

             (cond ((<= currlen desired-length) (push currlist final-list))
                   (t (push  (subseq (flat currlist) 0 desired-length) final-list))
                   ))

       (setq final-list (reverse final-list))
  
       (loop for currlist in final-list do
             (push (length (flat currlist)) final-lengths))
  
       (setq final-lengths (reverse final-lengths))

      ; (print 'final-list)
      ; (print final-list)

       (loop for currlist in final-list 
             for currgrouping in list-groupings do
             (push  (group-list currlist currgrouping 'linear) very-final-list))

       ;;get rid of any nils!!!!

       (loop for currlist in very-final-list do
             (loop for elem in currlist do
                   (push (remove nil elem) hold))
             (push (reverse hold) no-nils-very-final-list)
             (setf hold '()))
       
                   

      ; (setf no-nils-very-final-list (reverse no-nils-very-final-list))
  
       (list no-nils-very-final-list final-lengths)

       )

     )    

    )

)



;;;==================================
;;; O-SUM-LISTS
;;;==================================

(in-package :cnmat)

;;;map the lists when given list of lists to maintain
(defun map-lists (my-list)
   (list (mapcar #' reduce-sum-as-list my-list)))

;;;helper function for sum-lists that returns as a list each time
(defun reduce-sum-as-list (list)
  (list (reduce #' + (flat list))))

;;;helper function for sum-lists
(defun reduce-sum (mylist)
  (reduce #' + (flat mylist))
)

(defun reduce-sum-lists (mylist)
  (list (reduce #' + (flat mylist)))
)

;;;(defun sum-of-lists (list)
;;;  (mapcar #'reduce-sum list))

;;; output sums of lists of lists
(om::defmethod! o-sum-lists ((my-list list) &optional (mode 0))
  :icon 5
  :indoc '("list of lists to be summed" "mode: 0 = sum all numbers; 1 = sum numbers and retain list structure")
  :initvals '(((1 2 3) (4 5 6)) 0)
  :menuins '((1 (("sum of the list of lists per voice" 0) ("sums that preserve list structure" 1))))
  :doc "Returns a list of sums of list arguments"

  (case mode 
    ;;;sum of the lists of lists per voice
    (0 (let ((final-list '())
      (prelim-list '()))

       (loop for elem in my-list do
       (setq prelim-list '())
           (loop for onelist in elem do
                (push (list (reduce '+ onelist)) prelim-list))
           (push (list (reduce '+ (flat prelim-list))) prelim-list)
           (push (reverse prelim-list) final-list))

       (reverse final-list)
     )

     )
   
    ;;;sum that preserves the list structure
    ;;(1 (mapcar #'reduce-sum-lists my-list))
    ;;;format for double-checking sums of sublists
    
    ;;(2 (reduce-sum my-list))

    (1 (let ((hold-sublist '())
             (final-list '()))
               
         (loop for sublist in my-list do
               (loop for sub-sublist in sublist do
                     (push (list (length sub-sublist)) hold-sublist))
               (push (reverse hold-sublist) final-list)
               (setq hold-sublist '()))
         (reverse final-list)
      
     )
     )
    )
)



;;;==================================
;;; O-LIST-INFO
;;;==================================


(defun tatum-lookup (tatum)
  ;;returns number of tatums per beat
  (cond ((= tatum 1) 1)
        ((= tatum 2) 1)
        ((= tatum 4) 1)
        ((= tatum 8) 2)
        ((= tatum 12) 3)
        ((= tatum 16) 4)
        ((= tatum 20) 5)
        ((= tatum 24) 6)
        (t nil))
)


;;; output sums of lists of lists
(om::defmethod! o-list-info ((mylist list) &optional (mode 0))
  :icon 5
  :indoc '("list of lists to be tallied" "mode: 0 length of sublist and sum of sublist returned retaining list structure"
           "mode 1 for simple list of lists, e.g. pitch lists")
  :initvals '(((1 2 3) (3 5 5 6 7) (19 43 59) (34) (68)) ((1 2 3 4) (22 4)) 0)
  :menuins '((1 (("list to insert" 0) ("optional mode" 1))))
  :doc "Returns a list of sums of list arguments"

  (case mode 
    ;;;sum of the lists of lists per voice
    (0 

     (let ((current-list '())
           (running-sum '())
           (running-length '())
           (final-list '()))

       (loop for sublist in mylist do
             (loop for elem in sublist do
                   (push (list (length elem) (reduce #'+ elem)) current-list)
                   (push (reduce #'+ elem) running-sum)
                   (push (length elem) running-length))
             (push (list (reduce #'+ running-length) (reduce #'+ running-sum)) current-list)
             (push (reverse current-list) final-list)
             (setq current-list '())
             (setq running-sum '())
             (setq running-length '()))

        (reverse final-list) 
       )
     )

    (1 
     (let ((current-list '())
           (running-sum '())
           (running-length '())
           (final-list '()))

       (loop for sublist in (list mylist) do
             (loop for elem in sublist do
                   (push (list (length elem) (reduce #'+ elem)) current-list)
                   (push (reduce #'+ elem) running-sum)
                   (push (length elem) running-length))
             ;this calculation is unnecessary here
             ;(push (list (reduce #'+ running-length) (reduce #'+ running-sum)) current-list)
             (push (reverse current-list) final-list)
             (setq current-list '())
             (setq running-sum '())
             (setq running-length '()))

       ;;flat the result one level and get rid of the unnecessary sums at the end
       (flat (reverse final-list) 1)
       )
     )

    (2 

     (let* ((current-list '())
           (running-sum '())
           (running-length '())
           (final-list '()))

       (loop for sublist in mylist do
             (loop for elem in sublist do
                   (push (list (first elem) (om* (first elem)   (tatum-lookup (second (flat elem))))) current-list)
                   ;(print current-list)
                   )
             (push (list (reduce #'+ (mapcar #'first current-list))(reduce #'+ (mapcar #'second current-list))) current-list)
             (push (reverse current-list) final-list)
             (setq current-list '()))

       (reverse final-list)
       )
     )


     )
)



;;;==================================
;;; O-LIST-INFO-2
;;;==================================



;;; output sums of lists of lists
(om::defmethod! o-list-info2 ((mylist list) (tatumlist list) &optional (mode 0))
  :icon 5
  :indoc '("list of lists to be tallied" "a tatum list to be tallied"
           "An optional mode argument; mode 1 for simple list of lists, e.g. pitch lists")
  :initvals '(((1 2 3) (3 5 5 6 7) (19 43 59) (34) (68)) ((1 2 3 4) (22 4)) 0)
  :numouts 2
  :menuins '((1 (("sum of the list of lists per voice" 0) ("sums that preserve list structure" 1))))
  :doc "Returns a list of sums of list input."

     (values (o-list-info mylist mode) (o-list-info tatumlist 2)) 
  
)




;;;==================================
;;; O-LIST-TRANS
;;;==================================

;;;REQUIRES ALEA LIBRARY TO WORK



(in-package :cnmat)

(defun bpf-probabilities (mybpf num-samples)

  (let* ((samples (om::bpf-sample mybpf 'nil 'nil num-samples))
        (listA-probabilities (om::om- 1.0 (om::om/ samples 100)))
        (listB-probabilities (om::om- 1.0 listA-probabilities))
        )
   
    (mat-trans (list listA-probabilities listB-probabilities))
    )
)



;;;==================================
;;; O-TATUM-MAKER
;;;==================================



(om::defmethod! o-tatum-maker ((mylist list) )

  :icon 6
  :indoc '("a list of lists or more lists of lists" )
  :outdoc '("compare lists of sublists") 
  :initvals '( nil)
  :doc "Compare the lengths of lists of lists"

  (let (( hold-list '())
      (final-list '()))

    (loop for voice in mylist do
      (loop for elem in voice do
            (push (list (car elem)  (cdr elem)) hold-list))
      (push (reverse hold-list) final-list)
      (setf hold-list '()))

    (reverse final-list)

    )
)



;;;==================================
;;; O-TATUM-FORMAT
;;;==================================


(defun tatum-format-helper (mylist)

(let ((final-list '()))

(loop for elem in mylist do
      (cond ((eql elem 1) (push '(1 (4)) final-list))
            ((eql elem 2) (push '(1 (8)) final-list))
            ((eql elem 3) (push '(1 (12)) final-list))
            ((eql elem 4) (push '(1 (16)) final-list))
            ((eql elem 5) (push '(1 (20)) final-list))
            ((eql elem 6) (push '(1 (24)) final-list))
            ((eql elem 8) (push '(1 (32)) final-list))
            ((eql elem 10) (push '(1 (40)) final-list))
            ((eql elem 12) (push '(2 (6)) final-list))
            ((eql elem 15) (push '(1 (2)) final-list))
            ((eql elem 20) (push '(1 (1)) final-list))
            (t (push 'nil final-list)))
            )


(list (reverse final-list))
)

)

(defun tatum-elements-helper (mylist)

(let ((outputlist '())
      (pre-outputlist '()))

  (loop for sublist in mylist do
        (loop for elem in sublist do
              (print elem)
              (cond ((eql elem 20) (push 1 pre-outputlist)) ; whole notes
                    ((eql elem 15) (push 1 pre-outputlist)) ; half notes
                    (t (push elem pre-outputlist))))
        
        (push (reduce #'+ pre-outputlist) outputlist)
        (setf pre-outputlist '()))
        
  (reverse outputlist)
)

)


(om::defmethod! o-tatum-format ((mylist list) )

  :icon 6
  :indoc '("a list of lists or more lists of lists" )
  :outdoc '("compare lists of sublists") 
  :initvals '( nil)
  :numouts 2
  :doc "Converts a tatum shorthand into a notation suitable for use with tessellate objects"

 (values (flat  (mapcar #'tatum-format-helper  mylist) 1) (tatum-elements-helper mylist))

)

