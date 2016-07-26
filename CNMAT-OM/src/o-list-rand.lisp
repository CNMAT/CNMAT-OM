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
      (print 'given-probabilities)
      (print given-probabilities-sum)
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

(print 'all-probabilities)
      (print all-probabilities)
(print 'final-list)
      (print final-list)


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

    ;(list (repeat-n (mapcar (lambda (x) (build-sequence x)) built-weights) num-return-vals))
   
    ;(list
   ; (loop for x from 1 to num-return-vals 
   ;       collect(build-sequence built-weights))
   ; )

 
   (loop for x from 1 to num-return-vals do
         ( push (build-sequence built-weights) output-list))
   
   (reverse output-list)

   (loop for elem in output-list do
         (if (eq 'nil (cdr elem)) (push (car elem) final-list) (push  elem final-list) ))

  (list (reverse final-list))


)

)
