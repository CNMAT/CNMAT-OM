
;;;==================================
;;; Q-QUERIES
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


  (let* ((mylist-rotations (get-rotations mylist))
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
;;; Q-COMBI-FROM-ELEMENTS
;;;==================================


(defun count-overlaps-sum-num-elems (mylist)
  (let* ((dx-list (mapcar (lambda (x) (dx->x 0 x)) mylist))
        (no-last-attacks (mapcar (lambda (x) (om::butlast x)) dx-list))
        (no-initial-attacks (mapcar (lambda (x) (rest x)) no-last-attacks))
        (flat-list (print (flat no-initial-attacks))))

    (let ((no-dupes-list (print (om::remove-dup flat-list 'eq 1)))
          (count 0))
;count the times a certain number appears in the list
      (loop for test-number in no-dupes-list do
        (let ((little-count 0))
        (loop for elem in flat-list do
              
              (if (eq test-number elem) (setq little-count (+ little-count 1))))

        ;if little-count is > 1 then add it to count
        ;then set little-count to 0 and continue
        (if (> little-count 1) (setq count (+ count little-count)))

        ))

      ;return a list of the original list and a count of overlaps list
       (flat (list mylist  (list (list "no_elems=" (length (first mylist)) "sum_elems=" (reduce #'+ (first mylist)) "overlaps=" count))) 1)
      )
  )

)




(defun q-combi-local-helper (elem)
(if (cnmat::q-canon elem) elem)
)



;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! q-combi-from-elements ( (elements-list list) (ordered number)  &optional (mode 0))

  :icon 2
  :indoc '("a list of elements" "ordered variable number" "mode: 0, 1, 2, 3 or 4")
  :outdoc '("a list") 
  :initvals '( (1 2 3 4 5 6 8 10 12) 1 0)
  :doc "Mode=0 is all combinations.  Mode=1 is only combinations that fit the canon query. Mode=2 all combinations in order of number of elements
Mode=3 all combinations in order of sum of elements.  Mode=4 all combinations in order of overlaps"
  
  (let* (( combis (mapcar (lambda (x) (q-combi elements-list x nil ordered)) elements-list))
        (rotations (mapcar #'q-rotations (flat combis 1)))
        (all-combis+data (mapcar #'count-overlaps-sum-num-elems rotations))
        )
    (print combis)
  (case  mode

    (0 all-combis+data
       )
    (1 (remove nil (mapcar #'q-combi-local-helper rotations))
       )

    ;;mode=2 sort by number of elements
    
    (2 (sort all-combis+data  #'< :key #'(lambda (x) (second (last-elem x))))
       )

    ;;mode=3 sort by sum of elements

    (3 (sort all-combis+data  #'< :key #'(lambda (x) (fourth (last-elem x))))
       )

    (4 (sort all-combis+data  #'< :key #'(lambda (x) (sixth (last-elem x))))
       )
       )
  )
)



;;;==================================
;;; Q-CANON-PERMUTATIONS-NO-OVERLAPS
;;;==================================

(in-package :cnmat)


(defun perm-canon (permutations-list mainlist)
 (let ((canon-query-result (canon-query (list mainlist (car permutations-list) )))
       (constructed-list (test-canon (cons mainlist permutations-list))))
    (omif canon-query-result constructed-list)
    )
)


(om::defmethod! q-canon-permutations-no-overlaps ((main-list list))

  :icon 3
  :indoc '("a list of lists")
  :outdoc '("Tests all permutations of a canon and only returns those with no overlaps.") 
  :initvals '((1 2 7 12))
  :menuins '((1 (("rhythm list" 0) )))
  :doc "Tests all permutations of a canon and only returns those with no overlaps."
  
  (let ((rotated-permutations-list (get-rotations (cdr (permutations main-list)))))
    (remove nil (mapcar (lambda (x) (perm-canon x main-list)) rotated-permutations-list))
    )
)





;;;==================================
;;; COMBINATIONS
;;;==================================

(in-package :cnmat)


;;; recursive search function (ordered without repetitions)
(defun search-n-sum (base rest n sum)
  (let ((curr-sum (apply '+ base)))
    (if (zerop n) (if (= curr-sum sum) (list base))
      (when (< curr-sum sum)
        (loop for restcdr on rest collect 
              (search-n-sum (append base (list (car restcdr))) (cdr restcdr) (1- n) sum)))
      )))

;;; recursive search function (not ordered without repetitions)
(defun search-n-sum-u (base rest n sum)
  (print (list "current base:" base))
  (let ((curr-sum (apply '+ base)))
    (if (zerop n) (if (= curr-sum sum) (list base))
      (when (< curr-sum sum)
        (loop for elem in (remove-if #'(lambda (x) (find x base :test '=)) rest) collect 
              (search-n-sum-u (append base (list elem)) rest (1- n) sum)))
      )))

;;; recursive search function (not ordered with repetitions)
(defun search-n-sum-r (base rest n sum)
   (list "current base:" base)
  (let ((curr-sum (apply '+ base)))
    (if (zerop n) 
        (when (= curr-sum sum) 
          ;(print " ===> FOUND ONE!") 
          (list base))
      (when (< curr-sum sum)
        (loop for elem in rest collect 
              (search-n-sum-r (append base (list elem)) rest (1- n) sum)))
      )))

(om::defmethod! q-combi ((dur-space list) (sum integer) (num list) &optional mode)
  :icon 1
  :indoc '("list of allowed durations" "sum of durations" "number of elements" "allow repeated elements?")
  :outdoc '("list of solutions") 
  :initvals '((1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120) 120 nil 0)
  :menuins '((3 (("ordered without repetitions" 0) ("unordered without repetitions" 1) ("unordered with repetitions" 2))))
  :doc "Computes and returns the list of all combinations of <num> elements in <dur-space> which sum up to <sum>.
<mode> (0, 1 or 2) determines whether or not the list must remain ordered and if repetitions are allowed in the results.

<num> can be a signle number or a list of integers. If <num> = NIL allnumber of elements will be searched."
  (let ((sorted-space (sort dur-space '<)))
  (loop for n in num append
        (remove nil 
                (loop for next on sorted-space
                      ;; no need to search further if the first element is too big already
                      while (<= (car next) sum) 
                      append (flat  
                              (remove nil 
                                      (case mode 
                                        (0 (search-n-sum (list (car next)) (cdr next) (1- n) sum))
                                        (1 (search-n-sum-u (list (car next)) next (1- n) sum))
                                        (2 (search-n-sum-r (list (car next)) next (1- n) sum))
                                        ))
                              (1- n))
                      )))))

(om::defmethod! q-combi ((dur-space list) (sum integer) (num integer) &optional mode)
   (q-combi dur-space sum (list num) mode))

(om::defmethod! q-combi ((dur-space list) (sum integer) (num null) &optional mode)
   (q-combi dur-space sum (arithm-ser 1 (length dur-space) 1) mode))





(om::defmethod! q-rotations ((durations list))
  :icon 2
  :indoc '("a list of values")
  :outdoc '("list of rotations (circular permutations)") 
  :initvals '((1 2 3))
  :doc "Computes and returns the list of all rotations (circular permutations) of a given list <durations>."
  (loop for i from 0 to (1- (length durations)) collect (om::rotate durations i)))
  

(om::defmethod! q-canon ((voices list) &optional print)
  :icon 3
  :indoc '("a list of list of durations")
  :outdoc '("T if the voices tile with no simultaneous attacks") 
  :initvals '(((1 2 3) (2 3 1) (3 1 2)))
  :doc "Checks wether or not the lists of durations form a canon (no simultaneous pulses except first and last one)"
  (let ((attacks (loop for v in voices append
                       (butlast (cdr (om::dx->x 0 v)))))
        (nodup t))
    ; there must be no equal values
    (loop for a on attacks 
          while nodup do
          (when (find (car a) (cdr a) :test '=)
            (setf nodup nil)))
    nodup))


(om::defmethod! q-rotation-canon? ((voice list) &optional print)
  :icon 4
  :indoc '("a list of durations" "print flag")
  :outdoc '("Returns the list if the voice rotations makes a conon, or NIL otherwise") 
  :initvals '((1 2 3) nil)
  :doc "Checks wether or not the list of durations forms a canon when rotated."
  (let ((result (q-canon (get-rotations voice))))
    (when print (print (format nil "-- testing ~A: ~A" voice result)))
    (if result voice)))



;;;==================================
;;; Q-N-PERMUTATIONS-NO-ROTATIONS
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function


  

(om::defmethod! q-n-permutations-no-rotations ((mylist list) (num-results number))

  :icon 2
  :indoc '("a list for permuting" "the number of permutations desired")
  :outdoc '("non-rotational permuted lists output") 
  :initvals '((1 2 3 4 5) 4)
  :doc "Returns a specified number of permutations from a given list such that none of the returned lists are rotations of one another."

  (let* ((final-list '())
         (count 0))
    (loop while (< count num-results) do
          (print 'got-here)
          (let* ((test (om::permut-random mylist)))
            (unless (find test final-list :test #'cnmat::is-rotation?)
              (progn (push test final-list) (setq count (+ count 1))))))
               
      
;return the final list
    final-list
    )

)
