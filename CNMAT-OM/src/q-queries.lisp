
;;;==================================
;;; Q-QUERIES
;;;==================================




;;;==================================
;;; Q-COMBI-FROM-ELEMENTS
;;;==================================

(defun q-combi-local-helper (elem)
(if (cnmat::q-canon elem) elem)
)



;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! q-combi-from-elements ( (elements-list list) (ordered number)  &optional (mode 0))

  :icon 2
  :indoc '("a list of elements" "ordered variable number" "mode: 0or 1")
  :outdoc '("a list") 
  :initvals '( (1 2 3 4 5 6 8 10 12) 1 0)
  :doc "Converts a rotation list into music notation using the ."
  
  (let* (( combis (mapcar (lambda (x) (q-combi elements-list x nil mode)) elements-list))
        (rotations (mapcar #'q-rotations (flat combis 1)))
        )

    
  (case mode

    (0 (mapcar #'count-overlaps rotations)
       )
    (1 (remove nil (mapcar #'q-combi-local-helper rotations))
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
          (let* ((test (permut-random mylist)))
            (unless (find test final-list :test #'cnmat::is-rotation?)
              (progn (push test final-list) (setq count (+ count 1))))))
               
      
;return the final list
    final-list
    )

)
