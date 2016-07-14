;;;==================================
;;; PERMUTATIONS+CANON-GROWTH
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(defun canon-growth-test (mylist)

;; iteratively add elements from the list and include them if they pass the canon-query test
  (let* ((outputlist '()))
    (loop for elem in mylist do
          (let* ((test (cons elem outputlist)))
              (if (canon-query test) (push elem outputlist))
            ))

;return the list in the order that elements were added
  (reverse outputlist)
  )
)
  

(om::defmethod! permutations+canon-growth ((mylist list) )

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
