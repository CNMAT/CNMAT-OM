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
