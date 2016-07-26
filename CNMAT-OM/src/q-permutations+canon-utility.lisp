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
