;;;==================================
;;; Q-PERMUTATIONS+CANON-ALL
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(defun is-rotation-special? (l1 l2)
;;special version for lists with added overlaps argument

  (let ((ok nil)
        (xl1 (om::butlast l1))
        (xl2 (om::butlast l2)))

    (loop for i from 0 to (1- (length xl1))
          while (not ok)
          do (setf ok (equal (rotate xl1 i) xl2)))
    ok))

(defun count-overlaps (mylist)
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
      (flat (list mylist (list count)) 1)
      )
  )

)


(defun get-every-grouping-helper (mylist)

  (let* ((holding-list (cdr mylist))
         (first-element (car mylist))
      (masterlist '())) 

  (loop for i from 1 to (- (length mylist) 1) do
        (progn (push (cons first-element holding-list) masterlist)
               (setq holding-list (rotate holding-list 1))))

  (reverse masterlist)

  )

)


(defun get-every-grouping (mylist-rotations)

(let* ((bulk-list (get-every-grouping-helper mylist-rotations))
       (all-groupings '())
      (this-grouping '()))

;build a list with all of the groupings in this rotation
  
      (loop for elem in mylist-rotations do
        ;;here is where you get all the permutations of every grouping
        (progn (push elem this-grouping))
        (push (reverse this-grouping) all-groupings))

  (reverse all-groupings)

  )
        
)
  


(om::defmethod! q-permutations+canon-all ((mylist list) &optional (mode 0))

  :icon 2
  :indoc '("a list of lists" "optional mode argument")
  :outdoc '("lists that pass the canon query") 
  :initvals '( '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) 0)
  :doc "Provided with a group of lists, iteratively builds output lists returning lists built up by element for each rotation.
.  Returns combinations, supplies number of overlaps, and sorts by fewest overlaps. Default returns everything. Mode 1 returns everything sorted by fewest onset overlaps.  Mode 2 returns results sorted by fewest overlaps and excluding rotations"


(case mode
    
    (0 ; DEFAULT
    ;now return the final-list

  (let* ((mylist-rotations  (get-rotations mylist))
         (mylist-exploded  (print (flat (mapcar (lambda (x) (get-every-grouping-helper x)) mylist-rotations) 1)))

         (iterative-list   (mapcar (lambda (x) (get-every-grouping x)) mylist-exploded))
         ;dont do the first element because it is just 1 list, instead of 2 or more lists
         (pre-final-list (mapcar (lambda (x) (count-overlaps x)) (flat iterative-list 1)))
         (final-list '())
         )
; get rid of lists that have only 1 list of attacks because they dont matter    

    (loop for elem in pre-final-list do
          (if (> (length elem) 2) (push elem final-list))
    ) 

    (print 'final-list)
    (print final-list)
    ;(reverse final-list)

    (mapcar (lambda (x) (count-overlaps-sum-num-elems (butlast x))) (reverse final-list))

   )


    )
 

 (1 ; CASE 1 return all sorted by fewest overlaps

 
   )

(2 ; CASE 2 return all sorted by fewest overlaps and excluding rotations

       )


   )


)
