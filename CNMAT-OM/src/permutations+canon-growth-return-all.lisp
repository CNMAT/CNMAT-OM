;;;==================================
;;; PERMUTATIONS+CANON-GROWTH-RETURN-ALL
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

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

(defun get-every-grouping (mylist-rotations)

(let* ((all-groupings '())
      (this-grouping '()))
;build a list with all of the groupings in this rotation
  (loop for elem in mylist-rotations do
        ;;here is where you get all the permutations of every grouping
        (progn (push elem this-grouping))
        (push (reverse this-grouping) all-groupings))
  (reverse all-groupings)
  )
        
)
  


(om::defmethod! permutations+canon-growth-return-all ((mylist list))

  :icon 2
  :indoc '("a list of lists" "optional mode argument")
  :outdoc '("lists that pass the canon query") 
  :initvals '((1 2 3 4 5) )
  :doc "From the original lists it builds lists of lists based on a iterative operations.  Returns combinations, supplies number of overlaps, and sorts by fewest overlaps."


  (let* ((mylist-rotations  (get-rotations mylist))
         (iterative-list  (mapcar (lambda (x) (get-every-grouping x)) mylist-rotations))
         ;(final-list (mapcar (lambda (x) (count-overlaps x)) mylist-rotations))
         ;dont do the first element because it is just 1 list, instead of 2 or more lists
         (pre-final-list (mapcar (lambda (x) (count-overlaps x)) (flat iterative-list 1)))
         (final-list '())
         )
           
; get rid of lists that have only 1 list of attacks because they dont matter    

    (loop for elem in pre-final-list do
          (if (> (length elem) 2) (push elem final-list))
    )

    ;now return the final-list
    (reverse final-list)
    )



)
