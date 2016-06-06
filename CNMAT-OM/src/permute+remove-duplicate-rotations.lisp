;;;==================================
;;; PERMUTE+REMOVE-DUPLICATE-ROTATIONS
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


(om::defmethod! permute+remove-duplicate-rotations ((main-list list))

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
