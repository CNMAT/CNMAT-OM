;;;==================================
;;; CANON-PERMUTATIONS-NO-OVERLAPS
;;;==================================

(in-package :cnmat)


(defun perm-canon (permutations-list mainlist)
 (let ((canon-query-result (canon-query (list mainlist (car permutations-list) )))
       (constructed-list (test-canon (cons mainlist permutations-list))))
    (omif canon-query-result constructed-list)
    )
)


(om::defmethod! canon-permutations-no-overlaps ((main-list list))

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
