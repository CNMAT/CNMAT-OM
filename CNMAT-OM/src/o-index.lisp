;;;==================================
;;; O-INDEX
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function


(defun get-position (source-list index-list)

  (mapcar #'posn-match source-list index-list)
)








(om::defmethod! o-index ((source list) (index-lists list) &optional (mode 0))

  :icon 7
  :indoc '("a list of lists" "a list of lists" "mode: 0 = multiple source lists with multiple index lists; 1 = single source list with multiple index lists")
  :outdoc '("Returns a list of lists of elements from the source lists chosen by index number") 
  :initvals '(((3 4 5 1 2)) ((0 1 2 3 4)) 0)
  :menuins '((2 (("multiple source lists with index list" 0) ("single source list, multiple index lists" 1) )))
  :doc "Returns a list of lists of elements from the source lists chosen by index number"
  
  (case mode
    (0  (mapcar (lambda (x) (get-position x (flat index-lists 1))) source)
     )
     
    (1 (list (mapcar (lambda (x) (posn-match (flat source) x)) (flat index-lists 1)))
     )
  )

)
