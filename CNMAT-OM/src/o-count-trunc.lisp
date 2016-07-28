;;;==================================
;;; O-COUNT-TRUNC
;;;==================================

(in-package :cnmat)

;;;truncate a list if it is too long

(defun list-lengths (my-list)
  (length my-list)
)

(defun list-truncate (my-list list-length length-desired)
  (cond ((<= list-length length-desired) (subseq my-list 0 list-length))
        (t (subseq my-list 0 length-desired))
        )
)

(om::defmethod! o-count-trunc ((my-list list) (length-desired integer))

  :icon 2
  :indoc '("a  list of lists" "the desired length in elements of each list")
  :outdoc '("list of lists with elements of the desired length") 
  :initvals '(((1 2 3)) 2)
  :doc "As necessary, truncates all lists to the length of desired elements"

  (let ((lengths (mapcar #'list-lengths my-list)))
    (mapcar (lambda (x y) (list-truncate x y length-desired)) my-list lengths)

    )
)
