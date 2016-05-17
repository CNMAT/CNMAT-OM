;;;==================================
;;; RETROGRADE-LIST
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

;;; previous version using textfiles
;;;(defun retrograde (my-list)
;;; (let ((list-flat (flat my-list 1)))
;;;   (list (mapcar #'reverse list-flat))
;;;   )
;;;)


(defun retrograde (my-list)
 (mapcar #'reverse my-list)
)



(defun retrograde-flatten (my-list)
  (reverse (flat my-list))
)


(om::defmethod! retrograde-list ((my-list list) &optional (mode 0))

  :icon 7
  :indoc '("a list of lists" "mode: 0 = retain list structure; 1 = flatten lists")
  :outdoc '("Returns the retrograde of a list of lists, either preserving the sublists or flattening them into one list") 
  :initvals '(((1 2 3)) 0)
  :menuins '((1 (("retain lists structure" 0) ("flatten lists" 1) )))
  :doc "Returns the retrograde of a list of lists, either preserving the sublists or flattening them into one list"
  
  (case mode
    (0 (retrograde my-list)
     )
    (1 (retrograde-flatten my-list)
     )
  )

)
