;;;==================================
;;; O-LIST-REPEAT
;;;==================================

(in-package :cnmat)



(defun repeater (my-list no-times)

      (mapcar (lambda (x)   (flat (repeat-n x no-times))) my-list) 
)



(om::defmethod! o-list-repeat ((my-list list) (no-times integer))

  :icon 6
  :indoc '("a list of lists" "an integer number for number of repetitions")
  :outdoc '("returns a repeated list preserving the original list structure") 
  :initvals '(((1 2 3)(34 45)) 2)
  :doc "Repeats a given list n-times while preserving the original list structure"

   (repeater my-list no-times)
)
