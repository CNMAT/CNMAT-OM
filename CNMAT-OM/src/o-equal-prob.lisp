;;;==================================
;;; O-EQUAL-PROB
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function


  

(om::defmethod! o-equal-prob ((my-list list))

  :icon 2
  :indoc '("a list of elements")
  :outdoc '("a list of elements and a list of equal probabilities = 1") 
  :initvals '((55 56 57 58 59))
  :doc "Takes in a list of elements and returns the original list plus a list of equal probabilities for each element with a total sum of 1.0"

  (list (flat my-list)
        (let ((my-length (length (flat my-list))))
          (om-scale/sum (repeat-n (/ 1 my-length) my-length) 1.0)
          )
       
   )
)
