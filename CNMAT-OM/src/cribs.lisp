;;;==================================
;;; CRIBS
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(om::defmethod! get-rotations ((durations list))
  :icon 2
  :indoc '("a list of lists of values")
  :outdoc '("list of rotations (circular permutations)") 
  :initvals '((1 2 3))
  :doc "Computes and returns the list of all rotations (circular permutations) of a given list <durations>."
  (loop for i from 0 to (1- (length durations)) collect (om::rotate durations i)))
  

(om::defmethod! cribs ((my-list list))

  :icon 2
  :indoc '("a list of lists of values")
  :outdoc '("list of rotations (circular permutations) for each list") 
  :initvals '(((1 2 3) (4 5 6)))
  :doc "Computes and returns the list of all rotations (circular permutations) of a given group of lists <durations lists>."

  (mapcar #'get-rotations my-list)

)
