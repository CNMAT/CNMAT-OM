;;;==================================
;;; P-INVERSION
;;;==================================

(in-package :cnmat)



(defun list-inversion (inversion-element elements-list)

 (mapcar (lambda (x) (helper-inversion inversion-element x)) (flat elements-list))

)


(defun helper-inversion (inversion element)

  (cond ((> element inversion) (- inversion  (- element inversion) ))
      (t (+ (- inversion element) inversion)))
)




(om::defmethod! p-inversion ((pitchlist list) (inversion-element integer))

  :icon 7
  :indoc '("a list of pitch lists" "a pitch to invert around")
  :outdoc '("Returns a list of lists of pitches inverted around a given pitch") 
  :initvals '(((7000 7100 8000) (6000 6800 7000)) 7100)
  :doc "Returns a list of lists of elements from the source lists chosen by index number"

  
  (list (mapcar (lambda (x) (list-inversion inversion-element x)) pitchlist))

  
)