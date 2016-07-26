;;;==================================
;;; S-COMBINE-POLYS
;;;==================================

(in-package :cnmat)



(om::defmethod! s-combine-polys ((poly1 poly) (poly2 poly))

  :icon 7
  :indoc '("a poly object" "a second poly object")
  :outdoc '("Joins (concatenates) one poly to another. Outputs a poly object.") 
  :initvals '('(nil) '(nil))
  :doc "Joins one poly to another. Use this when the concat object won't work, i.e. when segments of music don't end tidly at the end of a bar. Joins voices in the poly according to these rules: If the last rhythm is a rest then this last rest is deleted and the new voice is joined snug with the last pitch.Takes tempo and legato from the first voice. Outputs a poly object."

  (let* ((combined-voices (mapcar (lambda (x y) (combine-voices x y)) (om::voices poly1) (om::voices poly2))))

  (make-instance 'poly
                 :voices combined-voices)
  )

     ;;(mapcar (lambda (x y) (combine-voices x y)) (om::voices poly1) (om::voices poly2))


)