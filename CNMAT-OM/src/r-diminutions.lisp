;;;==================================
;;; R-DIMINUTIONS
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function

(defun substitute-processing (mylist)

(let ((final-list '())
      (collection '()))

  ;we just want the substitute vales to appear in the final lists
  ;if it is a list then just add it to final-list
  ;otherwise put it in collection and aoutput it
  ; as a rest when we need it
  
  (loop for elem in mylist do
      (if (listp elem)
          (progn (push (reduce #'+ collection) final-list)
                 (setq collection '())
                 (push (reverse elem) final-list))
        
          (push (* -1 elem) collection)))

  ;get any leftover elements
  (push collection final-list)

  ;output the list in the right order
   (remove 0 (remove 'nil (reverse (flat final-list))))

 )

)




(defmethod! r-diminutions ((rhythm rhythmic-frame) val subs) 
  (let ((substitutions (substitute subs val (pulses rhythm))))

   'rhythmic-frame
   :pulses  (substitute-processing substitutions))
)
  


(defmethod! r-diminutions ((rhythm polyrhythmic-frame) val subs) 
   (make-instance 
    'prf 
    :voices (mapcar #'(lambda (r) (r-diminutions r val subs)) (voices rhythm))
    ))