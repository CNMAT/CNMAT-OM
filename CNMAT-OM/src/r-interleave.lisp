;;;==================================
;;; RHYTHMIC-FRAME-INTERLEAVE
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function
(defmethod r-interleave (rhythm-parent   &rest rest )
  :initvals '(nil) 
  :indoc '("first element" "second element" "additional elements")
  :icon 1
  :doc "Interleaves rhythmic frames."
  (let* ((my-stuff (mapcar (lambda (x) (list x)) rest))
        (all-stuff (flat (mat-trans (cons rhythm-parent rest)))))
  
  ;(print 'all-stuff)
  ;this print call below is necessary--dont delete!
  (print all-stuff)




  )
)




(defmethod r-interleave2 (rhythm-parent &rest rest)
  :initvals '(nil) 
  :indoc '("first element" "second element" "additional elements")
  :icon 1
  :doc "Interleaves rhythmic frames."
  (loop for group in (mat-trans (cons rhythm-parent rest))
        collect (make-instance 'prf :voices group))
  )