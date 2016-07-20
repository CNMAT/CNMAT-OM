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
  
  (print 'all-stuff)

  (print all-stuff)

 ; (make-instance 'prf 
  ;               :voices all-stuff)


  )
)

;;(defun r-interleave (rhythm-parent  rhythm-child &optional &rest rest)
;;  (let ((myvoices (make-instance 
;;                   'prf :voices (flat 
;;                              (mapcar 'list 
;;                                              (voices rhythm-parent) (voices rhythm-child) (voices rest))))))



    ;(setf (elements-per-voice myvoices) 2)
 ;;   myvoices))
    


