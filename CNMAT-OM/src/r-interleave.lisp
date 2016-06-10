;;;==================================
;;; RHYTHMIC-FRAME-INTERLEAVE
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function


(defun r-interleave (rhythm-parent  rhythm-child)

   (let ((myvoices (mapcar (lambda (x y) (list x y)) (voices rhythm-parent) (voices rhythm-child))))

      (make-instance 
          'prf 
          :voices (flat myvoices)
      )


   )


)