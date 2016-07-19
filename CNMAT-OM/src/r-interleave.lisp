;;;==================================
;;; RHYTHMIC-FRAME-INTERLEAVE
;;;==================================

(in-package :cnmat)

;;;create all of the rotations of a rhythm
;;;uses get-rotations method/function


(defun r-interleave (rhythm-parent  rhythm-child)
  (let ((myvoices (make-instance 
                   'prf :voices (flat 
                                 (mapcar 'list 
                                              (voices rhythm-parent) (voices rhythm-child))))))
    ;(setf (elements-per-voice myvoices) 2)
    myvoices))
    