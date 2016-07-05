;;;==================================
;;; COMBINE-RHYTHM-LISTS
;;;==================================

(in-package :cnmat)

(defun rhythm-append (lista listb)
  
  ;;;or simply use x-append here

  (flat (list lista listb))

)


(om::defmethod! combine-rhythm-lists ((my-list list) &rest rest-list)

  :icon 6
  :indoc '("a list of lists" )
  :outdoc '("returns rhythm lists combined by voice") 
  :initvals '(((1 2 3 4) (5 6 7 8)) (nil))
  :doc "This object appends rhythm lists across an arbitrary number of voices and converts them into one (long) list of lists by voice. Add as many inputs to the combine-rhythm-lists object as desired for lists. (Use shift-alt right-arrow to add inputs and shift-alt left-arrow to subtract inputs). Requires formatting rhythms by voice as lists of lists. Each list of lists must be the same length (i.e. same number of voices)."

  (let ((big-list (mat-trans   (x-append (list my-list) rest-list))))

    (mapcar (lambda (x) (reduce 'rhythm-append x)) big-list)
  ) 

)