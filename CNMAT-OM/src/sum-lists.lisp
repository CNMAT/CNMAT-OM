;;;==================================
;;; SUM-LISTS
;;;==================================

(in-package :cnmat)

;;;map the lists when given list of lists to maintain
(defun map-lists (my-list)
   (list (mapcar #' reduce-sum-as-list my-list)))

;;;helper function for sum-lists that returns as a list each time
(defun reduce-sum-as-list (list)
  (list (reduce #' + (flat list))))

;;;helper function for sum-lists
(defun reduce-sum (list)
  (reduce #' + (flat list)))

;;;(defun sum-of-lists (list)
;;;  (mapcar #'reduce-sum list))

;;; output sums of lists of lists
(om::defmethod! sum-lists ((my-list list) &optional mode)
  :icon 5
  :indoc '("a list of lists to be summed")
  :initvals '((1 2 3) (4 5 6))
  :menuins '((1 (("sum of the list of lists per voice" 0) ("sums that preserve list structure" 1))))
  :doc "Returns a list of sums of list arguments"

  (case mode 
    ;;;sum of the lists of lists per voice
    (0 (mapcar #'reduce-sum my-list))
    ;;;sum that preserves the list structure
    (1  (flat (mapcar #'map-lists my-list) 1))
    )
)
