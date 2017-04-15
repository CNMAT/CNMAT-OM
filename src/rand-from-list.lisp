;;;==================================
;;; RAND-FROM-LIST
;;;==================================

(in-package :cnmat)


(defun sort-by-first-elem (elem)
    (car (car elem))
)



(om::defmethod! rand-from-list ((mylist list) (num-return-vals number))

  :icon 7
  :indoc '("a list with or without random weights" "desired length of output sequence")
  :outdoc '("a sequence of random elements of n-length from a list") 
  :initvals '((7100 (7200 0.9) 7300 7400 7800) 20)
  :doc "Returns a sequence of random elements of n-length from a list. Provide probability weights as desired for any element in the list.Format as follows (element probability) where the sum of the probabilities is < 1.0. Any elements without designated p robabilities will be assigned equal probabilities from the remainder, adding to 1.0"
  
  (let* ((built-weights (build-random-weights mylist))
         (output-list '())
         (final-list '()))
 
      (loop for x from 1 to num-return-vals do
         ( push (build-sequence built-weights) output-list))
   
      (reverse output-list)

      (loop for elem in output-list do
         (if (eq 'nil (cdr elem)) (push (car elem) final-list) (push  elem final-list) ))

      (list (reverse final-list)))

)


