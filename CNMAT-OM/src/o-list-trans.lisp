;;;==================================
;;; O-LIST-TRANS
;;;==================================

;;;REQUIRES ALEA LIBRARY TO WORK



(in-package :cnmat)

(defun bpf-probabilities (mybpf num-samples)

  (let* ((samples (om::bpf-sample mybpf 'nil 'nil num-samples))
        (listA-probabilities (om::om- 1.0 (om::om/ samples 100)))
        (listB-probabilities (om::om- 1.0 listA-probabilities))
        )
   
    (mat-trans (list listA-probabilities listB-probabilities))
    )
)



(om::defmethod! o-list-trans ((mybpf bpf) (num-samples number) (listA list) (listB list) &optional (mode 0))

  :icon 7
  :indoc '("a  bpf" "number of samples desired" "list A" "list B" "optional mode")
  :outdoc '("a sequence of random elements of n-length transitioning between the given lists") 
  :initvals '(nil 10 '(6000 6100 6200) '(7200 7300 7400) 0)
  :doc "Return a random element from listA or listB using a bpf to guide the the probability of which list is chosen from. Within each list, elem  ents have equal proability of being chosen."
 
(case mode 
  (0
   (let* ((bpf-probabilities (bpf-probabilities mybpf num-samples)))

         (loop for n from 1 to num-samples
               for lp in bpf-probabilities
               collect (alea::choixmultiple 
                     lp 
                     ;returns a random element with equal proability
                     ;of being chosen
                     (nth (random (length listA)) listA) 
                     (nth (random (length listB)) listB) 
                     ))
         )
   )

  (1

    (let* ((bpf-probabilities (bpf-probabilities mybpf num-samples)))

         (flat (loop for n from 1 to num-samples
               for lp in bpf-probabilities
               collect (alea::choixmultiple 
                     lp 
                     ;returns a random element with equal proability
                     ;of being chosen
                     (rand-from-list listA 1)
                     (rand-from-list listB 1)                     
                     )))
         )

   )

))
