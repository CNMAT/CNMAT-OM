;;;==================================
;;; PITCH->BANDS-PITCHCLASS
;;;==================================

(in-package :cnmat)


(defun get-pitchclasses-helper (pitch-samples)

     
      (mapcar (lambda (x) (* (round (/ x 100)) 100)) pitch-samples)  

)

(defun get-pitchclasses-helper2 (rounded-results)

       (mapcar (lambda (x) (list (rem (/ x 100) 12) x)) rounded-results)

)

(defun get-pitchclasses (pitch-samples)
     
   (let ((rounded-results (mapcar (lambda (x) (get-pitchclasses-helper x)) pitch-samples)))


         (mapcar (lambda (x) (get-pitchclasses-helper2 x)) rounded-results)

         )

)


(defun get-allowable-pitches-helper (pitch-and-pitchclasses allowable-pitchclasses)

  (loop for elem in pitch-and-pitchclasses
           if (member (nth 0 elem) allowable-pitchclasses)
           return elem)

)


(defun check-pitches (allowable-pitchclasses pitch-samples)
  (let* ((pitch-and-pitchclasses (get-pitchclasses pitch-samples))
       (list-of-allowable-pitches (mapcar (lambda (x) (get-allowable-pitches-helper x allowable-pitchclasses))pitch-and-pitchclasses)))

    (mapcar (lambda (x) (nth 1 x)) list-of-allowable-pitches)

  )

)

(defun get-sample-from-bpfs (lower-limit upper-limit)

(om+ (min lower-limit upper-limit) (om* (round (om/ (random (abs(- lower-limit upper-limit))) 100)) 100))

)

(defun random-pitch-from-bands-pitchclass (lower-limit upper-limit)

;;return a random pitch from upper and lower bounds
;; add the random value returned to the lower of the two pitches 
;;return 100 choices for each pitch to try and get one that conforms to pitchclass requirements

;;not-used--(repeat-n (get-sample-from-bpfs lower-limit upper-limit) 100)

     (loop for i from 1 to 100
           collect (get-sample-from-bpfs lower-limit upper-limit))

)



(defun sample-from-bands-pitchclass (bpf-lib attacks-voice)

;;for each bpf sample it for the number of attacks
  (let ((bpf-samps (mapcar (lambda (x) (om::bpf-sample x nil nil (length attacks-voice))) bpf-lib)))
   
    (mapcar (lambda (x y) (random-pitch-from-bands-pitchclass x y)) (nth 0 bpf-samps) (nth 1 bpf-samps))
    )

)



  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! pitch->bands-pitchclass ( (bpf-lib list) (attacks-voices list) (allowable-pitchclasses list) &optional (mode 0))

  :icon 2
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" " a collection of allowable pitchclasses" "mode: 0 = n/a")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil ((1 2 3 4)(2 3 4 5))  (0 1 5))
  :doc "Returns random pitch samples that conforms to a list of allowable pitchclasses. The pitches sampled are determed following the contour of the bpf pitch bands provided "
  
  (case mode

    (0 (let* ((bpf-pitch-samples (mapcar (lambda (x) (sample-from-bands-pitchclass bpf-lib x))  attacks-voices))
              (checked-bpf-pitch-samples (mapcar (lambda (x) (check-pitches allowable-pitchclasses x)) bpf-pitch-samples)))
         
        ;; (print bpf-pitch-samples)
         checked-bpf-pitch-samples
         
         )
       )
   ;;; (1 (let ((resultant-pitches (mapcar (lambda (x) (random-from-set-pitch-map x mapping-list)) durations-list)))
         
       ;;  (check-pitch-map durations-list resultant-pitches)
        ;; )
      )


)


;;;this receives from rfi object

;;THIS IS NOT DONE--DO THIS!!! OR TAKE IT OUT
(om::defmethod! pitch-mapper1 ( (durations-list prf) (mapping-list list) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (pitch-mapper1 durations mapping-list mode)))

;;;