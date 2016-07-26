;;;==================================
;;; P-BANDS-COLLECTION
;;;==================================

(in-package :cnmat)

(defun random-pitch-from-bands (lower-limit upper-limit)

;;return a random pitch from upper and lower bounds
;; add the random value returned to the lower of the two pitches 

(om+ (min lower-limit upper-limit) (om* (round (om/ (random (abs(- lower-limit upper-limit))) 100)) 100))

)


(defun scaled-bpf-samples (my-bpf attacks-list)

  (let* ((sum-of-attacks (reduce '+ attacks-list))
         (x-scaled-bpf (bpf-scale my-bpf :x1 0 :x2 sum-of-attacks))
         (attack-intervals (butlast (dx->x 0 attacks-list))))

    (mapcar (lambda (x) (x-transfer x-scaled-bpf x)) attack-intervals)

    )

)


(defun sample-from-bands (bpf-lib attacks-voice)



  (let 

    ;;for each bpf sample it for the number of attacks
    ;;this is where I need to change the code
    ;;if ever desired, revert to previous version by switching out the next two lines of code\

    ;;((bpf-samps (mapcar (lambda (x) (om::bpf-sample x nil nil (length attacks-voice))) bpf-lib)))
    ((bpf-samps (mapcar (lambda (x) (scaled-bpf-samples x attacks-voice)) bpf-lib)))

    (mapcar (lambda (x y) (random-pitch-from-bands x y)) (nth 0 bpf-samps) (nth 1 bpf-samps))
    )

)


(defun get-pitch-from-collection-by-voice (voice-pitches pitch-collection)

  (mapcar (lambda (x) (find-closest-pitch-from-collection x pitch-collection)) voice-pitches)

)

(defun find-closest-pitch-from-collection (pitch pitch-collection)

     (let* ((differences (mapcar (lambda (x y) (list (abs (- pitch x)) y)) pitch-collection pitch-collection))
      (smallest-difference (nth 0 differences)))

        (mapcar (lambda (x) (if (<= (nth 0 x) (nth 0 smallest-difference)) (setq smallest-difference x))) differences)

       (nth 1 smallest-difference)



     )


)

  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-bands-collection ( (bpf-lib list) (attacks-voices list) (pitch-collection list) &optional (mode 0))

  :icon 2
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" " a pitch collection from which to draw pitches" "mode: 0 = n/a")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil ((1 2 3 4)(2 3 4 5))  (6000 6600 7200 7500))
  :doc "Returns random pitch samples from a collection provided. The pitches sampled are determed following the contour of the bpf pitch bands provided "
  
  (case mode

    (0 (let ((prelim-sample-pitches (mapcar (lambda (x) (sample-from-bands bpf-lib x))  attacks-voices)))
         
         (mapcar (lambda (x) (get-pitch-from-collection-by-voice x (flat pitch-collection))) prelim-sample-pitches)
         )
       )
   ;;; (1 (let ((resultant-pitches (mapcar (lambda (x) (random-from-set-pitch-map x mapping-list)) durations-list)))
         
       ;;  (check-pitch-map durations-list resultant-pitches)
        ;; )
       )


)


;;;this receives from rfi object
(om::defmethod! p-map1 ( (durations-list prf) (mapping-list list) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (pitch-mapper1 durations mapping-list mode)))

;;;