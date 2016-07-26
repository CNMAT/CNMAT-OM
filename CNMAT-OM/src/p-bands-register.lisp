;;;==================================
;;; P-BANDS-REGISTER
;;;==================================

(in-package :cnmat)

(defun scaled-bpf-samples-registration (my-bpf pitches)

  ;;(print "aaah!!!")
  (let ((hold-samples (om::bpf-sample my-bpf 'nil 'nil (length pitches))))
    ;round them
    (om::om* (om::om-round (om::om/ hold-samples 100.00)) 100)

    )

)


(defun scaled-bpf-samples (my-bpf attacks-list)

  (let* ((sum-of-attacks (reduce '+ attacks-list))
         (x-scaled-bpf (bpf-scale my-bpf :x1 0 :x2 sum-of-attacks))
         (attack-intervals (butlast (dx->x 0 attacks-list))))

    (mapcar (lambda (x) (x-transfer x-scaled-bpf x)) attack-intervals)

    )

)



(defun get-registrated-pitches-useless (pitch)
  ;make series below and above by octave to add to note
  ;this function could be more precise....
  (let* ((series (om::om* (arithm-ser 0 20 1) 1200))
         (hi-pitches (om::om+ series pitch))
         ;;dont include given pitch this time
         (low-pitches (om::om- series (- pitch 1200))
         (all-pitches (append hi-pitches low-pitches)))

  all-pitches
  ))
)

(defun get-registrated-pitches (pitch)
  ;make series below and above by octave to add to note
  ;this function could be more precise....
  (let* (
         (hi-pitches (arithm-ser pitch 12800 1200))
         (low-pitches (arithm-ser (- pitch 1200) 100 -1200))
         (all-pitches (append hi-pitches low-pitches)))

  all-pitches
  )
)


(defun get-possible-registrated-pitches (organized-bpf-samples all-possible-pitches)

;;collect any registration of the original pitch
;;if it falls within the sampled bpfband
( loop for elem in all-possible-pitches
       collect (if (and (>= elem (nth 0 organized-bpf-samples))
                        (<= elem (nth 1 organized-bpf-samples))) elem))

)




  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-bands-register ( (bpf-lib list) (pitches list) &optional (mode 0))

  :icon 2
  :indoc '("a bpf-lib of two bpfs" "a list of list of attacks, in rotations or other specification" "mode: 0 = n/a")
  :outdoc '("a list of pitces mapped from the bpf to the pitch-collection") 
  :initvals '( nil '(6000 6000 6000 6000)  0)
  :doc "Returns random pitch samples that conforms to a list of allowable pitchclasses. The pitches sampled are determed following the contour of the bpf pitch bands provided "
  
  (case mode

    (0 
        (let* ((bpf-samples (mapcar (lambda (x) (scaled-bpf-samples-registration x pitches)) bpf-lib))
               ;;put these bpf samples into high-low pairs
              (organized-bpf-samples (mat-trans bpf-samples))
              ;;now get all of the possibly relevant pitches
              (all-possible-pitches (mapcar (lambda (x) (get-registrated-pitches x )) pitches))
              (pitches-fitting-registration (mapcar (lambda (x y) (get-possible-registrated-pitches x y)) organized-bpf-samples all-possible-pitches))
              (right-pitches-no-nils (mapcar (lambda (x) (remove nil x)) pitches-fitting-registration)))

          ;;now return a random pitch from the pool of possible pitches for each pitch 
          ;;originally provided in the pitches list

          ;(print all-possible-pitches)
          (print pitches-fitting-registration)
          (print 'got-here)
          (print right-pitches-no-nils)


          
          (mapcar (lambda (x)  (nth (om-random 0 (- (length x) 1)) x)) right-pitches-no-nils)
          )

          )

     

    )


)

(om-random 0 9)

