;;;==================================
;;; P-MAP2
;;;==================================

(in-package :cnmat)

;;;mapping pitches onto the rhythmic lists in three different ways


(defun get-random-pitch (pitch-collection durations-list)
  (loop for attack in durations-list
        collect (nth (random (length pitch-collection)) pitch-collection)
      )


)

(defun get-random-pitch-from-range (pitch-collection durations-list)
  (loop for attack in durations-list
        collect (nth 
                             (random (length (arithm-ser (car pitch-collection) (car (cdr pitch-collection)) 100))) 
                                 (arithm-ser (car pitch-collection) (car (cdr pitch-collection)) 100)))
     
)
  
;;;this receieves list of durations lists (output from get-rotations)
(om::defmethod! p-map2 ( (durations-list list) (pitch-collection list) &optional (mode 0))

  :icon 2
  :indoc '("a list of rhythm lists (list of lists)" "a list (or list of lists) for pitch collections" "mode: 0 = random choice from a pitch collection; 1 = random choice from a range of pitches")
  :outdoc '("a list of pitch mappings to use with") 
  :initvals '( ((3 4 5) (4 5 3) (5 3 4)) (7200 7300 7500 7700)  0)
  :doc "Pitch mapping for each attack in each voice using random choice from a provided pitch collection.  In mode 1, random pitch selection from individual pitch collections, one for each voice.  In this case, provide a list of lists for wach voice pitch collection"
  
  (case mode

    (0 
             (let ((resultant-pitches (mapcar (lambda (x) (rand-from-list pitch-collection (length x))) durations-list)))
             (flat resultant-pitches 1)
             )
     )
          
     
(2 

         (cond ((listp (car pitch-collection))
               ;;this is the version when there are a list of list of pitch-collections
               (let ((resultant-pitches (mapcar (lambda (x y) (get-random-pitch-from-range x y)) pitch-collection durations-list)))
             resultant-pitches
             ))
             (t ;this is the version when there is just one shared pitch collection
             (let ((resultant-pitches (mapcar (lambda (x) (get-random-pitch-from-range pitch-collection x)) durations-list)))
             resultant-pitches
             ))
          )
     )

(1

         (cond ((listp (car pitch-collection))
               ;;this is the version when there are a list of list of pitch-collections
               (let* ( (length-durations (mapcar (lambda (x) (length x)) durations-list))
                       (resultant-pitches (mapcar (lambda (x y) (rand-from-list x y))  pitch-collection length-durations )))

                      ;(resultant-pitches (mapcar (lambda (x y) (rand-from-list x y))  pitch-collection flat-length-durations )))
             ;(print 'got-here)
             ;(print (flat resultant-pitches 1))
             (flat resultant-pitches 1)
             ))
             (t nil
             )
          )
     )

  ;;  (2 (let ((resultant-pitches (mapcar (lambda (x) (random-from-range-pitch-map x mapping-list)) durations-list)))
         
    ;;     (check-pitch-map durations-list resultant-pitches)
    ;;     )
   ;;    )
    )

)


;;;this receives from rfi object
(om::defmethod! p-map1 ( (durations-list prf) (mapping-list list) &optional (mode 0))
  (let ((durations (mapcar 'pulses (voices durations-list))))
    (pitch-mapper1 durations mapping-list mode)))

;;;