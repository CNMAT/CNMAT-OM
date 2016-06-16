;;;==================================
;;; MAKE-BANDS
;;;==================================

(in-package :cnmat)

(defun make-bpfs ( my-chord-seq separate-into-two-pitch-lists)

  (list
       (om::om-make-bpf 'bpf 
                       (butlast (om::lonset my-chord-seq)) ;;;x
                       (nth 0 separate-into-two-pitch-lists) ;;;y
                       2 ;;;number of decimals
                      )

        (om::om-make-bpf 'bpf 
                       (butlast (om::lonset my-chord-seq)) ;;;x
                       (nth 1 separate-into-two-pitch-lists) ;;;y
                       2 ;;;number of decimals
                      )
    )
)

(om::defmethod! make-bands ((my-chord-seq chord-seq) &optional (mode 0))
  :icon 7
  :indoc '("a chord-seq" "mode: 0 = n/a")
  :initvals '((nil) 0)
  :doc "Returns a bpf lib for the pitch band to be sampled. Describes a band of pitches. Takes in a chord-seq with  three of more dyads. Returns a bpf lib describing a pitch band that can be sample across a series of attacks. Make-bands should be used in conjunction with objects pitch->bands-pitchclass or pitch->bands-collection objects. See examples: 5b pitch->bands-collection or 5c pitch-bands->pitchclass"

  (case mode 
    ;;;sum of the lists of lists per voice
    (0 
     
        (let* ((sorted-pitch-lists (mapcar (lambda (x) (sort x  '<)) (om::lmidic my-chord-seq)))
               (separate-into-two-pitch-lists (mat-trans sorted-pitch-lists)))
              
               
               (om::make-instance 'bpf-lib 
                       :bpf-list (make-bpfs my-chord-seq separate-into-two-pitch-lists) ;;;bpf-lists
                      )
           
          )
     )

    )
)
