;;;==================================
;;; U-UTILITIES
;;;==================================

(in-package :cnmat)

(defun info-get-pitches (chord-seq)

  (let ((final-pitches '())
        (mypitches (om::lmidic chord-seq)))

     (loop for pitch in mypitches do
      (cond 
           ((< (length pitch) 2) (push  pitch final-pitches))
           (t (push (list pitch) final-pitches))))

     (reverse (flat final-pitches 1))
     )
  )

(defun info-get-rhythms-and-tatums (rhythms)

 (let* ((output-rhythms '())
       (abs-beat '())
       (pre-output-tatums '())
       (output-tatums '()))

;get down to the internal beat subdivisions and list them as tatums

;takes us down to the list of measures
       (loop for group in (flat (rest rhythms) 1) do
             (loop for subdivision in (rest group) do
                   ;takes us down to each individual measure
                   (loop for beat in subdivision do
                         ;if it is a full beat grab the rhythms
                         (cond ((listp beat) (push (reverse (flat (cdr beat) 1)) output-rhythms))
                               ;otherwise, grab the rest and put it in list, too
                               (t (push (list beat) output-rhythms)))
                         ;put a sum into abs-beat for the absolute value duration number of subdivisions in a beat
                         (cond ((listp beat) (progn (push (mapcar #'abs (flat (cdr beat) 1)) abs-beat)
                                                   ;put the abs total for the beat into output tatums)
                                                    (push (reduce #'+  (flat abs-beat)) pre-output-tatums)))
                               ;it's a rest, so push 1 thing in
                               ;(t (push (abs beat) pre-output-tatums)))
                               (t (push  (repeat-n 1 (abs beat)) pre-output-tatums)))
                         ;reset abs-beat
                         (setf abs-beat '())
                         )))

       ;(print 'pre-output-tatums)
       ;(print pre-output-tatums)
       ;now convert the pre-output tatums list into a correct tatums list for output
       (loop for elem in (flat pre-output-tatums) do
             (cond ((= elem 1) (push '(1 (4)) output-tatums))  ;quarters
                   ((= elem 2) (push '(1 (8)) output-tatums))  ;eighths
                   ((= elem 3) (push '(1 (12)) output-tatums)) ;triplets
                   ((= elem 4) (push '(1 (16)) output-tatums)) ;sixteenths
                   ((= elem 5) (push '(1 (20)) output-tatums)) ;quintuplets
                   ((= elem 6) (push '(1 (24)) output-tatums)) ;sextuplets
                   ((= elem 6) (push '(1 (28)) output-tatums)) ;septuplets
                   ((= elem 8) (push '(1 (32)) output-tatums))
                   (t nil))) ;thirty-secods

       ;output the tatums
       (list (reverse (flat output-rhythms))  output-tatums)
  )

)

(om::defmethod! u-score-lists ( (my-thing voice))

  :icon 7
  :indoc '("a poly or a voice")
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice" "and another thing") 
  :initvals '(nil)
  :numouts 5
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

  (let ((rhythms '())
        (pitches '())
        (meters '())
        (tatums '())
        (output '())
        )

   ; get the rhythms & tatums
   ;(push (tree2ratio (om::tree my-thing)) rhythms)
   (push (first (info-get-rhythms-and-tatums (om::tree my-thing))) rhythms)
   (push (second (info-get-rhythms-and-tatums (om::tree my-thing))) tatums)

   ; get the durations
   ;(push (om/ (tree2ratio (om::tree my-thing)) this-tatum) durations)

   ;get pitch
   (push (info-get-pitches  (make-instance 'chord-seq
                                           :lmidic (om::chords my-thing))) pitches)

   (push (om::get-signatures (om::tree my-thing)) meters)

   ;send out everything plus a last element that is the number of rhythm elements
   (push (list (flat pitches 1) (flat rhythms 1) (list (length (flat rhythms 1)))) output)

   ;(flat output 1)
   (values (flat meters 1) pitches rhythms tatums (list (list (length (flat rhythms 1)))))


   )
)


(om::defmethod! u-score-lists ( (my-thing poly)  )

  :icon 7
  :indoc '("a poly or a voice" )
  :outdoc '("Returns a list of lists containing pitches, rhythms and no. items (pitches). Connect 'self' output of poly or voice") 
  :initvals '(nil)
  :numouts 5
  :doc "Returns a list of lists containing pitches, rhythms and no. items (pitches)"

 ; (let* ((stuff (mapcar (lambda (x) (u-info x)) (om::voices my-thing)))
 ;      (organized (mat-trans stuff)))
        
    ;(print 'stuff)
    ;(print stuff)

       ;(values (first organized) organized (third organized))

  (let* ((voices (om::voices my-thing))
         (rhythms '())
         (tatums '())
         (pitches '())
         (meters '())
         (num-elems '())
         (output '())
         )
    
    (print 'voices)
    (print voices)

   ;get rhythms
   (loop for voice in voices do
         (push (first (info-get-rhythms-and-tatums (om::tree voice))) rhythms)
         (push (second (info-get-rhythms-and-tatums (om::tree voice))) tatums))

(print 'rhythms)
(print rhythms)
   
   ;(loop for voice in voices do
   ;      (push (om/ (tree2ratio (om::tree voice)) this-tatum) durations))

   ;get pitch
   (loop for voice in voices do
         (push (info-get-pitches  (make-instance 'chord-seq
                                           :lmidic (om::chords voice))) pitches))

   (loop for voice in voices do
         (push (om::get-signatures (om::tree voice)) meters))

   ;send out everything plus a last element that is the number of rhythm elements
   (loop for rhythm in rhythms do
         (push (list (length (flat rhythm 1))) num-elems))

   ;(flat output 1)
   (values (reverse (flat meters 1)) (reverse pitches)  (reverse rhythms) (reverse tatums) (reverse num-elems))


   )
)


(om::defmethod! u-retro ( (mylist list) &optional (mode 0))

  :icon 7
  :indoc '("a list of lists" )
  :outdoc '("Returns the retrograde of every list within a list of lists" "Mode=1 returns the retrograde of every sublist in the list of lists") 
  :initvals '(nil)
  :doc "Returns the retrograde of every list within a list of lists (default). Mode=1 returns the retrograde of every sublist in the list of lists"

 
  (case mode

    (0

     (let ((outputlist '()))
       (loop for elem in mylist do
             (push (reverse elem) outputlist))
       (reverse outputlist))
     )


    (1

     (let ((outputlist '())
      (pre-outputlist '()))

       (loop for elem in mylist do
             (loop for sublist in elem do
                   (push (reverse sublist) pre-outputlist))
             (push (reverse pre-outputlist) outputlist)
             (setf pre-outputlist '()))
       (reverse outputlist))
    )
    )
)

(om::defmethod! u-+ ( (mylist list) (mynumber number) &optional (mode 0) (mod 12))

  :icon 7
  :indoc '("a list" "a number" "Optional mode argument" "Optional mod argument" )
  :outdoc '("U-+ is a utility for quick addition of a number to a lists of numbers.  Default mode is normal addition. Optional Mode=1 is mod12-based addition.  Optional mod=12 to change mod number.") 
  :initvals '((nil) (nil) (nil))
  :doc "U-+ is a utility for quick addition of a number to a lists of numbers.  Default mode is normal addition. Optional Mode=1 is mod12-based addition.  Optional mod=12 to change mod number."

  (case mode
  
  (0
   ;use regular om addition
   (om::om+ mylist mynumber)
   )

  (1
   ;use zn+ for mod 12
   (om::mod+ (om/ mylist 100) mynumber mod)
   )
  )

)


(om::defmethod! u-- ( (mylist list) (mynumber number) &optional (mode 0) (mod 12))

  :icon 7
  :indoc '("a list" "a number" "Optional mode argument" "Optional mod argument" )
  :outdoc '("U-- is a utility for quick subtraction of a number from a lists of numbers.  Default mode is normal subtraction. Optional Mode=1 is mod12-based subtraction.  Optional mod=12 to change mod number.") 
  :initvals '((nil) (nil) (nil))
  :doc "U-- is a utility for quick subtraction of a number from a lists of numbers.  Default mode is normal subtraction. Optional Mode=1 is mod12-based subtraction.  Optional mod=12 to change mod number."

  (case mode
  
  (0
   ;use regular om addition
   (om::om- mylist mynumber)
   )

  (1
   ;use zn+ for mod 12
   (om::mod- (om/ mylist 100) mynumber mod)
   )
  )

)


(om::defmethod! u-* ( (mylist list) (mynumber number) &optional (mode 0) (mod 12))

  :icon 7
  :indoc '("a list" "a number" "Optional mode argument" "Optional mod argument" )
  :outdoc '("U-* is a utility for quick multiplication of a number with a lists of numbers.  Default mode is normal multiplication. Optional Mode=1 is mod12-based multiplication.  Optional mod=12 to change mod number.") 
  :initvals '((nil) (nil) (nil))
  :doc "U-* is a utility for quick multiplication of a number with a lists of numbers.  Default mode is normal multiplication. Optional Mode=1 is mod12-based multiplication.  Optional mod=12 to change mod number."

  (case mode
  
  (0
   ;use regular om addition
   (om::om* mylist mynumber)
   )

  (1
   ;use zn+ for mod 12
   (om::mod* (om/ mylist 100) mynumber mod)
   )
  )

)



(om::defmethod! u-inversion ((pitchlist list) (inversion-element integer) &optional (mode 0) (mod 12))

  :icon 7
  :indoc '("a list of pitch lists" "a pitch to invert around")
  :outdoc '("Returns a list of lists of pitches inverted around a given pitch. With optional mode=1 toconvert to mod12. And with optional mod input to change mod12 to another number.") 
  :initvals '(((7000 7100 8000) (6000 6800 7000)) 7100)
  :doc "Returns a list of lists of elements from the source lists chosen by index number"


  (case mode
    
    (0
; see list-inversion and helper-inversion functions in o-pitch.lisp
     (list (mapcar (lambda (x) (list-inversion inversion-element x)) pitchlist))
     )

    (1
; see list-inversion and helper-inversion functions in o-pitch.lisp
;divide all numbers by 100 to bring 
     (om::mod* (om/ (mapcar (lambda (x) (list-inversion inversion-element x)) pitchlist) 100) 1 mod)
     )
  
    )
)

(om::defmethod! u-midic->pc ( (mylist list))

  :icon 7
  :indoc '("a list of lists" )
  :outdoc '("U-midics->mod12 takes in a list of list of midics and converts them to lists of pitch class sets")
  :initvals '((nil) (nil) (nil))
  :doc "U-midics->mod12 takes in a list of list of midics and converts them to lists of pitch class sets"

 ;assumes input is midics list of lists
(let ((outputlist '())
      (subelemlist '()))

    (loop for elem in mylist do
          (cond ((listp elem) 
                (progn (loop for subelem in elem do
                      (push  (om::mod+ (om/ subelem 100) 0 12) subelemlist))
                       (push (reverse subelemlist) outputlist)
                       (setf subelemlist '())))
                
                (t (push (om::mod+ (om/ elem 100) 0 12) outputlist))))

(reverse outputlist)
)
)



(om::defmethod! u-pc->midic ( (mylist list) (reference-pitch number))

  :icon 7
  :indoc '("a list of lists" "a reference pitch" )
  :outdoc '("U-pc->midic takes in a list of list of pcs and converts them to lists of midics using a reference pitch to set the register. The reference pitch should be a midic, ideally a C pitchclass. The output midics will align with the closest octave.")
  :initvals '((nil) (nil) (nil))
  :doc "U-pc->midic takes in a list of list of pcs and converts them to lists of midics using a reference pitch to set the register."

 ;assumes input is midics list of lists

(let ((c-list '(0 1200 2400 3600 4800 6000 7200 8400 9600 10800 12000 13600))
      (closest-c 000)
      (outputlist '())
      (pre-outputlist '())
      (subelemlist '()))

(loop for elem in c-list do
      (if (< (abs (- reference-pitch elem)) (abs (- reference-pitch closest-c)))
          (setf closest-c elem)))

;;must take in a list of lists

(loop for sublist in mylist do
      (loop for elem in sublist do
          (cond ((listp elem) 
                (progn (loop for subelem in elem do
                      (push (+ (* subelem 100) closest-c) subelemlist))
                       (push (reverse subelemlist) pre-outputlist)
                       (setf subelemlist '())))
                
                (t (push (+ (* elem 100) closest-c) pre-outputlist))))
          (push (reverse pre-outputlist) outputlist)
          (setf pre-outputlist '()))
          
(reverse outputlist)))
