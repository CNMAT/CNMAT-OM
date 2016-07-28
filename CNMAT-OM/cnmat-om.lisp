;;;==================================
;;; An OM library with CNMAT tools
;;;==================================

(defpackage "CNMAT"
  (:use "COMMON-LISP" "OM")
  (:export 

;methods

   :combinations
   :rand-from-list
   :get-rotations
   :rotations->poly2
  :pitch->bands-collection
 


   :o-sum-lists
   :o-equal-prob
   :o-cribs
   :o-count-trunc
   :o-list-repeat
   :o-index
   :o-list-rand
   :o-list-trans

   :p-inversion
   :p-map1
   :p-map2
   :p-make-bands
   :p-bands-collection
   :p-bands-pitchclass
   :p-bands-register

   :r-retrograde-canon?
   :r-combine-lists
   :r-interleave
   :r-diminutions

   :q-combi
   :q-canon
   :q-permute+remove-dup-rotations
   :q-rotations
   :q-n-permutations-no-rotations
   :q-permutations+canon
   :q-permutations+canon-utility
   :q-permutations+canon-all
   :q-rotations-canon?
   :q-canon-permutations-no-overlaps

   :s-poly
   :s-voice
   :s-poly2
   :s-cuts
   :s-cuts2
   :s-poly->voice
   :s-combine-voices
   :s-combine-polys






   ))


(require-library "OMAlea")

(in-package :om)


;files?


(compile&load (om-relative-path '("src") "rhythmic-frames"))
(compile&load (om-relative-path '("src") "prf-editor"))
(compile&load (om-relative-path '("src") "combinations"))
(compile&load (om-relative-path '("src") "rand-from-list"))
;(compile&load (om-relative-path '("src") "make-bands"))
(compile&load (om-relative-path '("src") "rotations->poly2"))
(compile&load (om-relative-path '("src") "pitch->bands-collection"))
;(compile&load (om-relative-path '("src") "combine-polys"))



(compile&load (om-relative-path '("src") "o-count-trunc"))
(compile&load (om-relative-path '("src") "o-equal-prob"))
(compile&load (om-relative-path '("src") "o-sum-lists"))
(compile&load (om-relative-path '("src") "o-cribs"))
(compile&load (om-relative-path '("src") "o-list-repeat"))
(compile&load (om-relative-path '("src") "o-index"))
(compile&load (om-relative-path '("src") "o-list-rand"))
(compile&load (om-relative-path '("src") "o-list-trans"))


(compile&load (om-relative-path '("src") "p-inversion"))
(compile&load (om-relative-path '("src") "p-map1"))
(compile&load (om-relative-path '("src") "p-map2"))
(compile&load (om-relative-path '("src") "p-make-bands"))
(compile&load (om-relative-path '("src") "p-bands-collection"))
(compile&load (om-relative-path '("src") "p-bands-pitchclass"))
(compile&load (om-relative-path '("src") "p-bands-register"))

(compile&load (om-relative-path '("src") "r-diminutions"))
(compile&load (om-relative-path '("src") "r-interleave"))
(compile&load (om-relative-path '("src") "r-combine-lists"))
(compile&load (om-relative-path '("src") "r-retro-canon?"))

(compile&load (om-relative-path '("src") "q-combinations"))
(compile&load (om-relative-path '("src") "q-permutations+canon"))
(compile&load (om-relative-path '("src") "q-permutations+canon-utility"))
(compile&load (om-relative-path '("src") "q-permutations+canon-all"))
;(compile&load (om-relative-path '("src") "q-rotation-canon?"))
(compile&load (om-relative-path '("src") "q-canon-permutations-no-overlaps"))
(compile&load (om-relative-path '("src") "q-permute+remove-dup-rotations"))
(compile&load (om-relative-path '("src") "q-n-permutations-no-rotations"))


(compile&load (om-relative-path '("src") "s-poly"))
(compile&load (om-relative-path '("src") "s-voice"))
(compile&load (om-relative-path '("src") "s-poly2"))
(compile&load (om-relative-path '("src") "s-cuts"))
(compile&load (om-relative-path '("src") "s-cuts2"))
(compile&load (om-relative-path '("src") "s-poly->voice"))
(compile&load (om-relative-path '("src") "s-combine-voices"))
(compile&load (om-relative-path '("src") "s-combine-polys"))





(set-lib-release 0.1 (om::find-library "CNMAT-OM"))

(doc-library "CNMAT-OM contains compositional tools collected from CNMAT researh and projects.")

(om-print "
;;;================================================
;;; CNMAT-OM
;;; (c) Center for New Music and Audio Technologies
;;; University of California, Berkeley
;;;================================================
")


;;;==================================
;;; LIB INTERFACE (MENU ITEMS ETC.)
;;;==================================

(in-package :cnmat)

;(let ((this-lib (om::find-library "CNMAT-OM"))
;  (om::addgenfun2pack '(get-combi get-rotations canon-query sum-lists get-index) this-lib))

; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
(om::fill-library  
 '((NIL nil (prf-editor) (combinations rand-from-list) nil)
   ("queries-database" nil nil (q-combi q-canon q-rotations  q-permute+remove-dup-rotations q-n-permutations-no-rotations q-permutations+canon q-permutations+canon-utility q-permutations+canon-all q-rotation-canon? q-canon-permutations-no-overlaps) nil)
   ("pitch utilities" nil nil (p-map1 p-inversion p-map2 p-make-bands p-bands-collection p-bands-pitchclass p-bands-register ) nil)
   ("operations" nil nil ( o-list-trans o-sum-lists o-index o-cribs o-equal-prob o-list-repeat o-list-rand o-list-trans) nil)
   ("rhythm utilities" nil nil ( r-diminutions r-interleave r-combine-lists r-retro-canon? rhythmic-frames) nil)

   ("score utilities" nil nil (s-poly s-voice s-poly2 s-cuts s-cuts2 s-poly s-combine-voices s-combine-polys rotations->poly2) nil)

;   pitch->bands-collection
   ;;use lines below for sub-sub menus, if needed
   ;;("queries-database" (("subpack1" nil (prf) (get-combi get-rotations canon-query sum-lists get-index) nil)
   ;;         )
   ;; nil nil nil)
   )
 (om::find-library "CNMAT-OM")
 )

;;; evaluate this to generate the HTML reference
;;; (gen-lib-reference (find-library "CNMAT-OM"))