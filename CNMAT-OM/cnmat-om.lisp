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
 

   :o-operations
   :o-list-trans
   :o-index
   :o-count-trunc
   :o-cribs
   :o-equal-prob
   :o-list-rand
   :o-list-repeat
   :o-sum-lists


   :p-inversion
   :p-map1
   :p-map2
   :p-make-bands
   :p-bands-collection
   :p-bands-pitchclass
   :p-bands-register
   :p-harmony-from-analysis

   :r-scatter-attacks
   :r-retrograde-canon?
   :r-combine-lists
   :r-interleave
   :r-diminutions

   :q-combi
   :q-combi-filter
   :q-canon
   :q-permute+remove-dup-rotations
   :q-rotations
   :q-n-permutations-no-rotations
   :q-permutations+canon
   :q-permutations+canon-utility
   :q-permutations+canon-all
   :q-rotations-canon?
   :q-canon-permutations-no-overlaps
   :q-combi-from-elements
   :q-random-permutations-no-dups

   :s-poly
   :s-voice
   :s-poly2
   :s-cuts
   :s-cuts2
   :s-poly->voice
   :s-combine-voices
   :s-combine-polys
   
   :prf





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
;(compile&load (om-relative-path '("src") "combine-polys"))

(compile&load (om-relative-path '("src") "o-operations"))
(compile&load (om-relative-path '("src") "o-list-trans"))

(compile&load (om-relative-path '("src") "p-pitch"))

(compile&load (om-relative-path '("src") "r-rhythm"))

(compile&load (om-relative-path '("src") "q-queries"))
(compile&load (om-relative-path '("src") "q-permutations+canon-all"))

(compile&load (om-relative-path '("src") "s-score"))
(compile&load (om-relative-path '("src") "s-combine-voices"))





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
 '((NIL nil (prf-editor) (combinations ) nil)
   ("queries-database" nil nil (q-combi q-combi-filter q-canon q-rotations  q-permute+remove-dup-rotations q-n-permutations-no-rotations q-permutations+canon q-permutations+canon-utility q-permutations+canon-all q-rotation-canon? q-canon-permutations-no-overlaps q-combi-from-elements q-random-permutations-no-dups) nil)
   ("pitch utilities" nil nil (p-map1 p-inversion p-map2 p-make-bands p-bands-collection p-bands-pitchclass p-bands-register p-harmony-from-analysis) nil)
   ("operations" nil nil ( o-operations o-list-trans o-index o-count-trunc o-cribs o-equal-prob o-index o-list-rand o-list-repeat o-sum-lists o-list-trans rand-from-list) nil)
   ("rhythm utilities" nil nil ( prf r-scatter-attacks r-diminutions r-interleave r-combine-lists r-retro-canon? rhythmic-frames) nil)

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