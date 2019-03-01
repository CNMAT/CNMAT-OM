;;;==================================
;;; An OM library with CNMAT tools
;;;==================================

(defpackage "CNMAT"
  (:use "COMMON-LISP" "OM")
  (:export 

   :make-spat-room

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
   :o-list-info
   :o-list-info2
   :o-tatum-maker

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
   :q-remove-rotations

   :s-poly
   :s-voice
   :s-poly2
   :s-cuts
   :s-cuts2
   :s-poly->voice
   :s-combine-voices
   :s-combine-polys
   
   :prf

   :u-info
   :u-score-lists
   :u-retro
   :u-+
   :u--
   :u-*
   :u-inversion
   :u-midic->pc
   :u-flat-by-voice
   :u-pc-remap
   :u-divisors
   :u-list-info
   :u-tatum-format
   :u-list-duplicator
 
   ))


(require-library "OMAlea")

(in-package :om)


(mapc #'(lambda (file) 
          (compile&load (om-relative-path '("src" "tessellate") file)))
      '("rhythmic-frames"
        "prf-editor"
        "combinations"
        "o-operations"
        "p-pitch"
        "r-rhythm"
        "q-queries"
        "s-score"
        "s-combine-voices"
        "u-utilities"))



(set-lib-release 0.1 (om::find-library "CNMAT-OM"))

(doc-library "CNMAT-OM contains compositional tools collected from CNMAT researh and projects.")

(om-print "
;;;================================================
;;; CNMAT-OM
;;; Center for New Music and Audio Technologies
;;; Copyright (c) 2017 Regents of the University of California. 
;;; All rights reserved.
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
 '((NIL 
    (("Tessellate" 

     (("database" nil nil (q-combi q-combi-filter q-canon q-rotations  q-permute+remove-dup-rotations q-n-permutations-no-rotations q-permutations+canon q-permutations+canon-utility q-permutations+canon-all q-rotation-canon? q-canon-permutations-no-overlaps q-combi-from-elements q-random-permutations-no-dups q-remove-rotations ) nil)

      ("operations" nil nil ( o-operations o-list-trans o-index o-count-trunc o-equal-prob o-index o-list-rand o-list-repeat o-sum-lists  o-list-rand o-list-info) nil)

      ("pitch" nil nil (p-map1 p-inversion p-map2 p-make-bands p-bands-collection p-bands-pitchclass p-bands-register p-harmony-from-analysis) nil)

      ("rhythm" nil (prf) (prf rhythmic-frames r-diminutions r-interleave r-duration-list r-merge r-combine-lists r-retro-canon? r-substitute r-substitute-children r-tatum-mapping r-scatter-attacks r-retro-canon?) nil)
      
      ("score" () nil (s-poly s-poly->voice s-poly2 s-cuts s-cuts2 s-combine-voices s-combine-polys) nil)

      ("utilities" nil nil ( u-info u-list-info u-score-lists u-retro-canon? u-+ u-- u-* u-inversion u-midic->pc u-flat-by-voice u-pc-remap u-divisors u-tatum-format u-list-duplicator) nil))
     nil ;; classes
     (combinations) ;; functions
     nil))
    NIL NIL NIL)
   
   ;;use lines below for sub-sub menus, if needed
   ;;("queries-database" (("subpack1" nil (prf) (get-combi get-rotations canon-query sum-lists get-index) nil)
   ;;         )
   ;; nil nil nil)
   )
 (om::find-library "CNMAT-OM")
 )

;;; evaluate this to generate the HTML reference
;;;(gen-lib-reference (find-library "CNMAT-OM"))