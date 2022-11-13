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


(require-library "Alea")

