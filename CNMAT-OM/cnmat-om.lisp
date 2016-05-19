;;;==================================
;;; An OM library with CNMAT tools
;;;==================================

(defpackage "CNMAT"
  (:use "COMMON-LISP" "OM")
  (:export 
   :get-combi 
   :get-rotations
   :canon-query 
   :rotation-canon-query
   :sum-lists
   :get-index
   :count-truncate
   :cribs
   :equal-probability
   :pitch-inversion
   :repeat-list
   :retrograde-list
   :rotations->poly
   :rotations->voice))

(in-package :om)

(compile&load (om-relative-path '("src") "rhythmic-frames"))
(compile&load (om-relative-path '("src") "combinations"))
(compile&load (om-relative-path '("src") "sum-lists"))
(compile&load (om-relative-path '("src") "get-index"))
(compile&load (om-relative-path '("src") "count-truncate"))
(compile&load (om-relative-path '("src") "cribs"))
(compile&load (om-relative-path '("src") "equal-probability"))
(compile&load (om-relative-path '("src") "pitch-inversion"))
(compile&load (om-relative-path '("src") "repeat-list"))
(compile&load (om-relative-path '("src") "retrograde-list"))
(compile&load (om-relative-path '("src") "rotations->poly"))
(compile&load (om-relative-path '("src") "rotations->voice"))



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
 '((NIL nil (prf) (get-combi get-rotations canon-query) nil)
   ("operations" nil (prf) (sum-lists get-index count-truncate cribs equal-probability pitch-inversion repeat-list retrograde-list rotations->poly rotations->voice) nil)
   ("queries-database" nil (prf) (get-combi get-rotations canon-query) nil)

   
   ;;use lines below for sub-sub menus, if needed
   ;;("queries-database" (("subpack1" nil (prf) (get-combi get-rotations canon-query sum-lists get-index) nil)
   ;;         )
   ;; nil nil nil)
   )
 (om::find-library "CNMAT-OM")
 )

;;; evaluate this to generate the HTML reference
;;; (gen-lib-reference (find-library "CNMAT-OM"))