;;;==================================
;;; S-COMBINE-VOICES
;;;==================================

(in-package :cnmat)

(defun get-rid-of-rests (rhythms)

  (if ( plusp (last-elem rhythms))  rhythms (get-rid-of-rests (butlast rhythms)))

)




(om::defmethod! s-combine-voices ((voice1 voice) (voice2 voice))

  :icon 1
  :indoc '("a voice object" "a second voice object")
  :outdoc '("Joins (concatenates) one voice to another.") 
  :initvals '('(nil) '(nil))
  :doc "Combines voice scores. Use this when the concat object won't work, i.e. when segments of music dont end tidly at the end of a bar. Joins voices according to these rules: If the last rhythm is a rest then this last rest is deleted and the new voice is joined snug with the last pitch.Takes tempo and legato from the first voice."

(let* ((rhythms1 (om::tree2ratio (om::tree voice1)))
      (rhythms1-tested (get-rid-of-rests rhythms1))
      (chords1 (om::chords voice1))
      (tempo (om::tempo voice1))
      (legato (om::legato voice1))
      (rhythms2 (om::tree2ratio (om::tree voice2)))
      (chords2 (om::chords voice2))
      ;;and now get rid of any extra rest at end that would prevent further layering
      (pre-final-rhythms (flat (list rhythms1-tested rhythms2)))
      (final-rhythms (if ( plusp (last-elem pre-final-rhythms))  pre-final-rhythms (butlast pre-final-rhythms)))
)

  (make-instance 'voice
                 :tree final-rhythms
                 :chords (flat (list chords1 chords2) 1)
                 :tempo tempo
                 :legato legato)

   
)

)
