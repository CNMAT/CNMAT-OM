;;;==================================
;;; RETROGRADE-CANON-TEST
;;;==================================

(in-package :cnmat)

;;;return the retrograde of the list

(defun get-retrograde (my-list)
  (let ((final-list '(nil)))
       (loop for elem in my-list do
             (push elem final-list)
        )
    (remove nil final-list)
    )
)

  
(om::defmethod! retrograde-canon-test ((my-list list))

  :icon 7
  :indoc '("Tests canon-query on list combined with its retrograde")
  :outdoc '("If true, returns list. If false, returns nil") 
  :initvals '((1 2 3 4 5))
  :doc "Tests canon-query on a list combined with its retrograde. If true, returns combined list. If false, returns nil."

  (let* ((retrograde-list (get-retrograde my-list))
         (combo-list (list my-list retrograde-list )))
    
    ;;now test the canon query on the list and its retrograde

        (if (eq (cnmat::canon-query combo-list) 't) 
            combo-list
            'nil)
   )

)
