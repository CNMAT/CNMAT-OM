
(in-package :cnmat)


(defclass prfeditor (om::editorview) 
  ((datapanel :accessor datapanel :initform nil)
   (selection :accessor selection :initform nil)))

(defmethod om::class-has-editor-p ((self prf)) t)
(defmethod om::get-editor-class ((self prf)) 'prfeditor)

(defclass prf-panel (om::om-view) 
  ((selected :accessor selected :initarg :selected :initform nil)
   (prf :accessor prf :initarg :prf :initform nil)))

(defmethod om::editor ((self prf-panel)) 
  (om::editor (om::om-view-container self)))

(defmethod initialize-instance :after ((self prfeditor) &rest l)
  (declare (ignore l))
  (init-prf-editor self)
  self)

(defmethod om::update-editor-after-eval ((self prfeditor) val)
  (setf (om::object self) val)
  (reset-contents self))

(defmethod init-selection ((self prf-panel))
  (loop for sp in (oa::om-subviews self) do
        (setf (selected sp) nil)
        (oa::om-set-bg-color sp oa::*om-white-color*)
        (init-selection sp)))

(defmethod init-selection ((self prfeditor))
  (setf (selection self) nil)
  (init-selection (datapanel self)))
  

(defmethod set-selection ((self prfeditor) (panel prf-panel))
  (init-selection (datapanel self))
  (unless (equal panel (datapanel self))
    (setf (selected panel) t)
    (setf (selection self) panel)  
    (oa::om-set-bg-color panel (oa::om-make-color 0.9 1 0.9))
    (oa::om-invalidate-view self)))

(defmethod reset-contents ((self prfeditor))
  (oa::om-remove-subviews self (datapanel self))
  (init-prf-editor self)
  (init-selection self)
  (om::update-subviews self))

(defmethod init-prf-editor ((self prfeditor))
  (oa::om-add-subviews self (setf (datapanel self) 
                              (oa::om-make-view 'prf-panel 
                                            :position (oa::om-make-point 0 0)
                                            :size (oa::om-make-point 100 30)
                                            :bg-color oa::*om-light-gray-color*
                                            :prf (om::object self))))
  (init-prf-panel (datapanel self) (om::object self)))


(defmethod init-prf-panel ((self prf-panel) (prf polyrhythmic-frame))
  (loop for prf in (voices (prf self)) do
        (let ((sub-panel (om::om-make-view 'prf-panel 
                                           :position (oa::om-make-point 0 0)
                                           :size (oa::om-make-point 100 30)
                                           :prf prf
                                           :bg-color oa::*om-white-color*)))
          (oa::om-add-subviews self sub-panel)
          (init-prf-panel sub-panel prf)
          )))

(defmethod init-prf-panel ((self prf-panel) (prf rhythmic-frame)) nil)
  

(defmethod om::update-subviews ((self prfeditor))
  (call-next-method)
  (let ((space 2))
    (oa::om-set-view-size (datapanel self)
                      (oa::om-make-point (- (om::w self) (* space 2))
                                     (- (om::h self) (* space 2)))) 
    (oa::om-set-view-position (datapanel self) (oa::om-make-point space space))
    (om::update-subviews (datapanel self))
    ))

(defmethod total-subviews ((self oa::om-view))
  (if (oa:om-subviews self)
      (apply '+ (mapcar 'total-subviews (oa:om-subviews self)))
    1))

(defmethod om::update-subviews ((self prf-panel))
  (when (oa::om-subviews self)
    (let* ((space 2)
           (total-views (total-subviews self))
           (n (length (oa::om-subviews self)))
           (vh (/ (- (om::h self) (* (1+ n) space)) total-views))
           (vw (om::w self))
           (vx 0)
           (pos space))
      (loop for sv in (oa::om-subviews self) 
            for i = 0 then (+ i 1) do
            (let ((nsv (max 1 (total-subviews sv))))
              (oa::om-set-view-size sv (oa::om-make-point vw (* vh nsv)))
              (oa::om-set-view-position sv (oa::om-make-point vx pos))
              (setf pos (+ pos (* vh nsv) space))
              (om::update-subviews sv)))
      )))
  
;; not very nice
(defmethod get-prf ((self prf-panel))
  (if (oa::om-subviews self)
      (prf self)
    (get-prf (oa::om-view-container self))))
    
(defmethod get-prf ((self prfeditor))
  (om::object self))
    

(defmethod oa::om-draw-contents ((self prf-panel)) 
  (if (oa::om-subviews self)
    ;(oa::om-with-focused-view self
    ;  (oa:om-draw-rect-outline 1 1 (- (om::w self) 2) (- (om::h self) 2)))
    NIL 
    (oa::om-with-focused-view self
      (draw-rhythmic-line (prf self) 
                          (get-r-frame-size (get-prf self))
                          4 (- (om::w self) 8) 2 (- (om::h self) 4) 0)  
      )))

(defmethod oa::om-view-click-handler ((self prf-panel) pos)
  (unless (selected self)
    (set-selection (om::editor self) self)))

(defmethod delete-prf-voice ((voice t) (from rhythmic-frame) top-level) nil)

(defmethod delete-prf-voice ((voice t) (from polyrhythmic-frame) top-level)
  (unless (equal voice from)
    (if (find voice (voices from))
        (setf (voices from) (remove voice (voices from)))
      (loop for v in (voices from) do
            (delete-prf-voice voice v top-level)))
    (when (null (voices from)) (delete-prf-voice from top-level top-level))))


(defmethod apply-r-substitute ((container-prf polyrhythmic-frame) frame val subs)
  (let* ((value (or val (ignore-errors 
                          (read-from-string 
                           (oa:om-get-user-string "Value to substitute")))))
         (substitutes (and value 
                           (or subs 
                               (ignore-errors 
                                 (read-from-string 
                                  (oa:om-get-user-string "Values to substitute with" :initial-string "( )")))))))
    
    (when (and value substitutes)
      
      (let ((new-frame (r-substitute frame value substitutes))
            (pos (or (position frame (voices container-prf)) -1)))
        
        (setf (voices container-prf) 
              (insert-in-list (voices container-prf) new-frame (1+ pos)))
        
        ))
    ))

       
(defmethod om::handle-key-event ((self prfeditor) char)
  (let ((selected-panel (selection self)))
    (when selected-panel
      (let ((selected-frame (prf selected-panel))
            (container-prf (get-prf selected-panel)))
        (case char
          (:om-key-delete 
           (delete-prf-voice selected-frame (om::object self) (om::object self))
           (reset-contents self)
           (om::report-modifications self))
          (:om-key-esc
           (init-selection self))
          (#\s  
           (apply-r-substitute container-prf selected-frame nil nil)
           (reset-contents self)
           (om::report-modifications self))
          )
        ))))

(defmethod om::get-menubar ((self prfeditor)) 
  (list (oa::om-make-menu 
         "File" 
         (list 
          (oa::om-new-leafmenu "Close" #'(lambda () (oa::om-close-window (oa::om-view-window self))) "w")
          (oa::om-new-leafmenu "Print" #'(lambda () (oa::om-print-window (oa::om-view-window self))) "p")))
        (om::make-om-menu 'om::windows :editor self)))


