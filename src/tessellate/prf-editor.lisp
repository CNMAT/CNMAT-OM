
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
    (setf (selection self) panel)  
    ;(setf (selected panel) t)   
    ;(oa::om-set-bg-color panel (oa::om-make-color 0.9 1 0.9))
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
      (when (selected self) 
        (oa:om-with-fg-color nil
            (oa::om-make-color 0.98 0.96 0.95)
            (oa::om-fill-rect 0 0 (1- (om::w self)) (1- (om::h self)))))
      (draw-rhythmic-line (prf self) 
                          (get-r-frame-size (get-prf self))
                          4 (- (om::w self) 8) 2 (- (om::h self) 4) 0
                          (selected self))  
      )))

(defmethod oa::om-view-click-handler ((self prf-panel) pos)
  (init-selection (om::editor self))
  (set-selection (om::editor self) self)
  (let ((beatpos (beat-graphic-positions (get-r-frame-size (get-prf self))
                                         (- (om::w self) 8)
                                         4)))
    ;; click on beat ?
    (let ((clicked-beat (position pos beatpos :test #'(lambda (p x)
                                    (and (>= (oa::om-point-x p) (- x 5))
                                         (<= (oa::om-point-x p) (+ x 5))
                                         (>= (oa::om-point-y p) (- (round (om::h self) 2) 15))
                                         (<= (oa::om-point-y p) (+ (round (om::h self) 2) 10)))))))
      (if clicked-beat  ;;; the click is on one of the beats
          
          ;;; is  this beat part of the line's pulses ?
          (let ((nth-pulse (position clicked-beat (accent-beats (pulses (prf self)))))) 
            (when (and (oa::om-command-key-p) (not nth-pulse))
              ;;; ... if not + cmd-click : add the beat
              (setf (pulses (prf self)) (accents-to-pulses (sort (cons clicked-beat (accent-beats (pulses (prf self)))) '<))))
            
            ;;; set this beat as selection in any case
            (setf (selected self) (list (position clicked-beat (accent-beats (pulses (prf self))))))
            
            (om::report-modifications (om::editor self)))
        
        ;; else (if no beat selected) just select the line
        (unless (selected self) (setf (selected self) (list -1))))
      )
   
    (oa:om-invalidate-view self)))


(defmethod om::handle-key-event ((self prfeditor) char)
  (let ((selected-panel (selection self)))
    (when selected-panel
      (let* ((rf (prf selected-panel))
             (container-prf (get-prf selected-panel))
             (accents (accent-beats (pulses rf)))
             (selection (car (selected selected-panel))))
        
        (case char

          (:om-key-delete
           (if (and (numberp selection) (>= selection 0))
               ;;; there's a beat selected
               ;;; => remove it from line's pulses
               (progn  
                 (setf (pulses rf)
                       (accents-to-pulses (append (om::first-n accents selection)
                                          (nthcdr (1+ selection) accents))))
                 (initialize-instance rf))
             (progn 
               ;;; the pane is selected but no specific beat
               ;;; => delete it
               (delete-prf-voice rf (om::object self) (om::object self))
               (reset-contents self)))
           (init-selection (om::editor self))
           (om::report-modifications self))

          (#\-  
           ;;; inverse pulse / silence
           (let ((pos (position selection accents)))
             (setf (nth selection (pulses rf))
                   (- (nth selection (pulses rf))))
             (oa:om-invalidate-view selected-panel)
             (om::report-modifications self)))

          (:om-key-right  
           ;;; move selected pulse to the right
           (when (and (numberp selection) (>= selection 0))
             (let* ((current-accent (nth selection accents))
                    (new-accent? (1+ current-accent))
                    (silence? (minusp (nth selection (pulses rf)))))
               
               (unless (or (>= new-accent? (size (prf selected-panel)))
                           (find new-accent? accents))
               
                 (setf (pulses rf) 
                       (accents-to-pulses (sort (cons new-accent? (remove current-accent accents)) '<)))
                 
                 (when silence? 
                   (setf (nth selection (pulses rf))
                       (- (nth selection (pulses rf)))))
                 
                 (oa:om-invalidate-view selected-panel)
               (om::report-modifications self)))))

          (:om-key-left 
           ;;; move selected pulse to the right
           (when (and (numberp selection) (>= selection 0))
             (let* ((current-accent (nth selection accents))
                    (current-pulse-value (nth selection (pulses rf)))
                    (new-accent? (1- current-accent))
                    (silence? (minusp (nth selection (pulses rf)))))
               
               (unless (or (< new-accent? 0)
                         (find new-accent? accents))
                 
                 (setf (pulses rf) 
                       (accents-to-pulses (sort (cons new-accent? (remove current-accent accents)) '<)))
                 
                 (when silence? 
                 (setf (nth selection (pulses rf))
                       (- (nth selection (pulses rf)))))
                 
                 (oa:om-invalidate-view selected-panel)
                 (om::report-modifications self)))))

          (:om-key-up
           ;;; extend the fralme averall size : longer duration for last pulse
           (setf (pulses rf) (append (butlast (pulses rf))
                                     (list (if (plusp (car (last (pulses rf))))
                                               (1+ (car (last (pulses rf))))
                                             (1- (car (last (pulses rf))))))))
           (initialize-instance rf)
           (oa:om-invalidate-view self)
           (om::report-modifications self))

          (:om-key-down 
           ;;; reduce move pulse to the right
           (when (> (abs (car (last (pulses rf)))) 1)
             (setf (pulses rf) (append (butlast (pulses rf))
                                     (list (if (plusp (car (last (pulses rf))))
                                               (1- (car (last (pulses rf))))
                                             (1+ (car (last (pulses rf))))))))
             (initialize-instance rf)
             (oa:om-invalidate-view self)
             (om::report-modifications self)))

          (:om-key-esc
           (init-selection self))

          (#\s  
           (apply-r-substitute container-prf rf nil nil)
           (reset-contents self)
           (om::report-modifications self))
          )
        ))))


;; todo : remove, inverse sign, move l/r

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





(defmethod om::get-menubar ((self prfeditor)) 
  (list (oa::om-make-menu 
         "File" 
         (list 
          (oa::om-new-leafmenu "Close" #'(lambda () (oa::om-close-window (oa::om-view-window self))) "w")
          (oa::om-new-leafmenu "Print" #'(lambda () (oa::om-print-window (oa::om-view-window self))) "p")))
        (om::make-om-menu 'om::windows :editor self)))


