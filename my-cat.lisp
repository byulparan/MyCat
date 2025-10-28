;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.10.23 byulparan@gmail.com
;; 
;; MyCat
;; 


(ql:quickload :cl-nextstep)

(defpackage #:my-cat
  (:use #:cl)
  (:export #:main
	   #:add-menubar
	   #:*assets*))

(in-package :my-cat)

(defvar *assets* "./assets/")
(defvar *cat-view* nil)
(defvar *dog-view* nil)

(defclass view (ns:view)
  ((timer :accessor timer)
   (location :accessor location)
   (assets :initform nil :accessor assets)
   (model :initarg :model :reader model)
   (inverse :initarg :inverse :initform nil :accessor inverse)))


(defun set-model (view model)
  (when (assets view)
    (loop for img in (assets view)
	  do (cg:release-image img)))
  (let* ((model-assets (directory (format nil "~acatndog/~a*.png" *assets* model))))
    (setf (assets view)
      (loop for i below (length model-assets)
	    collect (cg:load-image (format nil "~acatndog/~a (~d).png" *assets* model (+ i 1)))))))



(defmethod ns:init ((view view))
  (set-model view (model view))
  (setf (timer view) (make-instance 'ns:timer
		       :repeats t
		       :interval (/ 1 30.0)
		       :timer-fn (lambda ()
				   (ns:redisplay view)))))



(defmethod ns:draw ((view view))
  (let* ((ctx (ns:current-cg-context))
	 (w (ns:width view))
	 (h (ns:height view))
	 (index (mod (floor (* 10.0 (/ (get-internal-real-time) internal-time-units-per-second))) (length (assets view)))))
    (cg:clear-rect ctx (ns:rect 0 0 w h))
    (when (inverse view)
      (cg:translate-ctm ctx w 0)
      (cg:scale-ctm ctx -1.0 1.0))
    (let* ((img (nth index (assets view)))
	   (img-w (cg:image-width img))
	   (img-h (cg:image-height img))
	   (ratio-w nil)
	   (ratio-h nil))
      (if (> img-w img-h) (setf ratio-w w ratio-h (* h (/ img-h img-w)))
	(setf ratio-w (* w (/ img-w img-h)) ratio-h h))
      (cg:draw-image ctx (ns:rect 0 0 ratio-w ratio-h) img))))


(defmethod ns:release ((view view))
  (ns:invalidate (timer view)))



(defmethod ns:mouse-down ((self view) event loc-x loc-y)
  (when (ns:shift-p event)
    (setf (inverse self) (not (inverse self))))
  (setf (location self) (ns:objc event "locationInWindow" (:struct ns:point))))


(defmethod ns:mouse-dragged ((self view) event loc-x loc-y)
  (let* ((window (ns:objc self "window" :pointer))
	 (current (ns:objc event "locationInWindow" (:struct ns:point)))
	 (rect (ns:objc window "frame" (:struct ns:rect)))
	 (new-origin (ns:point (+ (ns:rect-x rect) (- (ns:point-x current) (ns:point-x (location self))))
			       (+ (ns:rect-y rect) (- (ns:point-y current) (ns:point-y (location self)))))))
    (ns:objc window "setFrameOrigin:" (:struct ns:point) new-origin)))



(defun make-cat ()
  (let* ((win (make-instance 'ns:window
                :x 60 :y 620 :w 200 :h 200))
	 (cview (ns:content-view win)))
    (setf (ns:content-view win)
      (setf *cat-view* (make-instance 'view
			 :x 0 :y 0 :w 100 :h 100
			 :model "cat/Run")))
    (ns:objc win "setStyleMask:" :int 0)
    (ns:objc win "setOpaque:" :bool nil)
    (ns:objc win "setBackgroundColor:" :pointer (ns:objc "NSColor" "clearColor" :pointer))
    (ns:window-show win)))


(defun make-dog ()
  (let* ((win (make-instance 'ns:window
                :x 200 :y 620 :w 200 :h 200))
	 (cview (ns:content-view win)))
    (setf (ns:content-view win)
      (setf *dog-view* (make-instance 'view
			 :x 0 :y 0 :w 100 :h 100
			 :model "dog/Jump")))
    (ns:objc win "setStyleMask:" :int 0)
    (ns:objc win "setOpaque:" :bool nil)
    (ns:objc win "setBackgroundColor:" :pointer (ns:objc "NSColor" "clearColor" :pointer))
    (ns:window-show win)))



(defun main ()
  (ns:with-event-loop nil
    (make-dog)
    (make-cat)))



(defun add-menubar ()
  (let* ((status-item (ns:objc
		       (ns:objc "NSStatusBar" "systemStatusBar" :pointer)
		       "statusItemWithLength:" :double -1.0d0
		       :pointer))
	 (icon (ns:objc "NSImage" "imageWithSystemSymbolName:accessibilityDescription:"
			:pointer (ns:make-ns-string "cat.fill")
			:pointer (cffi:null-pointer)
			:pointer)))
    (ns:objc
     (ns:objc status-item "button" :pointer)
     "setImage:" :pointer icon)
    (let* ((menu (ns:alloc "NSMenu")))
      (ns:objc status-item "setMenu:" :pointer menu)
      (ns:objc menu "addItem:" :pointer (ns:make-menu-item "Quit" :action "terminate:" :key ""))
      (let* ((models (list "Dead" "Fall" "Hurt" "Idle" "Jump" "Run" "Slide" "Walk")))
	(loop for animal in (list "Cat" "Dog")
	      do (ns:objc menu "addItem:" :pointer (ns:objc "NSMenuItem" "separatorItem" :pointer))
		 (loop for model in models
		       do (let* ((item (ns:make-menu-item (format nil "~a:~a" animal model) :action "lispUserAction:" :key ""))
				 (animal animal)
				 (model model))
			    (ns:objc menu "addItem:" :pointer item)
			    (setf (gethash (cffi:pointer-address item) ns:*user-action-table*)
			      (lambda (obj)
				(declare (ignore obj))
				(set-model (if (string= animal "Cat") *cat-view* *dog-view*)
					   (format nil "~a/~a" (string-downcase animal) model)))))))))))


