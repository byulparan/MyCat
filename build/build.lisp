
(ql:quickload :cl-nextstep)


;;;;;;;;;;;;;;;;;;;;;;;;
;;  load source code  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(load "../my-cat.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;
;;  remove libraries  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((remove-libraries (list "libcl-nextstep")))
  (setf sb-alien::*shared-objects*
    (remove-if
     (lambda (lib) (find (pathname-name (sb-alien::shared-object-pathname lib)) remove-libraries :test #'string=))
     sb-alien::*shared-objects*)))


;;;;;;;;;;;;;;;;;;;
;;  setup entry  ;;
;;;;;;;;;;;;;;;;;;;

(defun cl-user::build-toplevel-function ()
  (let* ((runtime-dir (namestring (make-pathname :directory (pathname-directory sb-ext::*runtime-pathname*))))
	 (frameworks-dir (concatenate 'string runtime-dir "../Frameworks/")))
    (flet ((load-library (name)
	     (cffi:load-foreign-library (concatenate 'string frameworks-dir name))))
      (load-library "libcl-nextstep.dylib"))
    (setf my-cat:*assets* (concatenate 'string runtime-dir "../Resources/")))
  (ns:start-event-loop :initial-functions (list #'my-cat:add-menubar)))



(pushnew
 (lambda ()
   (my-cat:main))
 ns:*startup-hooks*)



