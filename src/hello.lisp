(require "hunchentoot")
(require "cl-who")
(use-package :cl-who)
(use-package :hunchentoot)

(defclass search-server (acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions")))

(defvar *mysrv* (make-instance 'search-server :port 80))

(defun find-not-nil (l p) 
  (if (endp l)
      nil
    (or (funcall p (car l))
         (find-not-nil (cdr l) p))))

(defmethod acceptor-dispatch-request ((srv search-server) (req request))
  (let ((l (find-not-nil (dispatch-table srv)
                         (lambda (disp) (funcall disp req)))))
    (or l (call-next-method))))


(defmacro with-html ((var) &body body)
  `(with-html-output-to-string (,var)
                               ,@body))

(defun my-prefix-disp (prefix handler)
  (lambda (req)
    (let ((m (mismatch prefix (script-name* req))))
      (if (or (null m) (>= m (length prefix)))
          (funcall handler req)))))

(defun push-dispatcher (srv disp)
  (push disp (dispatch-table srv)))

(let ((counter 0))
  (defun dummy-dispatch (req)
    (with-html (s)
               (:html
                (:body
                 "Greetings! You are visitor number "
                 (str (incf counter)))))))

(push-dispatcher *mysrv*
                 (my-prefix-disp "/hello" (quote dummy-dispatch)))

(start *mysrv*)
