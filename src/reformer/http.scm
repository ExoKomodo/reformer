(define-module (reformer http)
  #:export (not-found log-request))

(use-modules (web request)
             (web response)
             (web uri))

(define (log-header header)
    (format #t "~s=~a," (car header) (cdr header)))

(define (log-request request)
  (format #t
          "~s:~s:"
          (request-method request)
          (uri->string (request-uri request)))
  (let ((headers (request-headers request)))
    (for-each log-header headers))
  (format #t "~%"))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))
