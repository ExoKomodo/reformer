(define-module (reformer http)
  #:export (not-found log-request))

(use-modules (web request)
             (web response)
             (web uri))

(define (log-header header)
    (format #t "~/~s: ~a~%" (car header) (cdr header)))

(define (log-request request)
  (format #t "Method: ~s~%" (request-method request))
  (format #t "Path:   ~s~%" (uri->string (request-uri request)))
  (format #t "Headers:~%")
  (let ((headers (request-headers request)))
    (for-each log-header headers)))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))