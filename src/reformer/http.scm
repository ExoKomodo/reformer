(define-module (reformer http)
  #:export (not-found log-request http/build-simple-response http/build-html-response http/query-args->alist))

(use-modules (sxml simple)
             (web request)
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

(define (http/query-args->alist args)
  (map (lambda (arg)
         (let ((pair (string-split arg #\=)))
           (cons (string->symbol (uri-decode (car pair))) (uri-decode (car (cdr pair))))))
       (string-split args #\&)))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define* (http/build-simple-response #:optional body
                                     #:key
                                     (status 200))
  (values (build-response #:code status)
          body))

(define* (http/build-html-response #:optional body
                                   #:key
                                   (status 200)
                                   (content-type-params '((charset . "utf-8")))
                                   (content-type 'text/html)
                                   (extra-headers '())
                                   (sxml body))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (when sxml
              (sxml->xml sxml port)))))
