(define-module (reformer routing)
  #:export (router))

(use-modules (web server)
             (web response)
             (web request)
             (web uri)
             (oop goops)
             (ice-9 format)
             (rnrs bytevectors)
             (reformer http)
             (reformer models)
             (reformer pages feed)
             ((reformer pages about) #:prefix about:)
             ((reformer pages home) #:prefix home:))

(define visits 0)

(define (router request request-body db)
  (log-request request)
  (let ((path (uri->string (request-uri request)))
        (method (request-method request)))
    (cond
      ((string=? "/" path)
       (set! visits (+ visits 1))
       (home:index visits))
      ((string=? "/feed" path)
       (set! visits (+ visits 1))
       (feed/index db))
      ((string=? "/about" path)
       (set! visits (+ visits 1))
       (about:index))
      ((string-prefix? "/post" path)
       (set! visits (+ visits 1))
       (cond ((string=? (symbol->string method) "POST")
              (begin (let* ((request-data (http/query-args->alist (utf8->string request-body)))
                            (content (assoc-ref request-data 'content))
                            (password (assoc-ref request-data 'password))
                            (user (user/get-user-by-password db password)))
                       (if (not user)
                           (http/build-simple-response #:status 401)
                           (begin
                             (post/save (make-instance <post>
                                                       #:id #f
                                                       #:content content
                                                       #:poster-id (user/id user))
                                        db)
							 (format #t "Created post with text: ~s" content)
                             (http/build-html-response (feed/post-list (post/read-all db)
                                                                       db)))))))
             (else (not-found request))))
      (else (not-found request)))))
