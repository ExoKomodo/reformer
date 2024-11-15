(define-module (reformer routing))

(use-modules (web server)
             (web response)
             (web request)
             (web uri)
             (oop goops)
             (ice-9 format)
             (rnrs bytevectors)
             (reformer auth)
             (reformer config)
             (reformer http)
             (reformer models)
             (reformer pages feed)
             ((reformer pages about) #:prefix about:)
             ((reformer pages home) #:prefix home:))

(define visits 0)

;; TODO: Turn this into a macro to guard all auth routes
(define-public (login/handler request db)
  (let* ([query-args (http/query-args->alist (uri-query (request-uri request)))]
              [username (assoc-ref query-args 'reformer-username)]
              [token (assoc-ref query-args 'reformer-token)]
              [user (user/get-user-by-handle db username)])
          (if (and user (user/auth/verify-code user token))
            (http/build-simple-response #:status 200
                                        #:extra-headers `((Set-Cookie . ,(format #f "reformer-token=~a" token))
                                                          (Set-Cookie . ,(format #f "reformer-username=~a" username))))
            (http/build-simple-response #:status 401))))

(define-public (router request request-body db)
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
     ((string-prefix? "/login" path)
       (set! visits (+ visits 1))
       (login/handler request db))
     ((string-prefix? "/post" path)
      (set! visits (+ visits 1))
      (cond ((string=? (symbol->string method) "POST")
             (begin (let* ((request-data (http/query-args->alist (utf8->string request-body)))
                           (content (assoc-ref request-data 'content))
                           (handle (assoc-ref request-data 'handle))
                           (user (user/get-user-by-handle db handle)))
                      (if (not user)
                          (http/build-simple-response #:status 401)
                          (begin
                            (post/save (make-instance <post>
                                                      #:id #f
                                                      #:content content
                                                      #:user-id (user/id user))
                                       db)
							              (format #t "Created post with text: ~s" content)
                            (http/build-html-response (feed/post-list (post/read-all db)
                                                                      db)))))))
            (else (not-found request))))
     (else (not-found request)))))
