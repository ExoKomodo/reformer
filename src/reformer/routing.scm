(define-module (reformer routing)
  #:export (router))

(define visits 0)

(use-modules ((web server))
             ((web request))
             ((web uri))
             ((ice-9 format))
             ((reformer http))
             ((reformer pages about) #:prefix about:)
             ((reformer pages feed) #:prefix feed:)
             ((reformer pages home) #:prefix home:))

(define (router request request-body db)
  (log-request request)
  (let ((path (uri->string (request-uri request))))
    (cond
      ((string=? "/" path)
        (set! visits (+ visits 1))
        (home:index visits))
      ((string=? "/feed" path)
        (set! visits (+ visits 1))
        (feed:index db))
      ((string=? "/about" path)
        (set! visits (+ visits 1))
        (about:index))
      (else
        (not-found request)))))
