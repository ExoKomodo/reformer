(define-module (reformer config)
  #:export (host
            port
            scheme
            db-implementation))

(define host "0.0.0.0")
(define port 8080)
(define scheme 'http)
(define db-implementation (or (getenv "DB_IMPLEMENTATION") "sqlite"))
