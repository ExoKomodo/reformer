(define-module (reformer config))

(define-public host "0.0.0.0")
(define-public port 8080)
(define-public scheme 'http)
(define-public db-implementation (or (getenv "DB_IMPLEMENTATION") "sqlite"))

(define-public mail-smtp "smtp.gmail.com")
(define-public mail-imap "imap.gmail.com")
(define-public mail-username "exokomodo@gmail.com")
(define-public mail-password (getenv "REFORMER_MAIL_PASSWORD"))
