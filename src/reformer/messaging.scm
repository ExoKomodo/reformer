(define-module (reformer messaging)
  #:use-module (email)
  #:use-module (reformer config))

(define-public (send/email username subject content)
    (send-email
        content
        #:to username
        #:subject subject
        #:smtp mail-smtp
        #:username mail-username
        #:password mail-password))
