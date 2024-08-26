(define-module (reformer models)
  #:export (<post> get-content set-content content
                   get-user set-user user
            <user> get-handle set-handle handle))

(use-modules (oop goops))

(define-class <user> (<object>)
  (handle #:init-keyword #:handle
          #:getter get-handle
          #:setter set-handle
          #:accessor handle))

(define-class <post> (<object>)
  (content #:init-keyword #:content
           #:getter get-content
           #:setter set-content
           #:accessor content)
  (user #:init-keyword #:user
          #:init-form (make-instance <user> #:handle "DEFAULT")
          #:getter get-user
          #:setter set-user
          #:accessor user))

