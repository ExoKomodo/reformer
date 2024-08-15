(define-module (reformer models)
  #:export (<post> get-name set-name name
                   get-content set-content content
                   get-poster set-poster poster
            <poster> get-handle set-handle handle))

(use-modules (oop goops))

(define-class <poster> (<object>)
  (handle #:init-keyword #:handle
          #:getter get-handle
          #:setter set-handle
          #:accessor handle))

(define-class <post> (<object>)
  (name #:init-keyword #:name
        #:getter get-name
        #:setter set-name
        #:accessor name)
  (content #:init-keyword #:content
           #:getter get-content
           #:setter set-content
           #:accessor content)
  (poster #:init-keyword #:poster
          #:init-form (make-instance <poster> #:handle "DEFAULT")
          #:getter get-poster
          #:setter set-poster
          #:accessor poster))

