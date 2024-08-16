(define-module (reformer components navbar)
  #:export (navbar))

(use-modules (web request)
             (web response)
             (web uri)
             (reformer html)
             (sxml simple))

(define (navbar)
  `((div (@ (class "theme antique navbar"))
         (ul
          (li (a (@ (href "/"))
                 "Home"))
          (li (a (@ (href "/feed"))
                 "Feed"))
          (li (a (@ (href "/about"))
                 "About"))))))
