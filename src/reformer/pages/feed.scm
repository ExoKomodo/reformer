(define-module (reformer pages feed)
  #:export (index))

(use-modules (oop goops)
             (sxml simple)
             (web request)
             (web response)
             (web uri)
             (reformer components navbar)
             (reformer models)
             (reformer html))

(define (index db)
  (let ((loaded-posts (post/read-all db)))
    (html-page
     `(,((lambda () (navbar)))
       (h1 (a (@ (href "/feed"))
              "Reformer Feed"))
       ,(map
         (lambda (post)
           `(div
             (p ,(post/content post))
             (h5 ,(format #f "@~a" (user/handle (user/read-by-id db
                                                                 (post/user-id post)))))
             ))
         loaded-posts)))))
