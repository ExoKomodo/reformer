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

(define (get-field field alist)
  (cdr (assoc field alist)))

(define (load-posts posts)
  (map (lambda (post)
         (let ((user (make-instance <user>
                                      #:handle (get-field 'handle
                                                          (get-field 'user post)))))
           (make-instance <post>
                          #:content (get-field 'content post)
                          #:user user)))
       posts))

(define dummy-posts '(((content . "Lorem ipsum odor amet, consectetuer adipiscing elit. Dictum id penatibus facilisi commodo accumsan odio. Venenatis dis sollicitudin elementum torquent facilisis aenean hac feugiat metus.")
                       (user . ((handle . "OtherGuy"))))
                      ((content . "Lorem ipsum odor amet, consectetuer adipiscing elit.")
                       (user . ((handle . "SomeGuy"))))))

(define (index db)
  (let ((loaded-posts (load-posts dummy-posts)))
    (html-page
     `(,((lambda () (navbar)))
       (h1 (a (@ (href "/feed"))
              "Reformer Feed"))
       ,(map
         (lambda (post)
           `(div
             (p ,(post/content post))
             (h5 ,(format #f "@~a" (user/handle (post/user post))))
             ))
         loaded-posts)))))
