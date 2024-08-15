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
         (let ((poster (make-instance <poster>
                                      #:handle (get-field 'handle
                                                          (get-field 'poster post)))))
           (make-instance <post>
                          #:name (get-field 'name post)
                          #:content (get-field 'content post)
                          #:poster poster)))
       posts))

(define dummy-posts '(((name . "A post")
                       (content . "Lorem ipsum odor amet, consectetuer adipiscing elit. Dictum id penatibus facilisi commodo accumsan odio. Venenatis dis sollicitudin elementum torquent facilisis aenean hac feugiat metus.")
                       (poster . ((handle . "OtherGuy"))))
                      ((name . "Another post")
                       (content . "Lorem ipsum odor amet, consectetuer adipiscing elit.")
                       (poster . ((handle . "SomeGuy"))))))

(define (index)
  (let ((loaded-posts (load-posts dummy-posts)))
    (html-page
     `(,(navbar)
       (h1 (a (@ (href "/feed"))
              "Reformer Feed"))
       ,(map
         (lambda (post)
           `(div
             (h3 ,(name post))
             (p ,(content post))
             (h4 ,(format #f "@~a" (handle (poster post))))
             ))
         loaded-posts)))))
