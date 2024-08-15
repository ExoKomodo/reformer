(define-module (reformer pages feed)
  #:export (index))

(use-modules (sxml simple)
             (web request)
             (web response)
             (web uri)
             (reformer components navbar)
             (reformer html))

(define (get-field field alist)
  (cdr (assoc field alist)))

(define posts '(((name . "A post")
                 (content . "Lorem ipsum odor amet, consectetuer adipiscing elit. Dictum id penatibus facilisi commodo accumsan odio. Venenatis dis sollicitudin elementum torquent facilisis aenean hac feugiat metus.")
                 (poster . ((handle . "OtherGuy"))))
                ((name . "Another post")
                 (content . "Lorem ipsum odor amet, consectetuer adipiscing elit.")
                 (poster . ((handle . "SomeGuy"))))))

(define (get-content post)
  (get-field 'content post))

(define (get-name post)
  (get-field 'name post))

(define (get-poster post)
  (get-field 'poster post))

(define (get-handle post)
  (get-field 'handle (get-poster post)))

(define (index)
  (html-page
   `(,(navbar)
     (h1 (a (@ (href "/feed"))
            "Reformer Feed"))
     ,(map 
       (lambda (post)
         `(div
           (h3 ,(get-name post))
           (p ,(get-content post))
           (h4 ,(format #f "@~a" (get-handle post)))
           ))
       posts))))
