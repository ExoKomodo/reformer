(define-module (reformer pages feed)
  #:export (index))

(use-modules (web request)
             (web response)
             (web uri)
             (reformer html)
             (sxml simple)
             (reformer components navbar))

(define posts '(((name . "A post") (content . "Lorem ipsum odor amet, consectetuer adipiscing elit. Dictum id penatibus facilisi commodo accumsan odio. Venenatis dis sollicitudin elementum torquent facilisis aenean hac feugiat metus."))
                ((name . "Another post") (content . "Lorem ipsum odor amet, consectetuer adipiscing elit."))))

(define (get-field field alist)
  (cdr (assoc field alist)))

(define (index)
  (html-page
    `(,(navbar)
        (h1 (a (@ (href "/feed")) "Reformer Feed"))
        ,(map 
          (lambda (post)
            `(div
              (h3 ,(get-field 'name post))
              (p ,(get-field 'content post))
            ))
          posts))))
