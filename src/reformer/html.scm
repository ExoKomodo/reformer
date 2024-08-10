(define-module (reformer html)
  #:export (html-page html-page-template))

(use-modules (web response)
             (sxml simple))

(define (html-page-template title body)
  `(html (head (title ,title))
         (body (@ (style "background-color: antiquewhite;")) ,@body)))

(define* (html-page
  #:optional body #:key
  (status 200)
  (title "Reformer")
  (doctype "<!DOCTYPE html>\n")
  (content-type-params '((charset . "utf-8")))
  (content-type 'text/html)
  (extra-headers '())
  (sxml (and body (html-page-template title body))))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (if sxml
                (begin
                  (if doctype (display doctype port))
                  (sxml->xml sxml port))))))
