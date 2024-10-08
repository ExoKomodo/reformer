(define-module (reformer html)
  #:export (html-page html-page-template))

(use-modules (web response)
             (sxml simple))

(define (html-page-template title body)
  `(html (head (title ,title)
               (link (@ (rel "stylesheet")
                        (href "/static/css/index.css")))
               (script (@ (src "https://unpkg.com/htmx.org@2.0.2")) ""))
         (body (@ (class "theme antique"))
               ,@body)))

(define* (html-page #:optional body
                    #:key
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
