(define-module (reformer backend)
  #:export (cfrr))

(define visits 0)

(use-modules (web server)
             (web request)
             (web uri)
             (ice-9 format)
             (reformer http))

(define (router request request-body)
  (log-request request)
  (let ((path (uri->string (request-uri request))))
    (cond
      ((string=? "/" path)
        (set! visits (+ visits 1))
        (values '((content-type . (text/html)))
            (format #f "<html><body><div>Hello World!</div> <div>Visit #~d</div></body></html>" visits)))
      (else
        (not-found request)))))

;; Function: cfrr
;; Description: Runs the backend process loop. creation-fall-redemption-restoration
;; Parameters: '()
;; Return Value: #f
;; Examples:
;;   (cfrr)
;;   => 8
;; Edge Cases:
;;   - Running more than one instance on the same port will fail
;; Error Handling: '()
(define (cfrr)
  (run-server
    (lambda (request request-body) (router request request-body))
    'http
    '(#:host "0.0.0.0"
      #:port 8080)))
