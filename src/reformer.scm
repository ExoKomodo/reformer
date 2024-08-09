(define-module (reformer)
  #:export (cfrr))

(use-modules (web server)
             (reformer routing))

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
