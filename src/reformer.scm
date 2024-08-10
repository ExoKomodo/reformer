(define-module (reformer)
  #:export (cfrr))

(use-modules (web server)
             (reformer config)
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
  (format #t "Reformer is running at ~s:~d~%" host port)
  (run-server
    (lambda (request request-body)
      (router request request-body))
    'http
    `(#:host ,host
      #:port ,port)))
