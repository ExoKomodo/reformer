(define-module (reformer)
  #:export (cfrr))

(use-modules (reformer config)
             (reformer db)
             (reformer routing)
             (web server)
             (rnrs))

(define (creation)
  "Sets up the storage engine for Reformer"
  (format #t "Setting up creation~%")
  (format #t "Testing the database...~%")
  (db/test)
  (format #t "Database testing is complete~%")
  (db/open (db/boot db/sqlite-db-path)))

(define (fall db)
  "Sets up login and identity management systems"
  (format #t "Accounting for the sins of the fall~%"))

(define (redemption db)
  "*Sets up observability and backup systems*"
  (format #t "Providing security for the elect~%"))

(define (restoration db)
  "Perpetual loop of Reformer"
  (format #t "We are to be restored~%")
  (format #t "Reformer will be running at ~a://~a:~d~%" scheme host port)
  (run-server
   (lambda (request request-body)
     (router request request-body db))
   'http
   `(#:host ,host
            #:port ,port)))

(define (cfrr)
  (let ((db (creation)))
    (fall db)
    (redemption db)
    (restoration db)))
