(define-module (reformer)
  #:export (cfrr))

(use-modules (external sqlite3)
             (ice-9 threads)
             (reformer config)
             (reformer db)
             (reformer routing)
             (web server)
             (rnrs))

(define (test-db)
  (with-db (db "test.db")
    (sqlite-exec db
                 "CREATE TABLE IF NOT EXISTS foo (id INTEGER PRIMARY KEY, bar STRING)")
    (sqlite-exec db
                 "INSERT INTO foo ('bar') VALUES ('something')")
    (sqlite-exec* db
                  "SELECT * FROM foo WHERE :column > :threshold"
                  #:parameters '((column . id)
                                 (threshold . 0))
                  #:indexed #t)))

(define (creation)
  "Sets up the storage engine for Reformer"
  (format #t "Setting up creation~%")
  (format #t "Testing the database...~%")
  (test-db)
  (format #f "Database testing is complete~%"))

(define (fall)
  "Sets up login and identity management systems"
  (format #t "Accounting for the sins of the fall~%"))

(define (redemption)
  "*Sets up observability and backup systems*"
  (format #t "Providing security for the elect~%"))

(define (restoration)
  "Perpetual loop of Reformer"
  (format #t "We are to be restored~%")
  (format #t "Reformer will be running at ~a://~a:~d~%" scheme host port)
  (run-server
   (lambda (request request-body)
     (router request request-body))
   'http
   `(#:host ,host
            #:port ,port)))

(define (cfrr)
  (creation)
  (fall)
  (redemption)
  (restoration))
