(define-module (reformer db)
  #:export (db/close
            db/open
            db/test
            with-db))

(use-modules (external sqlite3)
             (reformer config)
             (reformer models)
             (ice-9 format)
             ((rnrs) #:prefix rnrs:))

(define (db/open name)
  (format #t "Bringing up the ~s database...~%" name)
  (let ((db (sqlite-open name)))
    (rnrs:assert (sqlite-db? db))
    (format #t "Database is running!~%")
    db))

(define (db/close db)
  (format #t "Closing the database...~%")
  (sqlite-close db)
  (format #t "Closed the database!~%"))

(define-syntax-rule (with-db (db-binding db-path) body ...)
  (let ((db-binding (db/open db-path)))
    (let ((result (begin body ...)))
      (db/close db-binding)
      result)))

(define (db/test)
  (with-db (db db/sqlite-test-db-path)
    (sqlite-exec db
                 "CREATE TABLE IF NOT EXISTS foo (id INTEGER PRIMARY KEY, bar STRING)")
    (sqlite-exec db
                 "DELETE FROM foo")
    (sqlite-exec db
                 "INSERT INTO foo ('bar') VALUES ('something')")
    (sqlite-exec* db
                  "SELECT * FROM foo WHERE :column > :threshold"
                  #:parameters '((column . id)
                                 (threshold . 0))
                  #:indexed #t
                  #:row-handler identity)))

