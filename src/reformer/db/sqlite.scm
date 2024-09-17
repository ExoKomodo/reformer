(define-module (reformer db sqlite)
  #:export (db/close
            db/open
            db/test
            db/with))

(use-modules (sqlite3)
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

(define-syntax-rule (db/with (db-binding db-path) body ...)
  (let ((db-binding (db/open db-path)))
    (let ((result (begin body ...)))
      (db/close db-binding)
      result)))

(define (db/test)
  (db/with (db db/sqlite-test-db-path)
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

