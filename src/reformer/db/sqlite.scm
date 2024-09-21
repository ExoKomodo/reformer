(define-module (reformer db sqlite)
  #:export (db/close
            db/open
			db/query
            db/test
            db/with))

(use-modules (sqlite3)
             (reformer config)
             (reformer models)
             (ice-9 format)
             ((rnrs) #:prefix rnrs:))

(define (db/close db)
  (format #t "Closing the database...~%")
  (sqlite-close db)
  (format #t "Closed the database!~%"))

(define (db/open name)
  (format #t "Bringing up the ~s database...~%" name)
  (let ((db (sqlite-open name)))
    (rnrs:assert (sqlite-db? db))
    (format #t "Database is running!~%")
    db))

(define* (db/query db query
				   #:key
				   (parameters '())
				   (indexed #t)
				   (row-handler identity))
		 (sqlite-exec* db query
					   #:parameters parameters
					   #:indexed indexed
					   #:row-handler row-handler))

(define-syntax-rule (db/with (db-binding db-path) body ...)
  (let ((db-binding (db/open db-path)))
    (let ((result (begin body ...)))
      (db/close db-binding)
      result)))

(define (db/test)
  (db/with (db db/sqlite-test-db-path)
    (db/query db
			  "CREATE TABLE IF NOT EXISTS foo (id INTEGER PRIMARY KEY, bar STRING)")
	(db/query db
			  "DELETE FROM foo")
    (db/query db
			  "INSERT INTO foo ('bar') VALUES ('something')")
    (db/query db
			  "SELECT * FROM foo WHERE :column > :threshold"
			  #:parameters '((column . id)
							 (threshold . 0))
			  #:indexed #t
			  #:row-handler identity)))

