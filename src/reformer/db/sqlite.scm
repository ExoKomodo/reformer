(define-module (reformer db sqlite)
  #:export (db/apply-ddl
			      db/close
            db/open
			      db/query
            db/test
            db/with
			      db/connection-string
			      db/test-connection-string
			      db/ddl-file-path))

(use-modules (f)
			       (sqlite3)
             (reformer config)
             (reformer models)
             (ice-9 format)
             ((rnrs) #:prefix rnrs:))

(define db/connection-string "reformer.db")
(define db/test-connection-string "test.db")
(define db/ddl-file-path (or (getenv "REFORMER_DB_DDL_FILE_PATH") "src/ddl.sqlite.sql"))

(define (db/apply-ddl db)
  (let ((ddl (read-text db/ddl-file-path)))
	  (sqlite-exec db ddl)))

(define (db/close db)
  (format #t "Closing the sqlite database...~%")
  (sqlite-close db)
  (format #t "Closed the sqlite database!~%"))

(define (db/open name)
  (format #t "Bringing up the ~s sqlite database...~%" name)
  (let ((db (sqlite-open name)))
    (rnrs:assert (sqlite-db? db))
    (format #t "Sqlite database is running!~%")
    db))

(define* (db/query db query
				           #:key
				           (parameters '())
				           (row-handler identity))
  (format #t "Sqlite query: ~a~%" query)
	(sqlite-exec* db query
					      #:parameters parameters
                #:indexed #t
					      #:row-handler row-handler))

(define-syntax-rule (db/with (db-binding db-path) body ...)
  (let ((db-binding (db/open db-path)))
    (let ((result (begin body ...)))
      (db/close db-binding)
      result)))

(define (db/test)
  (db/with (db db/test-connection-string)
		   (db/query db
					 "CREATE TABLE IF NOT EXISTS foo (id INTEGER PRIMARY KEY, bar TEXT);")
		   (db/query db
					 "DELETE FROM foo;")
		   (db/query db
					 "INSERT INTO foo (id, bar) VALUES (1, 'something');")
		   (db/query db
					 "SELECT * FROM foo WHERE id > 0;"
					 #:row-handler identity)))

