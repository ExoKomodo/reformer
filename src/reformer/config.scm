(define-module (reformer config)
  #:export (host
            port
            scheme
            db/sqlite-db-path
            db/sqlite-test-db-path
            db/ddl-file-path))

(define host "0.0.0.0")
(define port 8080)
(define scheme 'http)
(define db/sqlite-db-path (or (getenv "REFORMER_DB_SQLITE_DB_PATH") "reformer.db"))
(define db/sqlite-test-db-path (or (getenv "REFORMER_DB_SQLITE_TEST_DB_PATH") "test.db"))
(define db/ddl-file-path (or (getenv "REFORMER_DB_DDL_FILE_PATH") "src/ddl.sql"))


