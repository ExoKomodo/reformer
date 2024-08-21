(define-module (reformer db)
  #:export (db-close
            db-open
            with-db))

(use-modules (external sqlite3)
             (ice-9 format)
             ((rnrs) #:prefix rnrs:))

(define (db-open name)
  (format #t "Bringing up the ~s database...~%" name)
  (let ((db (sqlite-open name)))
    (rnrs:assert (sqlite-db? db))
    (format #t "Database is running!~%")
    db))

(define (db-close db)
  (format #t "Closing the database...~%")
  (sqlite-close db)
  (format #t "Closed the database!~%"))

(define-syntax-rule (with-db (db-binding db-path) body ...)
  (let ((db-binding (db-open db-path)))
    (let ((result (begin body ...)))
      (db-close db-binding)
      result)))
