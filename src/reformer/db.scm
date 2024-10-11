(define-module (reformer db)
	#:use-module ((reformer db psql) #:prefix psql:)
	#:use-module ((reformer db sqlite) #:prefix sqlite:)
  #:use-module (reformer config))

(format #t "Implementation: ~a~%" db-implementation)

(cond
 ((string=? db-implementation "psql")
  (re-export
	 (psql:db/apply-ddl . db/apply-ddl)
	 (psql:db/close . db/close)
	 (psql:db/open . db/open)
	 (psql:db/query . db/query)
	 (psql:db/test . db/test)
	 (psql:db/with . db/with)
	 (psql:db/connection-string . db/connection-string)
	 (psql:db/test-connection-string . db/test-connection-string)
	 (psql:db/ddl-file-path . db/ddl-file-path)))
 ((string=? db-implementation "sqlite")
	(re-export
	 (sqlite:db/apply-ddl . db/apply-ddl)
	 (sqlite:db/close . db/close)
	 (sqlite:db/open . db/open)
	 (sqlite:db/query . db/query)
	 (sqlite:db/test . db/test)
	 (sqlite:db/with . db/with)
	 (sqlite:db/connection-string . db/connection-string)
	 (sqlite:db/test-connection-string . db/test-connection-string)
	 (sqlite:db/ddl-file-path . db/ddl-file-path)))
 (else
  (format #t "UNSUPPORTED: unknown db backend - ~a~%" db-implementation)
  (exit)))

