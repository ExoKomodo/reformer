(define-module (reformer)
  #:export (cfrr))

(use-modules (f)
			 (pipe)
			 (reformer config)
             ;; ((reformer db psql) #:prefix pq:)
             (reformer db sqlite)
             (reformer routing)
             (reformer models)
             (sqlite3)
             (oop goops)
             (web server))

(define (creation)
  "Sets up the storage engine for Reformer"
  (format #t "Setting up creation~%")
  (format #t "Testing the database...~%")
  (db/test)
  (format #t "Database testing is complete~%")
  (db/with (db db/sqlite-db-path)
           (let ((ddl (read-text db/ddl-file-path)))
             (sqlite-exec db ddl)
             ;; TODO: Install [guile-gcrypt](https://notabug.org/cwebber/guile-gcrypt) and hash the password
             (user/save (make-instance <user>
                                       #:id #f
                                       #:handle "jamesaorson"
                                       #:password-hash "myman") db)
             (user/save (make-instance <user>
                                       #:id #f
                                       #:handle "nbarlow"
                                       #:password-hash "myguy") db)
             ;; TODO: Get id from save
             (post/save (make-instance <post> #:id #f #:content "Hey dude, what's the Lord working in you today?" #:user-id 1) db)
             (post/save (make-instance <post> #:id #f #:content "Something big and similar to bean burrito" #:user-id 2) db)))
  (db/open db/sqlite-db-path))

(define (fall db)
  "Sets up login and identity management systems"
  (format #t "Accounting for the sins of the fall: ~a~%" db)
  db)

(define (redemption db)
  "*Sets up observability and backup systems*"
  (format #t "Providing security for the elect~a~%" db)
  db)

(define (restoration db)
  "Perpetual loop of Reformer"
  (format #t "We are to be restored: ~a~%" db)
  (format #t "Reformer will be running at ~a://~a:~d~%" scheme host port)
  (run-server
   (lambda (request request-body)
     (router request request-body db))
   'http
   `(#:host ,host
            #:port ,port))
  db)

(define (cfrr)
  (-> (creation)
	  (fall)
	  (redemption)
	  (restoration)))

