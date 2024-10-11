(define-module (reformer)
  #:export (cfrr))

(use-modules (pipe)
			       (reformer config)
             (reformer db)
             (reformer routing)
             (reformer models)
             (oop goops)
             (web server))
(define (creation)
  "Sets up the storage engine for Reformer"
  (format #t "Setting up creation~%")
  (format #t "Testing the database...~%")
  (db/test)
  (format #t "Database testing is complete~%")
  (db/with (db db/connection-string)
		       (db/apply-ddl db)
		       ;; TODO: Install [guile-gcrypt](https://notabug.org/cwebber/guile-gcrypt) and hash the password
           (format #t "Hello?~%")
		       (user/create (make-instance <user>
									                     #:id 1
									                     #:handle "jamesaorson"
									                     #:password-hash "myman") db)
		       (user/create (make-instance <user>
									                     #:id 2
									                     #:handle "nbarlow"
									                     #:password-hash "myguy") db)
		       ;; TODO: Get id from save
		       (post/save (make-instance <post>
                                     #:id #f
                                     #:content "Hey <marquee><strong>dude</strong></marquee>, what is the Lord working in you today?"
                                     #:poster-id 1) db)
		       (post/save (make-instance <post>
                                     #:id #f
                                     #:content "Something big and similar to a bean burrito"
                                     #:poster-id 2) db)

           (format #t "Again?~%")
		       (post/save (make-instance <post>
                                     #:id #f
                                     #:content "<a href=\"https://letmegooglethat.com/\">Use this coding tutor</a>"
                                     #:poster-id 1) db)
		       (post/save (make-instance <post>
                                     #:id #f
                                     #:content "This might be rude, lewd, and obnoxious. <script>setTimeout(function() {window.alert(`haha got you!`)}, 5000)</script>"
                                     #:poster-id 1) db))
  (db/open db/connection-string))

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

