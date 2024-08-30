(define-module (reformer)
  #:export (cfrr))

(use-modules (reformer config)
             (reformer db)
             (reformer routing)
             (reformer models)
             (external sqlite3)
             (oop goops)
             (ice-9 textual-ports)
             (web server))

(define (creation)
  "Sets up the storage engine for Reformer"
  (format #t "Setting up creation~%")
  (format #t "Testing the database...~%")
  (db/test)
  (format #t "Database testing is complete~%")
  (with-db (db db/sqlite-db-path)
           (let ((ddl (call-with-input-file db/ddl-file-path get-string-all)))
             (sqlite-exec db ddl)
             (let ((user1 (user/save (make-instance <user> #:id #f #:handle "jamesaorson") db))
                   (user2 (user/save (make-instance <user> #:id #f #:handle "nbarlow") db)))
               ;; TODO: Get id from save
               (let ((post1 (post/save (make-instance <post> #:id #f #:content "bro" #:user-id 2) db))
                     (post2 (post/save (make-instance <post> #:id #f #:content "hello brosefus" #:user-id 1) db)))
               (format #t "User 1: ~a~%" user1)
               (format #t "User 2: ~a~%" user2)
               (format #t "Post 1: ~a~%" post1)
               (format #t "Post 2: ~a~%" post2)))))
  (db/open db/sqlite-db-path))

(define (fall db)
  "Sets up login and identity management systems"
  (format #t "Accounting for the sins of the fall~%"))

(define (redemption db)
  "*Sets up observability and backup systems*"
  (format #t "Providing security for the elect~%"))

(define (restoration db)
  "Perpetual loop of Reformer"
  (format #t "We are to be restored~%")
  (format #t "Reformer will be running at ~a://~a:~d~%" scheme host port)
  (run-server
   (lambda (request request-body)
     (router request request-body db))
   'http
   `(#:host ,host
            #:port ,port)))

(define (cfrr)
  (let ((db (creation)))
    (fall db)
    (redemption db)
    (restoration db)))
