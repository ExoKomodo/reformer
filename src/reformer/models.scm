(define-module (reformer models)
			   #:export (<post> post/id post/get-id
								post/content post/get-content post/set-content!
								post/poster-id post/get-poster-id post/set-poster-id!
								post/delete post/read-all post/save
								post->list
								<user> user/handle user/get-handle user/set-handle!
								user/id user/get-id
								user/password-hash user/get-password-hash
								user/get-user-by-password
								user/read-by-id
								user/read-all
								user/save
                user/create
								define-foreign-type))

(use-modules (oop goops)
			       (reformer db)
			       (system foreign))

(define-syntax-rule
  (define-foreign-type (reader writer) (field type) ...)
  (begin
	(define (reader bv offset)
	  (read-c-struct bv offset ((field type) ...) values))
	(define (writer bv offset field ...)
	  (write-c-struct bv offset ((field type) ...)))))

(define-class <user> (<object>)
			  (id #:init-keyword #:id
				  #:getter user/get-id
				  #:accessor user/id)
			  (handle #:init-keyword #:handle
					  #:getter user/get-handle
					  #:setter user/set-handle!
					  #:accessor user/handle)
			  (password-hash #:init-keyword #:password-hash
							 #:getter user/get-password-hash
							 #:setter user/set-password-hash!
							 #:accessor user/password-hash))

(define-method (user/create (user <user>) db)
	(if (user/id user)
			(db/query db
						    (format #f "INSERT INTO Users (id, handle, password_hash) VALUES (~d, '~a', '~a');"
                        (user/id user)
                        (user/handle user)
                        (user/password-hash user)))
			;; TODO: Get the id of the inserted row
			(db/query db
						    (format #f "INSERT INTO Users (handle, password_hash) VALUES ('~a', '~a');"
						            (user/handle user)
                        (user/password-hash user)))))

(define-method (user/save (user <user>) db)
	(if (user/id user)
			(db/query db
						    (format #f "UPDATE Users SET handle = '~a', password_hash = '~a' WHERE id = ~a;"
                        (user/handle user)
                        (user/password-hash user)
                        (user/id user)))
			;; TODO: Get the id of the inserted row
			(user/create user db)))

(define (user/from-row result-row)
  (let ((row (if (vector? result-row) (vector->list result-row) result-row)))
	  (let ((-id (car row))
		      (-rest (cdr row)))
	    (let ((-handle (car -rest))
			      (-rest (cdr -rest)))
		    (let ((-password-hash (car -rest))
			        (-rest (cdr -rest)))
		  (make-instance <user>
						         #:id (if (string? -id) (string->number -id) -id)
						         #:handle -handle
						         #:password-hash -password-hash))))))

(define (user/read-by-id db id)
  (car (db/query db
				 (format #f "SELECT * FROM Users WHERE id = '~a';"
                 id)
				 #:row-handler user/from-row)))

(define (user/read-all db)
  (db/query db
			"SELECT * FROM Users;"
			#:parameters '()
			#:row-handler user/from-row))

(define (user/get-user-by-password db password)
  (let ((result (db/query db
						  (format #f "SELECT * FROM Users WHERE password_hash = '~a';"
                      password)
						  #:row-handler user/from-row)))
	(if (null? result)
	  #f
	  (car result))))

(define-class <post> (<object>)
			  (id #:init-keyword #:id
				  #:getter post/get-id
				  #:accessor post/id)
			  (content #:init-keyword #:content
					   #:getter post/get-content
					   #:setter post/set-content!
					   #:accessor post/content)
			  (poster-id #:init-keyword #:poster-id
					   #:getter post/get-poster-id
					   #:setter post/set-poster-id!
					   #:accessor post/poster-id))

(define-method (post->list (post <post>))
			   `((id ,(post/id post))
				 (content ,(post/content post))
				 (poster-id ,(post/poster-id post))))

(define-method (post/delete (post <post>) db)
			   (format #t "TODO: models.scm post/delete~%"))

(define-method (post/save (post <post>) db)
			   (if (post/id post)
             (begin
				       (db/query db
						             (format #f "UPDATE Posts SET content = '~a', poster = ~d WHERE id = ~d"
                                 (post/content post)
                                 (post/poster-id post)
						                     (post/id post)))
						             #:row-handler post/from-row)
				     (db/query db
						           (format #f "INSERT INTO Posts (content, poster) VALUES ('~a', ~d);"
                               (post/content post)
                               (post/poster-id post))
						           #:row-handler post/from-row)))

(define (post/from-row result-row)
  (let ((row (if (vector? result-row) (vector->list result-row) result-row)))
	  (let ((-id (car row))
		      (-rest (cdr row)))
	    (let ((-content (car -rest))
			      (-rest (cdr -rest)))
		    (let ((-poster-id (car -rest)))
		      (make-instance <post>
						             #:id (if (string? -id) (string->number -id) -id)
						             #:content -content
						             #:poster-id -poster-id))))))

(define (post/read-all db)
  (db/query db "SELECT * FROM posts;"
            #:row-handler post/from-row))

