(define-module (reformer models)
	#:export (<post> post/id post/get-id
								   post/content post/get-content post/set-content!
								   post/user-id post/get-user-id post/set-user-id!
								   post/delete post/save
								   post->list
					         <user> user/handle user/get-handle user/set-handle!
                   user/make
								   user/id user/get-id
								   user/save
                   user/create
                   user/auth/generate-code
                   user/auth/send-code
                   user/auth/verify-code
								   define-foreign-type)

  #:use-module (ice-9 format)
  #:use-module (oop goops)
	#:use-module (reformer db)
	#:use-module (reformer messaging)
	#:use-module (system foreign))

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
					#:accessor user/handle))

(define* (user/make #:key id handle)
  (make-instance <user>
						     #:id (if (string? id) (string->number id) id)
						     #:handle handle))

(define-public (user/from-row result-row)
  (let ((row (if (vector? result-row) (vector->list result-row) result-row)))
	  (let ((-id (car row))
		      (-rest (cdr row)))
	    (let ((-handle (car -rest))
			      (-rest (cdr -rest)))
		    (user/make #:id -id #:handle -handle)))))

(define-public (user/read-by-id db id)
  (car (db/query db
				         (format #f "SELECT * FROM Users WHERE id = '~a';"
                         id)
				         #:row-handler user/from-row)))

(define-public (user/read-all db)
  (db/query db
			      "SELECT * FROM Users;"
			      #:parameters '()
			      #:row-handler user/from-row))

(define-public (user/get-user-by-handle db handle)
  (let ((result (db/query db
						              (format #f "SELECT * FROM Users WHERE handle = '~a';"
                                  handle)
						              #:row-handler user/from-row)))
	  (if (null? result)
	      #f
	      (car result))))

(define-method (user/create (user <user>) db)
  (if (user/id user)
		  (db/query db
						    (format #f "INSERT INTO Users (id, handle) VALUES (~d, '~a');"
                        (user/id user)
                        (user/handle user)))
		  ;; TODO: Get the id of the inserted row
		  (db/query db
						    (format #f "INSERT INTO Users (handle) VALUES ('~a');"
						            (user/handle user)))))

(define-method (user/save (user <user>) db)
  (if (user/id user)
		  (db/query db
						    (format #f "UPDATE Users SET handle = '~a' WHERE id = ~a;"
                        (user/handle user)
                        (user/id user)))
		  ;; TODO: Get the id of the inserted row
		  (user/create user db)))

(define-method (user/auth/generate-code (user <user>))
  (list->string (map (lambda (n)
                       (let ([random-n (random (if (>= n 26) 26 n) (random-state-from-platform))]
                             [starting-char (cond
                                             ((eq? n 10) (char->integer #\0))
                                             ((eq? n 26) (char->integer #\a))
                                             ((eq? n 52) (char->integer #\A)))])
                         (integer->char (+ starting-char random-n))))
                     '(10 26 52 52 26 10 10 26 52 10))))

(define login-codes '())

(define-method (user/auth/send-code (user <user>) code)
  (format #t "@~a: Sending login code~%" (user/handle user))
  (set! login-codes (assoc-set! login-codes (user/id user) `((code . ,code)
                                                             (created . ,(current-time)))))
  (send/email (format #f "~a@gmail.com" (user/handle user))
              "Reformer: Login code"
              (format #f "Login code: ~a" code)))

(define-method (user/auth/verify-code (user <user>) code)
  (format #t "@~a: Verifying login code~%" (user/handle user))
  (if (string? code)
      (let* ([login-info (assoc-ref login-codes (user/id user))]
             [login-code (assoc-ref login-info 'code)])
        (if (string=? login-code code)
            (begin
              (format #t "@~a: Good login code~%" (user/handle user))
              #t)
            (begin
              (format #t "@~a: Bad login code=~a~%" (user/handle user) code)
              #f)))
      (begin
        (format #t "@~a: Invalid type for code=~a~%" (user/handle user) code)
        #f)))

(define-class <post> (<object>)
  (id #:init-keyword #:id
		  #:getter post/get-id
		  #:accessor post/id)
  (content #:init-keyword #:content
				   #:getter post/get-content
				   #:setter post/set-content!
				   #:accessor post/content)
  (user-id #:init-keyword #:user-id
				   #:getter post/get-user-id
				   #:setter post/set-user-id!
				   #:accessor post/user-id))

(define-public (post/from-row result-row)
  (let ((row (if (vector? result-row) (vector->list result-row) result-row)))
	  (let ((-id (car row))
		      (-rest (cdr row)))
	    (let ((-content (car -rest))
			      (-rest (cdr -rest)))
		    (let ((-user-id (car -rest)))
		      (make-instance <post>
						             #:id (if (string? -id) (string->number -id) -id)
						             #:content -content
						             #:user-id -user-id))))))

(define-public (post/read-all db)
  (db/query db "SELECT * FROM posts;"
          #:row-handler post/from-row))

(define-method (post->list (post <post>))
`((id ,(post/id post))
	(content ,(post/content post))
	(user-id ,(post/user-id post))))

(define-method (post/delete (post <post>) db)
(format #t "TODO: models.scm post/delete~%"))

(define-method (post/save (post <post>) db)
(if (post/id post)
    (begin
			(db/query db
						    (format #f "UPDATE Posts SET content = '~a', usr = ~d WHERE id = ~d"
                        (post/content post)
                        (post/user-id post)
						            (post/id post)))
			#:row-handler post/from-row)
		(db/query db
						  (format #f "INSERT INTO Posts (content, usr) VALUES ('~a', ~d);"
                      (post/content post)
                      (post/user-id post))
						  #:row-handler post/from-row)))
