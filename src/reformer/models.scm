(define-module (reformer models)
  #:export (<post> post/id post/get-id
                   post/content post/get-content post/set-content!
                   post/user-id post/get-user-id post/set-user-id!
                   post/delete post/read-all post/save
                   post->list
            <user> user/handle user/get-handle user/set-handle!
            user/id user/get-id
            user/password-hash user/get-password-hash
            user/get-user-by-password
            user/read-by-id
            user/read-all
            user/save))

(use-modules (oop goops)
             (external sqlite3))

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

(define-method (user/save (user <user>) db)
  (if (user/id user)
      (sqlite-exec* db
                    "UPDATE Users SET handle = :handle, password_hash = :password WHERE id = :id"
                    #:parameters `((id . ,(user/id user))
                                   (handle . ,(user/handle user))
                                   (password . ,(user/password-hash)))
                    #:row-handler user/from-row-vector)
      ;; TODO: Get the id of the inserted row
      (sqlite-exec* db
                    "INSERT INTO Users ('handle', 'password_hash') VALUES (:handle, :password)"
                    #:parameters `((handle . ,(user/handle user))
                                   (password . ,(user/password-hash user)))
                    #:row-handler user/from-row-vector)))

(define (user/from-row-vector row-vector)
  (let ((row (vector->list row-vector)))
    (let ((-id (car row))
          (-rest (cdr row)))
      (let ((-handle (car -rest))
            (-rest (cdr -rest)))
        (let ((-password-hash (car -rest))
              (-rest (cdr -rest)))
          (make-instance <user>
                         #:id -id
                         #:handle -handle
                         #:password-hash -password-hash))))))

(define (user/read-by-id db id)
  (car (sqlite-exec* db
                     "SELECT * FROM Users WHERE id = :id"
                     #:parameters `((id . ,id))
                     #:row-handler user/from-row-vector)))

(define (user/read-all db)
  (sqlite-exec* db
                "SELECT * FROM Users"
                #:parameters '()
                #:row-handler user/from-row-vector))

(define (user/get-user-by-password db password)
  (let ((result (sqlite-exec* db
                              "SELECT * FROM Users WHERE password_hash = :password"
                              #:parameters `((password . ,password))
                              #:row-handler user/from-row-vector)))
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
  (user-id #:init-keyword #:user-id
           #:getter post/get-user-id
           #:setter post/set-user-id!
           #:accessor post/user-id))

(define-method (post->list (post <post>))
  `((id ,(post/id post))
    (content ,(post/content post))
    (user-id ,(post/user-id post))))

(define-method (post/delete (post <post>) db)
  (format #t "TODO: models.scm post/delete~%"))

(define-method (post/save (post <post>) db)
  (if (post/id post)
      (sqlite-exec* db
                    "UPDATE Posts SET content = :content, user = :user WHERE id = :id"
                    #:parameters `((id . ,(post/id post))
                                   (content . ,(post/content post))
                                   (user . ,(post/user-id post)))
                    #:row-handler post/from-row-vector)
      (sqlite-exec* db
                    "INSERT INTO Posts ('content', 'user') VALUES (:content, :user)"
                    #:parameters `((content . ,(post/content post))
                                   (user . ,(post/user-id post)))
                    #:row-handler post/from-row-vector)))

(define (post/from-row-vector row-vector)
  (let ((row (vector->list row-vector)))
    (let ((-id (car row))
          (-rest (cdr row)))
      (let ((-content (car -rest))
            (-rest (cdr -rest)))
        (let ((-user-id (car -rest)))
          (make-instance <post>
                         #:id -id
                         #:content -content
                         #:user-id -user-id))))))

(define (post/read-all db)
  (sqlite-exec* db
                "SELECT * FROM Posts"
                #:parameters '()
                #:row-handler post/from-row-vector))

