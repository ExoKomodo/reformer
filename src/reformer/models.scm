(define-module (reformer models)
  #:export (<post> post/content post/get-content post/set-content
                   post/user post/get-user post/set-user
                   post/delete post/read-all post/save
            <user> user/handle user/get-handle user/set-handle))

(use-modules (oop goops)
             (external sqlite3))

(define-class <user> (<object>)
  (handle #:init-keyword #:handle
          #:getter user/get-handle
          #:setter user/set-handle
          #:accessor user/handle))

(define-class <post> (<object>)
  (content #:init-keyword #:content
           #:getter post/get-content
           #:setter post/set-content
           #:accessor post/content)
  (user #:init-keyword #:user
          #:init-form (make-instance <user> #:handle "DEFAULT")
          #:getter post/get-user
          #:setter post/set-user
          #:accessor post/user))

(define-method (post/delete (post <post>) db)
  (format #t "TODO: models.scm post/delete~%"))

(define-method (post/save (post <post>) db)
  (format #t "TODO: models.scm post/save~%"))

(define (post/from-row row)
  (let ((-id (car row))
        (rest (cdr row)))
    (let ((-content (car rest))
          (rest (cdr rest)))
      (let ((-user (car rest)))
        (format #t "To: ~s ~s ~s~%" -id -content -user)))))

(define (post/read-all db)
  (let ((rows (sqlite-exec* db
                           "SELECT * FROM Posts"
                           #:parameters '()
                           #:row-handler (lambda (x) x))))
    (for-each (lambda (row) (begin
                              (format #t "Row: ~s~%" row)
                              (post/from-row row)))
              rows)
    rows))

