(use-modules (reformer models)
             (rnrs base))

(let* ([user (user/make #:id 1 #:handle "exokomodo")]
       [good-code (user/auth/generate-code user)]
       [bad-code "badcode"])
  (user/auth/send-code user good-code)

  (assert (user/auth/verify-code user good-code))
  (assert (not (user/auth/verify-code user bad-code))))

