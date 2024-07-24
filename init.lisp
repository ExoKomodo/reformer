;; Load quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(require :asdf)
(require :uiop)

;; Register local project
(let ((current-dir (uiop/os:getcwd)))
  (pushnew current-dir asdf:*central-registry* :test 'equal))

;; Install project deps
(ql:quickload :reformer)
(asdf:load-system :reformer)

;; Run entrypoint
(reformer:main)
