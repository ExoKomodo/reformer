(require :asdf)

(in-package :asdf-user)

(defsystem "reformer"
  :description "reformer: Reformed christian social network and resource page."
  :version "0.0.1"
  :author "James Orson <jamesaorson@gmail.com>; Nathan Barlow <nathanjbarlow@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:clack
               #:cl-who
               #:parenscript
               #:uiop)
  :components ((:module "src"
                :components
                ((:file "reformer")))))
