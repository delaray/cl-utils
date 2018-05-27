(cl:defpackage #:asdf-module
  (:use #:cl #:asdf #:mini-module))

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (asdf:oos 'asdf:compile-op :postmodern))

(eval-when (:load-toplevel)
  (asdf:oos 'asdf:load-op :postmodern))
