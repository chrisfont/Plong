;;;; plong.asd

(asdf:defsystem #:plong
  :description "Common Lisp Pong Clone"
  :author "Chris Font"
  :license "Don't use yet. Will license after deciding what is applicable"
  :depends-on (#:cl-glfw3 #:cl-opengl #:cl-glut)
  :serial t
  :components ((:file "package")
               (:file "plong")))
