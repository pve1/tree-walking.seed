(asdf:defsystem :tree-walking.seed
  :description "Tree walking functions."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "tree-walking.seed"))
  :depends-on (#:alexandria
               #:package.seed))
