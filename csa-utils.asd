;;;; csa-utils.asd

(asdf:defsystem #:csa-utils
  :serial t
  :description "Describe csa-utils here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
               #:cl-csv
               #:alexandria
               #:cl-ppcre
               #:anaphora
               #:xmls)
  :components ((:file "package")
               (:file "csa-utils")))

