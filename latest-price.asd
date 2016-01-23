;;;
;;; latest-price.asd
;;;

(in-package :asdf-user)

(asdf:defsystem :LATEST-PRICE
  :description "Keeps only the newest prices of csv input file."
  :version "0.0.2"
  :author "Thomas Voßen, CrimsonMagic.net"
  :maintainer "Thomas Voßen, CrimsonMagic.net"
  :components ((:file "package")
               (:file "latest-price")))
