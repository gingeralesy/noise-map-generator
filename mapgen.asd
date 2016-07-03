(in-package #:cl-user)
(asdf:defsystem mapgen
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "For experimenting with map generation."
  :components ((:file "package")
               (:file "fast-set-pixel")
               (:file "main")
               (:file "mapgen"))
  :depends-on (:verbose
               :qtools
               :qtcore
               :qtgui))
