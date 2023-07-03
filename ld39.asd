(asdf:defsystem ld39
  :components ((:file "package")
               (:file "base")
               (:file "objects")
               (:file "light-timer")
               (:file "collision")
               (:file "player")
               (:file "editor")
               (:file "main"))
  :depends-on (:trial-glfw
               :array-utils
               #+harmony :harmony-simple)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ld39"
  :entry-point "ld39:launch")
