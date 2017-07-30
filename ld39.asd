#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem ld39
  :components ((:file "package")
               (:file "base")
               (:file "objects")
               (:file "light-timer")
               (:file "collision")
               (:file "player")
               (:file "editor")
               (:file "main"))
  :depends-on (:trial
               :array-utils)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ld39"
  :entry-point "ld39:launch")
