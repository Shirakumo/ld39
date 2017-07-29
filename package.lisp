#|
 This file is a part of ld39
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:ld39
  (:nicknames #:org.shirakumo.fraf.ld39)
  (:use #:cl+trial)
  (:shadow #:launch #:main)
  (:export #:launch))
