;;;; ▀▄▀▄▀▄██▓▒░
;;;;   ╭────────────╮
;;;;   │ BUILD.LISP │
;;;;   ╰────────────╯
;;;;
;;;; Use this file to create a binary.
;;;; Usage: D:\tvossen\Applications\ccl\wx86cl64 --load build.lisp
;;;; Usage: /Users/tvossen/Applications/ccl/dx86cl64 --load build.lisp

;;; Add the source folder to the *central-registry*
;;;
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/cl-csv/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/iterate/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/cl-interpol/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/cl-unicode/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/cl-ppcre/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/flexi-streams/"
                asdf:*central-registry*)
#+windows (push #p"d:/tvossen/Common-Lisp/latest-price/Libraries/trivial-gray-streams/"
                asdf:*central-registry*)

#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/cl-csv/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/iterate/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/cl-interpol/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/cl-unicode/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/cl-ppcre/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/flexi-streams/"
               asdf:*central-registry*)
#+darwin (push #p"/Users/tvossen/Common-Lisp/latest-price/Libraries/trivial-gray-streams/"
               asdf:*central-registry*)



;;; Load the systems into the lisp image
;;;
(asdf:load-system :cl-csv)
(asdf:load-system :latest-price)

;;; Set the binary name for Windows
;;;
#+windows (defparameter *app-name* "latest-price.exe")

;;; Set the binary name for non Windows platforms
;;;
#+darwin (defparameter *app-name* "latest-price.osx")            

;;; Write out the binary image and quit CCL
;;;
(save-application *app-name*
                  :toplevel-function #'lp:main
                  :prepend-kernel t
                  :error-handler :quit)
