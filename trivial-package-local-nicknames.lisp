;;;; package.lisp

(defpackage #:trivial-package-local-nicknames
  (:use #:cl)
  (:import-from
   #+sbcl #:sb-ext
   #+ccl  #:ccl
   #+ecl  #:ext
   #+abcl #:ext
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname)
  (:export
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname))
