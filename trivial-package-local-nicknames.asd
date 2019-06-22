;;;; trivial-package-local-nicknames.asd

(asdf:defsystem #:trivial-package-local-nicknames
  :description "Portability library for package-local nicknames"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.1"
  :serial t
  :components ((:file "trivial-package-local-nicknames")))

(defmethod asdf:perform
    ((o asdf:test-op)
     (c (eql (asdf:find-system ':trivial-package-local-nicknames))))
  (asdf:load-system :package-local-nicknames-tests)
  (uiop:symbol-call :package-local-nicknames-tests :run nil))
