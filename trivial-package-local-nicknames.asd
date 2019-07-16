;;;; trivial-package-local-nicknames.asd

(asdf:defsystem #:trivial-package-local-nicknames
  :description "Portability library for package-local nicknames"
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "Public domain"
  :version "0.2"
  :serial t
  :components ((:file "trivial-package-local-nicknames")))

(defmethod asdf:perform
    ((o asdf:test-op)
     (c (eql (asdf:find-system ':trivial-package-local-nicknames))))
  (uiop:symbol-call :trivial-package-local-nicknames :run))
