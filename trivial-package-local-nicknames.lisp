;;;; trivial-package-local-nicknames.lisp

(defpackage #:trivial-package-local-nicknames
  (:use #:cl)
  (:import-from
   #+sbcl  #:sb-ext
   #+ccl   #:ccl
   #+ecl   #:ext
   #+clasp #:ext
   #+abcl  #:ext
   #+lispworks #:hcl
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname)
  (:export
   #:package-local-nicknames
   #:package-locally-nicknamed-by-list
   #:add-package-local-nickname
   #:remove-package-local-nickname))

(in-package #:trivial-package-local-nicknames)

;;; Test data

(progn
  (defparameter +test-data+
    #+sbcl  '(:sb-ext :sb :exit)
    #+ccl   '(:ccl    :cc :quit)
    #+ecl   '(:ext    :ex :exit)
    #+abcl  '(:ext    :ex :quit)
    #+clasp '(:core   :ex :quit)
    #+lispworks '(:lispworks  :ex :quit))

  (defparameter +pkg-name+ (first +test-data+))
  (defparameter +nn-name+ (second +test-data+))
  (defparameter +sym-name+ (third +test-data+))

  (defparameter +pkg-sname+ (string +pkg-name+))
  (defparameter +nn-sname+ (string +nn-name+))
  (defparameter +sym-sname+ (string +sym-name+))
  (defparameter +sym-fullname+ (concatenate 'string +pkg-sname+ ":" +sym-sname+))
  (defparameter +sym-fullnickname+ (concatenate 'string +nn-sname+ ":" +sym-sname+))
  (defparameter +sym+ (or (find-symbol +sym-sname+ +pkg-name+)
                          (error "Symbol not found while loading tests: check +SYM+ binding."))))

;;; Test runner

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *tests* '()))

(defmacro define-test (name &body body)
  `(progn
     (defun ,name () ,@body)
     (pushnew ',name *tests*)
     ',name))

(defun run (&optional (ignore-errors t))
  (let ((errors '()))
    (dolist (test *tests*)
      (if ignore-errors
          (handler-case (funcall test)
            (error (e)
              (format t ";; ~A:~%;;;; ~A~%" test e)
              (push e errors)))
          (funcall test)))
    (format t ";;~%;; ~D tests run, ~D failures."
            (length *tests*) (length errors))
    (null errors)))

;;; Test code

(defun reset-test-packages ()
  (#+sbcl sb-ext:without-package-locks
   #-sbcl progn
   (when (find-package :package-local-nicknames-test-1)
     (delete-package :package-local-nicknames-test-1))
   (when (find-package :package-local-nicknames-test-2)
     (delete-package :package-local-nicknames-test-2)))
  (eval `(defpackage :package-local-nicknames-test-1 (:use)
                     (:local-nicknames (:l :cl) (,+nn-name+ ,+pkg-name+))))
  (eval `(defpackage :package-local-nicknames-test-2 (:use)
                     (:export "CONS"))))

(define-test test-package-local-nicknames-introspection
  (reset-test-packages)
  (dolist (p '("KEYWORD" "COMMON-LISP" "COMMON-LISP-USER"
               :package-local-nicknames-test-1
               :package-local-nicknames-test-2))
    (let ((*package* (find-package p)))
      (let ((alist (package-local-nicknames :package-local-nicknames-test-1)))
        (assert (equal (cons "L" (find-package "CL")) (assoc "L" alist :test 'string=)))
        (assert (equal (cons +nn-sname+ (find-package +pkg-sname+))
                       (assoc +nn-sname+ alist :test 'string=)))
        (assert (eql 2 (length alist)))))))

(define-test test-package-local-nicknames-symbol-equality
  (reset-test-packages)
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (cons1 (find-symbol "CONS" :l))
          (cons1s (find-symbol "CONS" #\L))
          (exit0 (read-from-string +sym-fullname+))
          (exit1 (find-symbol +sym-sname+ +nn-name+)))
      (assert (eq 'cons cons0))
      (assert (eq 'cons cons1))
      (assert (eq 'cons cons1s))
      (assert (eq +sym+ exit0))
      (assert (eq +sym+ exit1)))))

(define-test test-package-local-nicknames-package-equality
  (reset-test-packages)
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cl (find-package :l))
          (cls (find-package #\L))
          (sb (find-package +nn-name+)))
      (assert (eq cl (find-package :common-lisp)))
      (assert (eq cls (find-package :common-lisp)))
      (assert (eq sb (find-package +pkg-name+))))))

(define-test test-package-local-nicknames-symbol-printing
  (reset-test-packages)
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (exit0 (read-from-string +sym-fullname+)))
      (assert (equal "L:CONS" (prin1-to-string cons0)))
      (assert (equal +sym-fullnickname+ (prin1-to-string exit0))))))

(define-test test-package-local-nicknames-nickname-collision
  (reset-test-packages)
  ;; Can't add same name twice for different global names.
  (assert (eq :oopsie
              (handler-case
                  (add-package-local-nickname :l :package-local-nicknames-test-2
                                              :package-local-nicknames-test-1)
                (package-error () :oopsie))))
  ;; ...but same name twice is OK.
  (add-package-local-nickname :l :cl :package-local-nicknames-test-1)
  (add-package-local-nickname #\L :cl :package-local-nicknames-test-1))

(define-test test-package-local-nicknames-nickname-removal
  (reset-test-packages)
  (assert (= 2 (length (package-local-nicknames :package-local-nicknames-test-1))))
  (assert (remove-package-local-nickname :l :package-local-nicknames-test-1))
  (assert (= 1 (length (package-local-nicknames :package-local-nicknames-test-1))))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (assert (not (find-package :l)))))

(define-test test-package-local-nicknames-nickname-removal-char
  (declare (optimize (debug 3) (speed 0)))
  (reset-test-packages)
  (assert (= 2 (length (package-local-nicknames :package-local-nicknames-test-1))))
  (assert (remove-package-local-nickname #\L :package-local-nicknames-test-1))
  (assert (= 1 (length (package-local-nicknames :package-local-nicknames-test-1))))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (assert (not (find-package :l)))))

(define-test test-package-local-nicknames-nickname-removal-remaining
  (reset-test-packages)
  (remove-package-local-nickname :l :package-local-nicknames-test-1)
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((exit0 (read-from-string +sym-fullname+))
          (exit1 (find-symbol +sym-sname+ +nn-name+))
          (sb (find-package +nn-name+)))
      (assert (eq +sym+ exit0))
      (assert (eq +sym+ exit1))
      (assert (equal +sym-fullnickname+ (prin1-to-string exit0)))
      (assert (eq sb (find-package +pkg-name+))))))

(define-test test-package-local-nicknames-nickname-removal-readd-another-symbol-equality
  (reset-test-packages)
  (assert (remove-package-local-nickname :l :package-local-nicknames-test-1))
  (assert (eq (find-package :package-local-nicknames-test-1)
              (add-package-local-nickname :l :package-local-nicknames-test-2
                                          :package-local-nicknames-test-1)))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (cons1 (find-symbol "CONS" :l))
          (exit0 (read-from-string +sym-fullnickname+))
          (exit1 (find-symbol +sym-sname+ +nn-name+)))
      (assert (eq cons0 cons1))
      (assert (not (eq 'cons cons0)))
      (assert (eq (find-symbol "CONS" :package-local-nicknames-test-2)
                  cons0))
      (assert (eq +sym+ exit0))
      (assert (eq +sym+ exit1)))))

(define-test test-package-local-nicknames-nickname-removal-readd-another-package-equality
  (reset-test-packages)
  (assert (remove-package-local-nickname :l :package-local-nicknames-test-1))
  (assert (eq (find-package :package-local-nicknames-test-1)
              (add-package-local-nickname :l :package-local-nicknames-test-2
                                          :package-local-nicknames-test-1)))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cl (find-package :l))
          (sb (find-package +nn-name+)))
      (assert (eq cl (find-package :package-local-nicknames-test-2)))
      (assert (eq sb (find-package +pkg-name+))))))

(define-test test-package-local-nicknames-nickname-removal-readd-another-symbol-printing
  (reset-test-packages)
  (assert (remove-package-local-nickname :l :package-local-nicknames-test-1))
  (assert (eq (find-package :package-local-nicknames-test-1)
              (add-package-local-nickname :l :package-local-nicknames-test-2
                                          :package-local-nicknames-test-1)))
  (let ((*package* (find-package :package-local-nicknames-test-1)))
    (let ((cons0 (read-from-string "L:CONS"))
          (exit0 (read-from-string +sym-fullnickname+)))
      (assert (equal "L:CONS" (prin1-to-string cons0)))
      (assert (equal +sym-fullnickname+ (prin1-to-string exit0))))))

#+sbcl
(define-test test-package-local-nicknames-package-locks
  ;; TODO Support for other implementations with package locks.
  (reset-test-packages)
  (progn
    (sb-ext:lock-package :package-local-nicknames-test-1)
    (assert (eq :package-oopsie
                (handler-case
                    (add-package-local-nickname :c :sb-c :package-local-nicknames-test-1)
                  (sb-ext:package-lock-violation ()
                    :package-oopsie))))
    (assert (eq :package-oopsie
                (handler-case
                    (remove-package-local-nickname :l :package-local-nicknames-test-1)
                  (sb-ext:package-lock-violation ()
                    :package-oopsie))))
    (sb-ext:unlock-package :package-local-nicknames-test-1)
    (add-package-local-nickname :c :sb-c :package-local-nicknames-test-1)
    (remove-package-local-nickname :l :package-local-nicknames-test-1)))

(defmacro with-tmp-packages (bindings &body body)
  `(let ,(mapcar #'car bindings)
     (unwind-protect
          (progn
            (setf ,@(apply #'append bindings))
            ,@body)
       ,@(mapcar (lambda (p)
                   `(when ,p (delete-package ,p)))
                 (mapcar #'car bindings)))))

(define-test test-delete-package-locally-nicknames-others
  (with-tmp-packages ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
                      (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
    (add-package-local-nickname :foo p2 p1)
    (assert (equal (list p1) (package-locally-nicknamed-by-list p2)))
    (delete-package p1)
    (assert (not (package-locally-nicknamed-by-list p2)))))

(define-test test-delete-package-locally-nicknamed-by-others
  (with-tmp-packages ((p1 (make-package "LOCALLY-NICKNAMES-OTHERS"))
                      (p2 (make-package "LOCALLY-NICKNAMED-BY-OTHERS")))
    (add-package-local-nickname :foo p2 p1)
    (assert (package-local-nicknames p1))
    (delete-package p2)
    (assert (not (package-local-nicknames p1)))))

(define-test test-own-name-as-local-nickname-cerror
  (with-tmp-packages ((p1 (make-package "OWN-NAME-AS-NICKNAME1"))
                      (p2 (make-package "OWN-NAME-AS-NICKNAME2")))
    (assert (eq :oopsie
                (handler-case
                    (add-package-local-nickname :own-name-as-nickname1 p2 p1)
                  (package-error () :oopsie))))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-name-as-nickname1 p2 p1))))

(define-test test-own-name-as-local-nickname-intern
  (with-tmp-packages ((p1 (make-package "OWN-NAME-AS-NICKNAME1"))
                      (p2 (make-package "OWN-NAME-AS-NICKNAME2")))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-name-as-nickname1 p2 p1))
    (assert (eq (intern "FOO" p2)
                (let ((*package* p1))
                  (intern "FOO" :own-name-as-nickname1))))
    (let ((sym (intern "BAR" p2))
          (lam '(lambda (x) (intern x :own-name-as-nickname1))))
      (dolist (p '("COMMON-LISP" "KEYWORD" "COMMON-LISP-USER"
                   "OWN-NAME-AS-NICKNAME1"
                   "OWN-NAME-AS-NICKNAME2"))
        (let ((*package* p1))
          (assert (eq sym (funcall
                           (let ((*package* (find-package p))) (compile nil lam))
                           "BAR"))
                  ()
                  "test-own-name-as-local-nickname-intern failed for p = ~s"
                  p))))))

(define-test test-own-nickname-as-local-nickname-cerror
  (with-tmp-packages ((p1 (make-package "OWN-NICKNAME-AS-NICKNAME1"
                                        :nicknames '("OWN-NICKNAME")))
                      (p2 (make-package "OWN-NICKNAME-AS-NICKNAME2")))
    (assert (eq :oopsie
                (handler-case
                    (add-package-local-nickname :own-nickname p2 p1)
                  (package-error () :oopsie))))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-nickname p2 p1))))

(define-test test-own-nickname-as-local-nickname-intern
  (with-tmp-packages ((p1 (make-package "OWN-NICKNAME-AS-NICKNAME1"
                                        :nicknames '("OWN-NICKNAME")))
                      (p2 (make-package "OWN-NICKNAME-AS-NICKNAME2")))
    (handler-bind ((package-error #'continue))
      (add-package-local-nickname :own-nickname p2 p1))
    (assert (eq (intern "FOO" p2)
                (let ((*package* p1))
                  (intern "FOO" :own-nickname))))
    (let ((sym (intern "BAR" p2))
          (lam '(lambda (x) (intern x :own-nickname)))
          (*package* p1))
      (dolist (p '("COMMON-LISP" "KEYWORD" "COMMON-LISP-USER"
                   "OWN-NICKNAME-AS-NICKNAME1"
                   "OWN-NICKNAME-AS-NICKNAME2"))
        (assert (eq sym
                    (funcall
                     (let ((*package* (find-package p))) (compile nil lam))
                     "BAR"))
                ()
                "test-own-nickname-as-local-nickname-intern failed on p = ~s"
                p)))))
