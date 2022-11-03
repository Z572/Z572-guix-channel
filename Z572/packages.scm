(define-module (Z572 packages)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (package* modifyed-package))

(define-syntax package*
  (lambda (x)
    (syntax-case x ()
      ((_ (it origin) (others ...) ...)
       #'(let ((it origin))
           (package
             (inherit it)
             (others ...) ...)))
      ((_ () (others ...) ...)
       #'(package (others ...) ...)))))

(define-macro (modifyed-package-helper . rest)
  (let ((inh (car rest))
        (os (cdr rest)))
    `(modifyed-package ,inh
       ,@(map (match-lambda
                (((symbol changed-name) sexp)
                 (list (list symbol changed-name) sexp))
                ((symbol sexp) (list (list symbol symbol) sexp)))
              os))))

(define-syntax modifyed-package
  (lambda (x)
    (syntax-case x ()
      ((_ (it origin) ((symbol changed-name) b) ...)
       #'(let ((type (record-type-descriptor origin)))
           (package* (it origin)
             (symbol (let ((changed-name
                              ;;; hack to thunked and delayed field
                            (let ((o ((record-accessor type 'symbol) origin)))
                              (cond ((procedure? o)
                                     (o origin))
                                    ((promise? o)
                                     (force o))
                                    (else o))))) b)) ...)))
      ((_ (it origin) (o b) ...)
       #'(modifyed-package-helper (it origin) (o b) ...)))))
