(define-module (Z572 utils)
  #:use-module (gnu packages)
  #:export (symbol->package))

(define symbol->package
  (compose list specification->package+output symbol->string))
