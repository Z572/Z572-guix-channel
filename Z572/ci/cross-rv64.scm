(define-module (Z572 ci cross-rv64)
  #:use-module (gnu ci)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)
  (append-map (lambda (target)
                (map (lambda (package)
                       ((@@ (gnu ci) package-cross-job)
                        store
                        (package-name package)
                        package target "x86_64-linux"))
                     ((@@ (gnu ci) all-packages))))
              (list "riscv64-linux-gnu")))
