(define-module (Z572 ci cross-rv64)
  #:use-module (gnu ci)
  #:export (cuirass-jobs))

(define (cuirass-jobs store arguments)
  (append-map (lambda (target)
                (map (lambda (package)
                       (package-cross-job store (job-name package)
                                          package target "x86_64-linux"))
                     (packages-to-cross-build target)))
              (list "riscv64-linux-gnu")))
