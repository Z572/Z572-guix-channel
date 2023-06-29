(define-module (Z572 ci cross-rv64)
  #:use-module (ice-9 match)
  #:use-module (guix)
  #:use-module (gnu packages base)
  #:use-module (gnu packages)
  #:use-module (gnu ci)
  #:use-module (srfi srfi-1)
  #:export (cuirass-jobs))

(define* (package-job store job-name package system
                      #:key cross? target (suffix ""))
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  (let ((job-name (string-append job-name "." system suffix)))
    (parameterize ((%graft? #f))
      (let* ((drv (if cross?
                      (package-cross-derivation store package target system
                                                #:graft? #f)
                      (package-derivation store package system
                                          #:graft? #f)))
             (max-silent-time (or (assoc-ref (package-properties package)
                                             'max-silent-time)
                                  3600))
             (timeout (or (assoc-ref (package-properties package)
                                     'timeout)
                          72000)))
        (derivation->job job-name drv
                         #:max-silent-time max-silent-time
                         #:timeout timeout)))))
(define (package-cross-job store job-name package target system)
  "Return a job called TARGET.JOB-NAME that cross-builds PACKAGE for TARGET on
SYSTEM."
  (let ((name (string-append target "." job-name)))
    (package-job store name package system
                 #:cross? #t
                 #:target target)))
(define (all-packages)
  "Return the list of packages to build."
  (define (adjust package result)
    (cond ((package-replacement package)
           ;; XXX: If PACKAGE and its replacement have the same name/version,
           ;; then both Cuirass jobs will have the same name, which
           ;; effectively means that the second one will be ignored.  Thus,
           ;; return the replacement first.
           (cons* (package-replacement package)   ;build both
                  package
                  result))
          ((package-superseded package)
           result)                                ;don't build it
          (else
           (cons package result))))

  (fold-packages adjust
                 (fold adjust '()                 ;include base packages
                       (match (%final-inputs)
                         (((labels packages _ ...) ...)
                          packages)))
                 #:select? (lambda (p)
                             (supported-package? p "riscv64-linux"))))

(define (cuirass-jobs store arguments)
  (map (lambda (package)
         (package-cross-job
          store
          (package-name package)
          package "riscv64-linux-gnu" "x86_64-linux"))
       (all-packages)))
