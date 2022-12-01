(define-module (Z572 packages cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pkg-config))

(define-public immer
  (package
    (name "immer")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arximboldi/immer")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11km3l5h3rgsbj8yfyzk3fnx9na55l6zs2sxpx922yvlvs2blh27"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-gcc-unknow-flags
            (lambda* _
              (substitute* "CMakeLists.txt"
                (("-Wno-extended-offsetof -Wno-c\\+\\+17-extensions \
-Wno-c\\+\\+1z-extensions -Wno-unknown-warning-option")
                 ""))))
          (add-before 'check 'make-check
            (lambda* (#:key tests? parallel-build?
                      #:allow-other-keys)
              (when tests?
                (invoke "make" "check" "-j"
                        (if parallel-build?
                            (number->string (parallel-job-count))
                            "1"))))))))
    (native-inputs
     (list
      ;; for test
      libgc fmt-7 boost))
    (home-page "https://sinusoid.es/immer")
    (synopsis "Postmodern immutable and persistent data structures for C++")
    (description
     "immer is a library of persistent and immutable data structures
written in C++.  These enable whole new kinds of architectures for interactive
and concurrent programs of simplicity, correctness, and performance.")
    (license license:boost1.0)))
