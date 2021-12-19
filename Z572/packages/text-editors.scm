(define-module (Z572 packages text-editors)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages text-editors))

(define-public texmacs-with-guile3
  (let ((commit "77d3fa5df0d4a7aa1def6a956eb2972ef173ac33")
        (revision "1"))
    (package
      (inherit texmacs)
      (name "texmacs-with-guile3")
      (version (git-version "2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mgubi/texmacs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1f95w66da13hkk2svfxgmz5arqy968hvhx9jlwmv8gc04swdcxjk"))))
      (build-system gnu-build-system)
      (inputs
       (modify-inputs (package-inputs texmacs)
         (replace "guile" guile-3.0-latest)))
      (arguments (substitute-keyword-arguments (package-arguments texmacs)
                   ((#:phases phases)
                    `(modify-phases ,phases
                       (add-after 'unpack 'chdir-src
                         (lambda _ (chdir "src")))
                       (add-before 'reset-gzip-timestamps 'make-files-writable
                         (lambda* (#:key outputs #:allow-other-keys)
                           ;; Make sure .gz files are writable so that the
                           ;; 'reset-gzip-timestamps' phase can do its work.
                           (let ((out (assoc-ref outputs "out")))
                             (for-each make-file-writable
                                       (find-files out "\\.gz$")))))))
                   ((#:configure-flags flags ''())
                    `(cons "--enable-guile2" ,flags)))))))
