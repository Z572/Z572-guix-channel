(define-module (Z572 packages linux)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:export (make-linux-cjktty-patch
            make-linux-with-cjktty-patch))

(define make-linux-libre*
  (@@ (gnu packages linux) make-linux-libre*))
(define computed-origin-method
  (@@ (guix packages) computed-origin-method))
(define source-with-patches
  (@@ (gnu packages linux) source-with-patches))

(define* (make-linux-cjktty-patch version #:key
                                  (major (version-major version))
                                  (version-in-patch-name (version-major+minor version))
                                  (commit "55a8845a449e742d79354fce0242339d68ea2bf9")
                                  (hash "1y85fj06by4v3x63wyyf02gz1m9yqsd2m3kjc6acp1brg7r7bcri")
                                  (revision "3"))
  (let ((orig (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zhmars/cjktty-patches")
                      (commit commit)))
                (file-name (git-file-name
                            "cjktty-patches"
                            (git-version "0" revision commit)))
                (sha256 (base32 hash)))))
    (origin
      (method computed-origin-method)
      (file-name (string-append "cjktty-" version-in-patch-name ".patch" ))
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules '((guix build utils))
           #~(begin
               (use-modules (guix build utils))
               (with-directory-excursion
                   (string-append
                    #$orig
                    "/v" #$major ".x")
                 (copy-file (string-append
                             "cjktty-"
                             #$version-in-patch-name ".patch") #$output)))))))))

(define kernel-config
  (@@ (gnu packages linux) kernel-config))

(define %default-extra-linux-options
  (@@ (gnu packages linux) %default-extra-linux-options))

(define* (make-linux-with-cjktty-patch linux
                                       #:key
                                       (extra-options (list))
                                       (configuration-file kernel-config))
  (package (inherit (make-linux-libre*
                     (package-version linux)
                     ""
                     (source-with-patches
                      (package-source linux)
                      (list (make-linux-cjktty-patch
                             (package-version linux))))
                     '("x86_64-linux")
                     #:configuration-file configuration-file
                     #:extra-options
                     (append
                      `(("CONFIG_MT7921E" . #t)
                        ("CONFIG_FONT_CJK_16x16" . #t)
                        ,@extra-options)
                      %default-extra-linux-options)))
           (name "linux-with-cjktty-patch")))
