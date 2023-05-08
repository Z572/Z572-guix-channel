(define-module (Z572 packages u-boot)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix utils)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages tls)
  #:use-module (guix gexp))

(define-public u-boot-lichee-rv-dock
  (let ((commit "528ae9bc6c55edd3ffe642734b4132a8246ea777")
        (base (make-u-boot-package
               "lichee_rv_dock"
               "riscv64-linux-gnu")))
    (package
      (inherit base)
      (source (origin
                (inherit (package-source base))
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/smaeul/u-boot")
                      (commit commit)))
                (file-name (git-file-name
                            (package-name base)
                            (package-version base)))
                (patches
                 (cons (local-file "./patches/u-boot-disable-openssl.patch")
                       (filter (negate (cut string-contains <> "openssl"))
                               (origin-patches (package-source base)))))
                (sha256
                 (base32
                  "1rfk2d9wxxmf8ypvmwq07g1vifkvzy2nzs4mdwdgxsadlhi8dn9s"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "OPENSBI" (search-input-file inputs
                                                       "fw_dynamic.bin"))))
              ;; (add-after 'build 'build-toc1
              ;;                 (lambda _
              ;;                   (call-with-output-file "toc1.cfg"
              ;;                     (lambda (port)
              ;;                       (format port
              ;;                               "[opensbi]
              ;; file = ~a
              ;; addr = 0x40000000
              ;; [dtb]
              ;; file = arch/riscv/dts/sun20i-d1-lichee-rv-dock.dtb
              ;; addr = 0x44000000
              ;; [u-boot]
              ;; file = u-boot-nodtb.bin
              ;; addr = 0x4a000000"
              ;;                               (getenv "OPENSBI"))))
              ;;                   (invoke "./tools/mkimage" "-T" "sunxi_toc1" "-d" "toc1.cfg" "u-boot.toc1")))
              ;; (add-after 'install 'install-toc1)
              ))))
      (inputs
       (modify-inputs (package-inputs base)
         (append opensbi-generic))))))
