(define-module (Z572 packages desktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix utils))

(define-public gtkgreet
  (package
    (name "gtkgreet")
    (version "0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~kennylevinsen/gtkgreet")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nny83jz4acxnm7ddys7xqgzk5bjp0dicw9kjxkdkk3kvcavdkws"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list gtk+ gtk-layer-shell json-c))
    (home-page "https://git.sr.ht/~kennylevinsen/gtkgreet")
    (synopsis "GTK based greeter for greetd, to be run under cage or similar.")
    (description "GTK based greeter for greetd, to be run under cage or similar.")
    (license license:gpl3+)))
