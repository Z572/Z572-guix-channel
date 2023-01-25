(define-module (Z572 packages emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages text-editors)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix git)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-next-29
  (let ((base emacs-next-pgtk)
        (commit "8b87d095acfb23b527f955873a59dd9c13ffc9b4")
        (revision "16"))
    (package
      (inherit base)
      (name "emacs-next-29")
      (version (git-version "29.0.5" revision commit))
      (source (origin
                (inherit (package-source base))
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacs.git/")
                      (commit commit)))
                (sha256
                 (base32
                  "1p8aqfzxz22gycp4d5k3kwkm2wd7ja2hjmq4j3h15jpa6q5pr7qa"))
                (file-name (git-file-name name version))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          `(cons* "--with-tree-sitter" ,flags))))
      (inputs (modify-inputs (package-inputs base)
                             (prepend libwebp xinput sqlite tree-sitter))))))
