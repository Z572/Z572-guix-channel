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
        (commit "350918e7be82fca046911073b360518173169255")
        (revision "15"))
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
                  "0j9xrq7nd00w3jdxhg11c5qpap51xv478n30s915sy3s6rkg2rzi"))
                (file-name (git-file-name name version))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          `(cons* "--with-tree-sitter" ,flags))))
      (inputs (modify-inputs (package-inputs base)
                             (prepend libwebp xinput sqlite tree-sitter))))))
