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

(define-public emacs-next-30
  (let ((base emacs-next-pgtk)
        (commit "ab7c2f809219b0c29e7ee2b5ac66f18b0e657080")
        (revision "0"))
    (package
      (inherit base)
      (name "emacs-next-30")
      (version (git-version "30.0.5" revision commit))
      (source (origin
                (inherit (package-source base))
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacs.git/")
                      (commit commit)))
                (sha256
                 (base32
                  "10d6ihw549ivv6mwssy02qdmsi4vf28yyh81581prflgrz9r5r26"))
                (file-name (git-file-name name version))))
      (inputs (modify-inputs (package-inputs base)
                             (prepend libwebp))))))
