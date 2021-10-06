(define-module (Z572 packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix git)
  #:use-module (Z572 packages tree-sitter)
  #:use-module (flat packages emacs))

(define-public emacs-next-pgtk-latest
  (package
    (inherit emacs-pgtk-native-comp)
    (name "emacs-next-pgtk-latest")
    (version "git.pgtk")
    (source
     (git-checkout
      (url "https://git.savannah.gnu.org/git/emacs.git/")
      (branch "feature/pgtk")))))

(define-public emacs-with-tree-sitter
  (let ((commit "106d050ad5d02f673f8a089e1f10c1eacfedd124")
        (revision "4"))
    (package
      (inherit emacs-native-comp)
      (name "emacs-with-tree-sitter")
      (version (git-version "28.0.50" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/casouri/emacs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1qpnha1cy1mkj373liw2wmn727hd2znim3c71n21fia7d6qq8b79"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-native-comp)
         ((#:configure-flags flags ''())
          `(cons* "--with-tree-sitter" ,flags))))
      (inputs
       `(("tree-sitter" ,tree-sitter)
         ,@(package-inputs emacs-native-comp)))
      (native-search-paths
       (list (search-path-specification
              (variable "LD_LIBRARY_PATH")
              (files '("lib/emacs"))))))))

