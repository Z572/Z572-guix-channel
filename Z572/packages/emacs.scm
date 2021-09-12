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
  (let ((commit "0e3384126893807ad5df4dbe24361c22e5d546d9")
        (revision "0"))
    (package
      (inherit emacs-native-comp)
      (name "emacs-with-tree-sitter")
      (version (git-version "28.0.50" revision commit))
      (source
       ;; (git-checkout
       ;;  (url "https://github.com/casouri/emacs")
       ;;  (branch "ts"))
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/casouri/emacs")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0q92bjpy44wdg4p2irmis3sj9c5a214cfb18hndfayzlmcir46ah"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-native-comp)
         ((#:configure-flags flags ''())
          `(cons* "--with-tree-sitter" ,flags))))
      (inputs
       `(("tree-sitter" ,tree-sitter)
         ,@(package-inputs emacs-native-comp))))))
