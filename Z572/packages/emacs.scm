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
  #:use-module (flat packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-next-29
  (let ((emacs emacs-pgtk-native-comp)
        (commit "352fc739a1df259b1d2de6bc442465f344e44fec") (revision "11"))
    (package
      (inherit emacs)
      (name "emacs-next-29")
      (version (git-version "29.0.5" revision commit))
      (source (origin
                (inherit (package-source emacs))
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacs.git/")
                      (commit commit)))
                (sha256
                 (base32
                  "01w53jafz2li2r5dzvaj0lmjyv9bcrjlnbz7wg2c3pdm71m2hh9g"))
                (file-name (git-file-name name version))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs)
         ((#:configure-flags flags
           ''()) `(cons* "--with-xwidgets" "--with-xinput2"
                         ,flags))))
      (propagated-inputs (modify-inputs (package-propagated-inputs
                                         emacs)
                                        (prepend gsettings-desktop-schemas glib-networking)))
      (inputs (modify-inputs (package-inputs emacs)
                             (prepend libwebp xinput sqlite webkitgtk-with-libsoup2))))))

(define-public emacs-with-tree-sitter
  (let ((commit "b74eedc7042f7e90e6a9c79e9c58f832d6c71999")
        (revision "6"))
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
           "0k05p1x8nq19sk51rs7vm53xpycxwaf1xxsphdx8h41iys9309xj"))))
      (arguments
       (substitute-keyword-arguments (package-arguments emacs-native-comp)
         ((#:configure-flags flags ''())
          `(cons* "--with-tree-sitter" ,flags))))
      (inputs
       (modify-inputs (package-inputs emacs-native-comp)
                      (prepend tree-sitter)))
      (native-search-paths
       (cons (search-path-specification
              (variable "LD_LIBRARY_PATH")
              (files '("lib/emacs")))
             (package-native-search-paths emacs-native-comp))))))

(define* (make-emacs-tree-sitter-module
          lang version hash-string #:key (commit (string-append "v" version))
          (url (string-append
                "https://github.com/"
                "tree-sitter/tree-sitter-" lang)))
  (let ((p (origin
             (method git-fetch)
             (uri (git-reference
                   (url (string-append
                         "https://github.com/casouri/tree-sitter-module"))
                   (commit "ac4645e8589d280c2087f543584cd6f49348066e")))
             (file-name (string-append "tree-sitter-module-2"))
             (sha256 (base32 "0rm41xnmvvzdxnf7qnvkdpcssbkzgzjsq7w20pdva7svp84gi7zv")))))
    (package
      (name (string-append "emacs-tree-sitter-" lang "-module"))
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url url)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash-string))))
      (inputs `(("tree-sitter-module" ,p)
                ("linux-headers" ,linux-libre-headers)
                ("tree-sitter" ,tree-sitter)
                ("emacs" ,emacs-minimal)))
      (native-inputs `(("gcc" ,gcc)
                       ("glibc" ,glibc)
                       ("binutils" ,binutils)))
      (build-system trivial-build-system)
      (arguments
       `(#:modules
         ((guix build utils))
         #:builder
         (begin
           (use-modules
            (guix build utils)
            (ice-9 regex)
            (ice-9 textual-ports)
            (ice-9 format))
           (let* (;; names
                  (lang ,lang)
                  (lang-c (regexp-substitute/global #f "[-]" lang 'pre "_" 'post))
                  (soname (string-append "libtree-sitter-" lang ".so"))
                  ;; inputs files or dirs
                  (source (assoc-ref %build-inputs "source"))
                  (module (assoc-ref %build-inputs "tree-sitter-module"))
                  (binutils (assoc-ref %build-inputs "binutils"))
                  (grammar-file (string-append source "/grammar.js"))
                  ;; output files or dirs
                  (output (assoc-ref %outputs "out"))
                  (lib (string-append output "/lib/emacs"))
                  ;;(site-lisp (string-append output "/share/emacs/site-lisp"))
                  ;; programs
                  (cc (string-append (assoc-ref %build-inputs "gcc") "/bin/gcc"))
                  (g++ (string-append (assoc-ref %build-inputs "gcc") "/bin/g++"))
                  (includes (string-append
                             (assoc-ref %build-inputs
                                        "linux-headers")
                             "/include"
                             ":" (assoc-ref %build-inputs
                                            "tree-sitter")
                             "/include"
                             ":" (assoc-ref %build-inputs
                                            "emacs")
                             "/include")))

             (display "set-env") (newline)
             (setenv "PATH" (string-append binutils "/bin"))
             (setenv "C_INCLUDE_PATH" includes)
             (setenv "CPLUS_INCLUDE_PATH" includes)
             (setenv "LIBRARY_PATH"
                     (string-append
                      (assoc-ref %build-inputs "glibc") "/lib"))
             (display "copy-file") (newline)
             (copy-recursively source "source" #:keep-mtime? #t)
             (chdir "source/src")
             (copy-file (string-append module "/tree-sitter-lang.in")
                        "tree-sitter-lang.c")

             ;; (substitute* "tree-sitter-lang.c"
             ;;   ;; (("\"emacs-module\\.h\"")
             ;;   ;;  "<emacs-module.h>")
             ;;   (("LANG_C") lang-c)
             ;;   (("LANG") lang))

             (display "dump" ) (newline)
             (call-with-output-file "grammar.js.dump"
               (lambda (dump)
                 (let ((s (call-with-input-file grammar-file
                            get-string-all)))
                   (apply format dump "~@{0x~x, ~}0x0a"
                          (map char->integer (string->list s))))))

             (display "build c") (newline)
             (apply invoke cc "-fPIC" "-c"
                    (find-files "." (make-regexp "\\.c$")))

             (unless (null? (find-files "." (make-regexp "\\.cc$")))
               (display "build cc") (newline)
               (apply invoke g++ "-c" "-fPIC"
                      (find-files "." (make-regexp "\\.cc$"))))

             (display "link") (newline)
             (apply invoke g++ "-shared" "-fPIC"
                    `(,@(find-files "." (make-regexp "\\.o$"))
                      "-o" ,soname))

             (display "install") (newline)
             (mkdir-p lib)
             (install-file soname lib)))))
      (home-page "")
      (synopsis "")
      (description
       "")
      (license license:expat))))

(define-public emacs-tree-sitter-html-module
  (make-emacs-tree-sitter-module
   "html"
   "0.19.0"
   "1hg7vbcy7bir6b8x11v0a4x0glvqnsqc3i2ixiarbxmycbgl3axy"))
