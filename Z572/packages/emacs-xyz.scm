(define-module (Z572 packages emacs-xyz)
  #:use-module (gnu packages code)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages search)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix cvs-download)
  #:use-module (guix download)
  #:use-module (guix bzr-download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages video))

(define-public emacs-leaf-keywords
  ;;latest git tag is on May 29, 2019, 1.1.0, but leaf-keywords.el version is
  ;;2.0.5
  (let ((commit "849b579f87c263e2f1d7fb7eda93b6ce441f217e")
        (revision "1"))
    (package
      (name "emacs-leaf-keywords")
      (version (git-version "2.0.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/conao3/leaf-keywords.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00fnkk6hl9l64dgmkhsqibhna7gdpazs4j28f7833n1dmg626ki6"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-leaf" ,emacs-leaf)))
      (home-page
       "https://github.com/conao3/leaf-keywords.el")
      (synopsis
       "Additional leaf.el keywords for external packages")
      (description
       "This package provides additional keywords for leaf.el defines
keywords that are dependent on an external package.")
      (license license:gpl3+))))

(define-public emacs-easy-escape
  (let ((commit "938497a21e65ba6b3ff8ec90e93a6d0ab18dc9b4")
        (revision "0"))
    (package
      (name "emacs-easy-escape")
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cpitclaudel/easy-escape")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bqwn6cd7lrk7f8vgcvclryvlpxvl2bndsmwmbn0zxmvqkdba7l1"))))
      (build-system emacs-build-system)
      (home-page
       "https://github.com/cpitclaudel/easy-escape")
      (synopsis
       "Improve readability of escape characters in regular expressions")
      (description
       "This package provides a uses syntax highlighting and composition to make
ELisp regular expressions more readable.")
      (license license:gpl3+))))

(define-public emacs-winum
  (let ((commit "c5455e866e8a5f7eab6a7263e2057aff5f1118b9")
        (revision "0"))
    (package
      (name "emacs-winum")
      (version (git-version "2.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/deb0ch/winum.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0lj4cp7ml7cjhkd66f6ivcl6sbfs2my8ajjlynzl3pm5qansfw5i"))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-dash" ,emacs-dash)))
      (home-page "https://github.com/deb0ch/winum.el")
      (synopsis
       "Navigate windows and frames using numbers.")
      (description
       "Window numbers for Emacs: Navigate your windows and frames using
numbers.")
      (license license:gpl3+))))

(define-public emacs-highlight-quoted
  (let ((commit "24103478158cd19fbcfb4339a3f1fa1f054f1469")
        (revision "0"))
    (package
      (name "emacs-highlight-quoted")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Fanael/highlight-quoted")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gq8inxfni9zgz2brqm4nlswgr8b0spq15wr532xfrgr456g10ks"))))
      (build-system emacs-build-system)
      (home-page
       "https://github.com/Fanael/highlight-quoted")
      (synopsis
       "Highlight Lisp quotes and quoted symbols")
      (description
       "Minor mode proving highlight of Lisp quotes and quoted symbols.")
      (license license:bsd-2))))

(define-public emacs-xeft
  (package
    (name "emacs-xeft")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/casouri/xeft")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b43v0lcgad9005bldsk27nkn0mzhh678na9c0dnlvzhnx254h8g"))
              (modules '((guix build utils)))
              (snippet
               '(delete-file "emacs-module.h"))))
    (build-system emacs-build-system)
    (arguments
     `(#:include
       (cons "\\.so$" %default-include)
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'build-emacs-module
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make"
                     (string-append "CXX=" ,(cxx-for-target))
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
    (inputs
     `(("xapian" ,xapian)))
    (home-page "https://github.com/casouri/xeft")
    (synopsis "note searching")
    (description "Xeft provides a dynamic module that exposes a very basic
indexing feature to Emacs Lisp, that lets you index and search a text files.")
    ;; not know license
    (license #f)))
