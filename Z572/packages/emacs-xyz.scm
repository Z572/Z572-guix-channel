(define-module (Z572 packages emacs-xyz)
  #:use-module (gnu packages code)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (guix packages)
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

(define-public emacs-telega-server-latest
  (package
    (name "emacs-telega-server")
    (version "0.7.030")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zevlg/telega.el")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1g29v5fgkqx43wsvh1npx0g3hj00n37lxgvxjvy85fs4h9226gl9"))
       (file-name (git-file-name "emacs-telega" version))
       (patches
        (search-patches "emacs-telega-path-placeholder.patch"
                        "emacs-telega-test-env.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "INSTALL_PREFIX="
                            (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-subdirectory
           (lambda _ (chdir "server") #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "run_tests.py"
                 (("^(TELEGA_SERVER = ).*$" _all prefix)
                  (string-append prefix
                                 "\"" out "/bin/telega-server\"\n"))))))
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check))
         (add-before 'install-license-files 'leave-subdirectory
           (lambda _ (chdir "..") #t)))
       #:test-target "test"))
    (inputs
     `(("tdlib" ,tdlib)
       ("libappindicator" ,libappindicator)))
    (native-inputs
     `(("python" ,python)
       ("pkg-config" ,pkg-config)))
    (home-page "https://zevlg.github.io/telega.el/")
    (synopsis "Server process of Telega")
    (description "Telega-server is helper program to interact with Telegram
service, and connect it with Emacs via inter-process communication.")
    (license license:gpl3+)))

(define-public emacs-telega-latest
  (package
    (inherit emacs-telega-server-latest)
    (name "emacs-telega")
    (build-system emacs-build-system)
    (arguments
     `(#:emacs
       ,(if (target-64bit?)
            emacs-minimal
            ;; Require wide-int support for 32-bit platform.
            emacs-wide-int)
       #:include (cons "^etc\\/" %default-include)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hard-code paths to `ffplay` and `ffmpeg`.
             (let* ((ffmpeg (assoc-ref inputs "ffmpeg"))
                    (ffmpeg-bin (string-append ffmpeg "/bin/ffmpeg"))
                    (ffplay-bin (string-append ffmpeg "/bin/ffplay")))
               (substitute* '("telega-ffplay.el" "telega-vvnote.el")
                 (("(shell-command-to-string\|concat) \"(ffmpeg\|ffprobe)"
                   all func cmd)
                  (string-append func " \"" (assoc-ref inputs "ffmpeg")
                                 "/bin/" cmd))
                 (("\\(executable-find \"ffplay\"\\)")
                  (string-append "(and (file-executable-p \"" ffplay-bin "\")"
                                 "\"" ffplay-bin "\")"))
                 (("\\(executable-find \"ffmpeg\"\\)")
                  (string-append "(and (file-executable-p \"" ffmpeg-bin "\")"
                                 "\"" ffmpeg-bin "\")"))))))
         (add-after 'unpack 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "telega-server.el"
               (("@TELEGA_SERVER_BIN@")
                (string-append (assoc-ref inputs "emacs-telega-server")
                               "/bin/telega-server")))
             (substitute* "telega-util.el"
               (("@TELEGA_SHARE@")
                (string-append (elpa-directory (assoc-ref outputs "out"))
                               "/etc"))))))))
    (inputs
     `(("emacs-telega-server" ,emacs-telega-server-latest)
       ("ffmpeg" ,ffmpeg)))
    (native-inputs '())
    (propagated-inputs
     `(("emacs-visual-fill-column" ,emacs-visual-fill-column)
       ("emacs-company" ,emacs-company)
       ("emacs-rainbow-identifiers" ,emacs-rainbow-identifiers)))
    (synopsis "GNU Emacs client for the Telegram messenger")
    (description "Telega is a full-featured, unofficial GNU Emacs-based client
for the Telegram messaging platform.")))

(define-public emacs-telega-contrib-latest
  (package
    (inherit emacs-telega-latest)
    (name "emacs-telega-contrib")
    (arguments
     `(#:exclude
       '("telega-live-location.el")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _ (chdir "contrib") #t))
         (add-before 'install-license-files 'leave-subdirectory
           (lambda _ (chdir "..") #t)))))
    (inputs '())
    (native-inputs '())
    (propagated-inputs
     `(("emacs-alert" ,emacs-alert)
       ("emacs-all-the-icons" ,emacs-all-the-icons)
       ("emacs-dashboard" ,emacs-dashboard)
       ("emacs-telega" ,emacs-telega-latest)
       ("emacs-transient" ,emacs-transient)))
    (synopsis "Contributed packages to Telega")
    (description "Telega-contrib is a collection of third-party
contributed packages to Telega.")))

(define-public emacs-citre
  (let ((commit "b9bca2d86f58d59e7e170f7224c78753c18e58a8")
        (revision "1"))
    (package
      (name "emacs-citre")
      (version (git-version "0.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/universal-ctags/citre")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1nfr07j32c4pq6pvd432brq8w8zqrnmal6095871hg6a505i44kj"))))
      (build-system emacs-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-ctag-path
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((u-ctags (assoc-ref inputs "universal-ctags"))
                      (readtags (string-append u-ctags "/bin/readtags"))
                      (ctags (string-append u-ctags "/bin/ctags")))

                 (make-file-writable "citre-core.el")
                 (make-file-writable "citre-ctags.el")
                 (emacs-substitute-variables "citre-core.el"
                   ("citre-readtags-program" readtags))
                 (emacs-substitute-variables "citre-ctags.el"
                   ("citre-ctags-program" ctags))))))))
      (inputs `(("universal-ctags" ,universal-ctags)))
      (home-page "https://github.com/universal-ctags/citre")
      (synopsis "Ctags IDE on the True Editor")
      (description
       "Citre is an advanced Ctags (or actually, readtags) frontend for Emacs.")
      (license license:gpl3+))))

(define-public emacs-bing-dict
  (let ((commit "1d581aaa9622b34f8fb83af5579fa252aa24cfef")
        (revision "0"))
    (package
      (name "emacs-bing-dict")
      (version (git-version "0.2.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cute-jumper/bing-dict.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1v0ncgnj9vv2r0qbwiipg63rlhnfjz8q23jx3la2l22l0i2lyivb"))))
      (build-system emacs-build-system)
      (home-page
       "https://github.com/cute-jumper/bing-dict.el")
      (synopsis
       "Minimalists' English-Chinese Bing dictionary")
      (description
       "A **minimalists'** Emacs extension to search http://www.bing.com/dict.
Support English to Chinese and Chinese to English.")
      ;; from bing-dict.el head
      (license license:gpl3+))))

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
  (let ((commit "a6449f22cb97160ee1c90121968de89e193268df")
        (revision "0"))
    (package
      (name "emacs-easy-escape")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cpitclaudel/easy-escape")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1spbavcs4a3vn1ggdcgwgb2wvq4lbk74xyfagr4y5b5w2azlkh51"))))
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

(define-public emacs-puni
  (let ((commit "3a3272c881945f0dfb4467f7f053d0853f612186")
        (revision "0"))
    (package
      (name "emacs-puni")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/AmaiKinono/puni")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1m2z4sif8558qyjzp33kfbjr1laz3nh79qbpzbnykk0j73q5zb9z"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/AmaiKinono/puni")
      (synopsis "Parentheses Universalistic")
      (description "Customizable soft deletion commands")
      (license license:gpl3+))))

(define-public emacs-meow
  (let ((commit "3e58697695327d1ecf2a210af645e8f2db845c32")
        (revision "1"))
    (package
      (name "emacs-meow")
      (version (git-version "1.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DogLooksGood/meow")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fl9fc7sibivna92ddnh6vv271qykkn9bw97nak1cnn9isi5hvn6"))))
      (build-system emacs-build-system)
      (propagated-inputs
       `(("emacs-dash" ,emacs-dash)
         ("emacs-s" ,emacs-s)))
      (home-page
       "https://github.com/DogLooksGood/meow")
      (synopsis "Modal Editing On Wheel")
      (description
       "Enable `meow-global-mode' to activate modal editing.")
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
  (let ((commit "fa6343271567d010e72c5aeafbb23f665e89b8c4")
        (revision "2"))
    (package
      (name "emacs-xeft")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/casouri/xeft")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jl5bmdn8q6gl632q2zl0ijngjphmqiahx86m72hvcx97sdv1raf"))
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
      (license #f))))
