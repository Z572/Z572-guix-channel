(define-module (Z572 packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages noweb)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (alist-delete))
  #:use-module (srfi srfi-26))

(define-public guile-libarchive
  (let ((commit "7ed41f0ecdbd8c6bd9ad3feff257022e7d80ba48")
        (revision "0"))
    (package
      (name "guile-libarchive")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/RhodiumToad/guile-libarchive")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "147kfvc6rchs4ani4r8m6cdhn6gxjrkvn2q9ay83nx9qbz0r1fjv"))))
      (build-system guile-build-system)
      (arguments
       (list #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'patch
                            (lambda* (#:key inputs #:allow-other-keys)
                              (substitute* "libarchive/base.scm"
                                (("dynamic-link \"libarchive\"")
                                 (string-append "dynamic-link \""
                                                (search-input-file inputs
                                                 "/lib/libarchive.so") "\""))))))))
      (inputs (list libarchive))
      (native-inputs (list guile-3.0))
      (home-page "https://github.com/RhodiumToad/guile-libarchive")
      (synopsis "Guile 3 libarchive modules, using FFI (no C)")
      (description
       "This is libarchive module for guile 3, using FFI (no C).
NOTE: This isn't complete, in particular there is no support yet for disk
archives or ACLs, but it seems to mostly work...")
      (license license:expat))))

(define-public guile3.0-g-wrap
  (package (inherit g-wrap)
    (name "guile3.0-g-wrap")
    (arguments
     (substitute-keyword-arguments (package-arguments g-wrap)
       ;; ((#:imported-modules modules '())
       ;;  `((ice-9 match)
       ;;    (ice-9 popen)
       ;;    (ice-9 rdelim)
       ;;    ,@modules
       ;;    ,@%gnu-build-system-modules))
       ;; ((#:modules modules '())
       ;;  `((ice-9 popen)
       ;;    (ice-9 rdelim)
       ;;    (ice-9 match)
       ;;    ,@modules
       ;;    ,@%gnu-build-system-modules))
       ((#:make-flags flags ''())
        `(cons "GUILE_AUTO_COMPILE=0" ,flags))
       ((#:phases phase ''())
        `(modify-phases ,phase
           (add-before 'build 'fix-build
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "guile/g-wrap/guile-runtime.c"
                 (("scm_class_([a-z]+)" all first)
                  (string-append "scm_c_public_ref(\"oop goops\",\"<" first ">\")")))))
           (add-after 'unpack 'set-indent-program-path
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "g-wrap/util.scm"
                 (("indent \\~S")
                  (string-append (search-input-file inputs "/bin/indent") " ~S")))))
           (add-before 'pre-configure 'allow-guile3
             (lambda _
               (substitute* "configure"
                 (("2\\.2 2\\.0")
                  "3.0 2.2 2.0"))))
           ;; (add-after 'install 'compile-go-file
           ;;   (lambda* (#:key inputs outputs #:allow-other-keys)
           ;;     (let* ((out (pk 'out (assoc-ref outputs "out")))
           ;;            (guile (search-input-file inputs "/bin/guile"))
           ;;            (effective (pk 'effective(read-line (open-pipe* OPEN_READ
           ;;                                                            guile
           ;;                                                            "-c" "(display (effective-version))"))))
           ;;            (dir (pk 'dir (string-append out "/share/guile/site/" effective)))
           ;;            (files (cons "./g-wrap.scm" (with-directory-excursion dir
           ;;                                          (find-files "./g-wrap"
           ;;                                                      "\\.scm$"))))
           ;;            (guild (search-input-file inputs "/bin/guild"))
           ;;            (go-dir (string-append out "/lib/guile/" effective "/site-ccache/")))
           ;;       ;; (setenv "GUILE_LOAD_PATH"
           ;;       ;;         (string-append dir
           ;;       ;;                        (match (getenv "GUILE_LOAD_PATH")
           ;;       ;;                          (#f "")
           ;;       ;;                          (path (string-append ":" path)))))
           ;;       (setenv "GUILE_LOAD_COMPILED_PATH"
           ;;               (string-append go-dir
           ;;                              (match (getenv "GUILE_LOAD_COMPILED_PATH")
           ;;                                (#f "")
           ;;                                (path (string-append ":" path)))))
           ;;       (mkdir-p go-dir)
           ;;       (for-each
           ;;        (lambda (file)
           ;;          (invoke
           ;;           guild "compile"
           ;;           "-L" dir
           ;;           "-o" (string-append go-dir (basename file ".scm") ".go")
           ;;           (string-append dir "/" file)))
           ;;        files))))
           ))))
    (inputs
     (modify-inputs
      (package-inputs g-wrap)
      (append indent)))
    (propagated-inputs
     (modify-inputs
      (package-propagated-inputs g-wrap)
      (replace "guile" guile-3.0-latest)
      ;; for pc file
      (append libffi)))))

(define-public guile3.0-gnome
  (package (inherit guile-gnome)
    (name "guile3.0-gnome")
    (arguments
     (substitute-keyword-arguments (package-arguments guile-gnome)
       ((#:tests? _)
        #t)
       ((#:make-flags flags ''())
        `(cons "GUILE_AUTO_COMPILE=0" ,flags))
       ;; ((#:configure-flags flags ''())
       ;;  `(cons* "--disable-deprecated"
       ;;          ,flags))
       ((#:phases phase ''())

        `(modify-phases ,phase
           ;; (replace 'bootstrap
           ;;   (lambda _
           ;;     (delete-file-recursively "build-aux")
           ;;     (invoke "autoreconf" "-vif")))
           (add-before 'check 'pre-check
             (lambda _
               ;; Tests require a running X server.
               (system "Xvfb :1&")
               (setenv "DISPLAY" ":1")))
           (add-before 'configure 'pre-configure
             (lambda _
               (substitute* "configure"
                 (("2\\.2 2\\.0")
                  "3.0 2.2 2.0")
                 ;; (("gtk\\+-2\\.0 >= 2.10\\.0")
                 ;;  "gtk+-3.0 >= 3.0.0")
                 )
               ;; (substitute* "common.mk"
               ;;                            (("GUILE_FLAGS =.*" all)
               ;;                             (string-append all "
               ;; godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
               ;; GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)
               ;; nobase_go_DATA = $(GOBJECTS)
               ;; guile_install_go_files = install-nobase_goDATA
               ;; $(guile_install_go_files): install-nobase_modDATA
               ;; CLEANFILES = $(GOBJECTS)
               ;; "))
               ;;                            (("SUFFIXES =(.*)" all)
               ;;                             (string-append all " .scm .go
               ;; .scm.go:
               ;; \t$(AM_V_GEN)$(GUILE_TOOLS) compile $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o \"$@\" \"$<\"")))
               (substitute* (list "glib/gnome/gobject/gtype.c"
                                  "gconf/gnome/gw/gconf-support.c"
                                  "gtk/gnome/gw/gtk-support.c"
                                  "glib/gnome/gobject/gsignal.c"
                                  "glib/gnome/gobject/gparameter.c"
                                  "glib/gnome/gobject/gvalue.c"
                                  "glib/gnome/gobject/gobject.c")
                 (("SCM_UNBOUND")
                  "SCM_UNSPECIFIED")
                 (("SCM_LIST([1-9])" all first )
                  (string-append "scm_list_" first)))
               (substitute* "gtk/gnome/gtk/graphical-repl.scm"
                 (("define-module.*$" all)
                  (string-append all "
#:use-module (ice-9 top-repl)
#:use-module (ice-9 scm-style-repl)
")))))
           (add-after 'install 'fix-extesion
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (so-dir (string-append out "/lib/guile-gnome-2")))
                 (let-syntax ((substitute-so
                               (syntax-rules ()
                                 ((_ (file name) ...)
                                  (begin (substitute* file
                                           ((name)
                                            (string-append so-dir "/" name ".so")))
                                         ...)))))
                   (with-directory-excursion (string-append out "/share/guile-gnome-2/gnome/gw")
                     (substitute-so
                      ("cairo.scm" "libgw-guile-gnome-cairo")
                      ("canvas.scm" "libgw-guile-gnome-canvas")
                      ("gconf.scm" "libgw-guile-gnome-gconf")
                      ("gobject.scm" "libgw-guile-gnome-gobject")
                      ("libglade.scm" "libgw-guile-gnome-libglade")
                      ("libgnomeui.scm" "libgw-guile-gnome-libguileui")
                      ("libgnome.scm" "libgw-guile-gnome-libgnome")
                      ("pangocairo.scm" "libgw-guile-gnome-pangocairo")
                      ("atk.scm" "libgw-guile-gnome-atk")
                      ("gtk.scm" "libgw-guile-gnome-gtk")
                      ("gdk.scm" "libgw-guile-gnome-gdk")
                      ("pango.scm" "libgw-guile-gnome-pango")
                      ("glib.scm" "libgw-guile-gnome-glib")))))))))))
    (native-inputs
     (modify-inputs
      (package-native-inputs guile-gnome)
      (append xorg-server-for-tests indent ;; automake autoconf libtool
              )
      ;; (replace "gtk+" gtk+)
      ;; (delete libglade)
      ))
    (propagated-inputs
     (modify-inputs
      (package-propagated-inputs guile-gnome)
      (replace "g-wrap" guile3.0-g-wrap)
      (replace "guile-lib" guile-lib)
      (replace "guile-cairo" guile-cairo)))
    (inputs
     (modify-inputs
      (package-inputs guile-gnome)
      (replace "guile" guile-3.0-latest)))))
