(define-module (Z572 packages web)
  #:use-module (Z572 packages)
  #:use-module (gnu packages web)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public nginx-with-realip
  (modifyed-package (o nginx)
    (name (string-append name "-with-realip"))
    (arguments
     (substitute-keyword-arguments arguments
       ((#:configure-flags flags #~''())
        #~(cons "--with-http_realip_module" #$flags))))))

(define-public nginx-dav-ext-module
  (package
    (inherit nginx)
    (name "nginx-dav-ext-module")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arut/nginx-dav-ext-module")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "000dm5zk0m1hm1iq60aff5r6y8xmqd7djrwhgnz9ig01xyhnjv9w"))))
    (build-system gnu-build-system)
    (inputs
     `(("nginx-sources" ,(package-source nginx))
       ,@(package-inputs nginx)))
    (arguments
     (substitute-keyword-arguments
         `(#:make-flags '("modules")
                        #:modules ((guix build utils)
                                   (guix build gnu-build-system))
                        ,@(package-arguments nginx)
                        #:configure-flags '("--with-http_dav_module" "--add-dynamic-module=."))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'unpack-nginx-sources
              (lambda* (#:key inputs native-inputs #:allow-other-keys)
                (begin
                  ;; The nginx source code is part of the module’s source.
                  (format #t "decompressing nginx source code~%")
                  (invoke "tar" "xvf" (assoc-ref inputs "nginx-sources")
                          ;; This package's LICENSE file would be
                          ;; overwritten with the one from nginx when
                          ;; unpacking the nginx source, so rename the nginx
                          ;; one when unpacking.
                          "--transform=s,/LICENSE$,/LICENSE.nginx,"
                          "--strip-components=1")
                  #t)))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((modules-dir (string-append (assoc-ref outputs "out")
                                                  "/etc/nginx/modules")))
                  (install-file "objs/ngx_http_dav_ext_module.so" modules-dir))))
            (delete 'fix-root-dirs)
            (delete 'install-man-page)))))
    (home-page "https://github.com/arut/nginx-rtmp-module")
    (synopsis "nginx WebDAV PROPFIND,OPTIONS,LOCK,UNLOCK support ")
    (description "The standard ngx_http_dav_module provides partial WebDAV
implementation and only supports GET,HEAD,PUT,DELETE,MKCOL,COPY,MOVE methods.
For full WebDAV support in nginx you need to enable the standard
ngx_http_dav_module as well as this module for the missing methods.")
    (license license:bsd-2)))
