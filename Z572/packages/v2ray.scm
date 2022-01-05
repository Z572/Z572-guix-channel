(define-module (Z572 packages v2ray)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define-public v2ray
  (package
    (name "v2ray")
    (version "4.44.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/v2fly/v2ray-core/releases/download/v"
                    version "/v2ray-linux-64.zip"))

              (sha256
               (base32
                "0s4fa88pl5x2bcyqcqhk4map4vfsmplhpnwdl74j6pgw0zi1psy6"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("v2ray" "bin/")
         ("v2ctl" "bin/")
         ("systemd" "lib/")
         ("." "etc/v2ray"
          #:include-regexp ("\\.json$" "\\.dat$")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (v2ray (string-append out "/bin/v2ray"))
                    (etc (string-append out "/etc/v2ray")))
               (wrap-program v2ray
                 `("V2RAY_LOCATION_ASSET" prefix (,etc)))))))))
    (search-paths
     (list (search-path-specification
            (variable "V2RAY_LOCATION_ASSET")
            (files '("etc/v2ray")))))
    (home-page "https://v2fly.org")
    (synopsis "A platform for building proxies to bypass network restrictions")
    (description "Project V is a set of network tools that helps you to build
your own computer network. It secures your network connections and thus protects
your privacy.")
    (license expat)))
