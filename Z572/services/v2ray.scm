(define-module (Z572 services v2ray)
  #:use-module (gnu services)
  #:use-module (Z572 packages v2ray)
  #:use-module (gnu build linux-container)
  #:use-module (gnu services shepherd)
  #:use-module (guix modules)
  #:use-module (ice-9 match)
  #:use-module (gnu services admin)
  #:use-module (json)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services shepherd)
  #:export (v2ray-configuration
            v2ray-configuration?
            v2ray-configuration-package
            v2ray-configuration-json
            v2ray-configuration-mappings
            v2ray-service-type))

(define-record-type* <v2ray-configuration>
  v2ray-configuration
  make-v2ray-configuration
  v2ray-configuration?
  (package v2ray-configuration-package
           (default v2ray))
  (json     v2ray-configuration-json
            (default "{}"))
  (mappings v2ray-configuration-mappings
            (default '()))
  (loglevel v2ray-configuration-loglevel
            (default "warning")))

(define %v2ray-accounts
  (list (user-account
         (name "v2ray")
         (group "v2ray")
         (system? #t)
         (comment "V2RAY daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group (name "v2ray") (system? #t))))

(define (v2ray-binary config)
  (file-append (v2ray-configuration-package config) "/bin/v2ray"))

(define (v2ray-json config)
  (match (v2ray-configuration-json config)
    ((? string? a)
     (plain-file "v2ray-config" a))
    ((? list? a)
     (plain-file "v2ray-config" (scm->json-string a)))
    (a a)))

(define (v2ray-shepherd-service config)
  "Return a <shepherd-service> for v2ray with CONFIG."
  (define log-config
    (plain-file "v2ray-log-config"
                (scm->json-string
                 `((log . ((loglevel . ,(v2ray-configuration-loglevel config))
                           (access . "/var/log/v2ray/access.log")
                           ("error" . "/var/log/v2ray/error.log")))))))
  (define v2ray-command
    #~(list #$(v2ray-binary config)
            "--config" #$(v2ray-json config)
            "--config" #$log-config))
  (list
   (with-imported-modules (source-module-closure
                           '((gnu build shepherd)
                             (gnu system file-systems)))
     (shepherd-service
      (provision '(v2ray))
      (requirement '(networking))
      (documentation "")
      (modules '((gnu build shepherd)
                 (gnu system file-systems)))
      (start #~(make-forkexec-constructor/container
                #$v2ray-command
                ;; #:namespaces '#$(delq 'net %namespaces)
                #:log-file "/var/log/v2ray.log"
                #:user "v2ray"
                #:mappings (cons* (file-system-mapping
                                   (source "/var/log/v2ray")
                                   (target source)
                                   (writable? #t))
                                  (file-system-mapping
                                   (source "/etc/ssl/certs")
                                   (target source))
                                  (list #$@(v2ray-configuration-mappings config)))
                #:group "v2ray"))
      (stop #~(make-kill-destructor))))))

(define (%v2ray-activation config)
  "Return an activation gexp for V2RAY with CONFIG"

  #~(begin
      (mkdir-p "/var/log/v2ray")
      (chown "/var/log/v2ray"
             (passwd:uid (getpw "v2ray"))
             (group:gid (getgr "v2ray")))))

(define %v2ray-log-rotations
  (list (log-rotation
         (files (list "/var/log/v2ray/error.log"
                      "/var/log/v2ray/access.log"))
         (options '(;; Run post-rotate once per rotation
                    "sharedscripts"
                    ;; Append .gz to rotated files
                    "storefile @FILENAME.@COMP_EXT")))))

(define v2ray-service-type
  (service-type
   (name 'v2ray)
   (extensions
    (list (service-extension account-service-type
                             (const %v2ray-accounts))
          (service-extension activation-service-type
                             %v2ray-activation)
          (service-extension rottlog-service-type
                             (const %v2ray-log-rotations))
          (service-extension shepherd-root-service-type
                             v2ray-shepherd-service)))
   (default-value (v2ray-configuration))
   (description
    "")))
