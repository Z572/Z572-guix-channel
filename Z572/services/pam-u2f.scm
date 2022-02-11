(define-module (Z572 services pam-u2f)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages security-token)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (pam-u2f-service-type))
(define (pam-u2f-pam-service config)
  (define optional-pam-u2f
    (pam-entry
     (control "optional")
     (module #~(string-append #$pam-u2f "/lib/security/pam_u2f.so"))))
  (list (lambda (pam)
          (if (member (pam-service-name pam)
                      '("login" "su" "slim" "gdm-password" "sddm"))
              (pam-service
               (inherit pam)
               (auth (append (pam-service-auth pam)
                             (list optional-pam-u2f)))
               (session (append (pam-service-session pam)
                                (list optional-pam-u2f))))
              pam))))

(define pam-u2f-service-type
  (service-type
   (name 'pam-u2f)
   (extensions (list (service-extension pam-root-service-type
                                        pam-u2f-pam-service)))
   (default-value #f)
   (description "Activate PAM-U2F support.  It allows")))
