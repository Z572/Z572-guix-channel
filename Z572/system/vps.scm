(define-module (Z572 system vps)
  #:use-module (gnu)
  #:use-module (Z572 utils)
  #:use-module (Z572 keys)
  #:export (vps-os))

(use-service-modules
 base networking ssh
 admin networking ssh
 mcron sysctl cuirass linux certbot mail web desktop)
(use-package-modules bootloaders)
(define (symbols->packages l) (map symbol->package l))
(define vps-os
  (operating-system
    (host-name "vps")
    (timezone "Etc/UTC")
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/vda"))
                 (terminal-outputs '(console))))
    (file-systems (cons (file-system
                          (mount-point "/")
                          (device "/dev/vda1")
                          (type "ext4"))
                        %base-file-systems))
    (packages
     (append
      (symbols->packages
       '(tmux git rsync mosh htop neofetch file nss-certs emacs-minimal))
      %base-packages))
    (services
     (append (list
              (simple-service
               'asus-guix-key guix-service-type
               (guix-extension (authorized-keys (list keyfile/guix/asus))))
              (service dhcp-client-service-type)
              (service openssh-service-type
                       (openssh-configuration
                        (port-number 2222)
                        (permit-root-login 'prohibit-password)
                        (allow-empty-passwords? #f)
                        (authorized-keys `(("root" ,keyfile/ssh/asus))))))
             %base-services))))
