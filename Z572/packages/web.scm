(define-module (Z572 packages web)
  #:use-module (gnu packages web)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public nginx-with-realip
  (package
    (inherit nginx)
    (name "nginx-with-realip")
    (arguments
     (substitute-keyword-arguments
         (package-arguments nginx)
       ((#:configure-flags flags ''())
        `(list "--with-http_realip_module"))))))
