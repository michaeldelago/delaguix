(define-module (statusbar)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix licenses)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz))

(define-public sbcl-statusbar
  (package 
    (name "statusbar")
    (synopsis "Statusbar for sway and i3")
    (description "Statusbar for sway and i3")
    (license expat)
    (home-page "github.com/michaeldelago/statusbar")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                (url "https://github.com/michaeldelago/statusbar/")
                (commit "guix")))
              (sha256
                (base32
                  "0x9pxndfaibfsfdhx2108c2dvscpvix1jyzdc480adjbhj5qmbcc"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs (list 
      sbcl 
      cl-alexandria 
      cl-ppcre 
      cl-local-time 
      cl-split-sequence
      cl-diskspace))
    (outputs '("out" "lib"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append (assoc-ref outputs "out") "/bin/statusbar")
               outputs
               #:entry-program '((statusbar:print-status) 0)))))))))
sbcl-statusbar