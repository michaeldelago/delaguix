(define-module (delaguix packages snackbar)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module (guix licenses)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp-xyz))

(define-public sbcl-snackbar
  (package 
    (name "snackbar")
    (synopsis "Statusbar for sway and i3")
    (description "Statusbar for sway and i3")
    (license expat)
    (home-page "github.com/michaeldelago/snackbar")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                (url "https://github.com/michaeldelago/snackbar/")
                (commit version)))
              (sha256
                (base32
                  "1y82mm1imb3p6rzlanxfgynkpnc0l13qwvw2snnr74ri4q3lq7p4"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs (list 
      sbcl 
      cl-alexandria 
      cl-ppcre 
      cl-local-time 
      cl-split-sequence
      cl-diskspace
      gcc))
    (inputs (list
      alsa-utils))
    (outputs '("out" "lib"))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append (assoc-ref outputs "out") "/bin/snackbar")
               outputs
               #:entry-program '((snackbar:print-status) 0)))))))))
