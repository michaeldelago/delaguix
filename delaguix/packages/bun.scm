(define-module (delaguix packages bun)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages development-tools)
  #:use-module (gnu packages programming-language)
  #:use-module (gnu packages python)
  #:use-module (gnu packages nodejs)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages go)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xorg))

(define-public bun
  (package
    (name "bun")
    (version "1.3.1") ; Replace with the appropriate version
    ;; git url https://github.com/oven-sh/bun
    ;; tag is "bun-v1.3.1"
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oven-sh/bun")
                    (commit (string-append "bun-v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32 "1234567890123456789012345678901234567890123456789012"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-shared" "--prefix=" (assoc-ref %outputs "out"))))
    (native-inputs
     `(("llvm-19" ,llvm-19)
       ("clang-19" ,clang-19)
       ("lld-19" ,lld-19)
       ("nodejs-24" ,nodejs-24)
       ("rustc" ,rustc)
       ("cargo" ,cargo)
       ("go" ,go)
       ("gcc" ,gcc)
       ("ninja" ,ninja)
       ("cmake" ,cmake)
       ("pkg-config" ,pkg-config)
       ("ccache" ,ccache)
       ("python-3" ,python-3)
       ("openssl" ,openssl)
       ("zlib" ,zlib)
       ("libxml2" ,libxml2)
       ("libiconv" ,libiconv)
       ("liberation-ttf" ,liberation-ttf)
       ("atk" ,atk)
       ("libdrm" ,libdrm)
       ("xorg.libxshmfence" ,xorg.libXshmfence)
       ("gdk-pixbuf" ,gdk-pixbuf)))
    (inputs `())
    (synopsis "Bun is a fast all-in-one JavaScript runtime.")
    (description "Bun is designed to be a high-performance alternative to Node.js, offering a more efficient and streamlined experience for JavaScript applications. This package includes the necessary build tools and dependencies as specified in the Nix flake configuration.")
    (home-page "https://bun.sh") ; Replace with actual home page
    (license license:expat))) ; Replace with appropriate license
