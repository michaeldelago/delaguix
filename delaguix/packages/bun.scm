(define-module (bun)
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
    (version "1.0") ; Replace with the appropriate version
    (source #f) ; Placeholder for source, replace with actual source URI
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-shared" "--prefix=" (assoc-ref %outputs "out"))
       #:make-flags
       (list "VERBOSE=1")
       #:test-flags
       (list "MAKEFLAGS=-j2")))
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
    (inputs
     `(
       ; macOS-specific dependencies if applicable
       )))
    (synopsis "Bun is a fast all-in-one JavaScript runtime.")
    (description "Bun is designed to be a high-performance alternative to Node.js, offering a more efficient and streamlined experience for JavaScript applications. This package includes the necessary build tools and dependencies as specified in the Nix flake configuration.")
    (home-page "https://bun.sh") ; Replace with actual home page
    (license license:expat))) ; Replace with appropriate license
