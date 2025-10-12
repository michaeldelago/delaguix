;;; docker.scm --- Guix build system for Docker/OCI containers

(define-module (delaguix build-system docker)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-utils)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix hash)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:export (docker-build-system
            docker-build))

(define (docker-build-system)
  "A build system for creating Docker/OCI containers."
  (build-system
   (name 'docker)
   (build-inputs
    (list (package-inputs (docker-build-system))))
   (arguments
    (list
     #:name "docker-container"
     #:phases
     (list
      (assoc-ref %standard-phases 'configure)
      (assoc-ref %standard-phases 'build)
      (assoc-ref %standard-phases 'install)
      (phase
       (name "create-container")
       (description "Create the container image")
       (build
        (lambda* (#:key inputs outputs store (system %current-system) #:allow-other-keys)
          (let* ((output (assoc-ref outputs "out"))
                 (image-name (or (getenv "IMAGE_NAME") "my-container"))
                 (image-tag (or (getenv "IMAGE_TAG") "latest"))
                 (base-image (or (getenv "BASE_IMAGE") "alpine:latest"))
                 (dockerfile-path (getenv "DOCKERFILE_PATH"))
                 (container-dir (string-append output "/container"))
                 (dockerfile (string-append container-dir "/Dockerfile"))
                 (native-inputs (getenv "NATIVE_INPUTS")))
            ;; Create container directory structure
            (mkdir-p container-dir)
            
            ;; Use provided Dockerfile or create a default one
            (if dockerfile-path
                ;; Copy the provided Dockerfile
                (copy-file dockerfile-path dockerfile)
                ;; Create a default Dockerfile
                (call-with-output-file dockerfile
                  (lambda (port)
                    (format port "FROM ~a~%" base-image)
                    (format port "LABEL maintainer=\"Guix Docker Builder\"~%")
                    (format port "WORKDIR /app~%")
                    (format port "COPY . /app~%")
                    (format port "CMD [\"/bin/sh\"]~%"))))
            
            ;; Handle native inputs using guix pack
            (when native-inputs
              (let ((pack-command (string-append "guix pack -n " native-inputs " -f docker " container-dir)))
                (system* "sh" "-c" pack-command)))
            
            ;; Build the container image
            (let ((build-command (string-append "docker build -t " image-name ":" image-tag " " container-dir)))
              (system* "sh" "-c" build-command))
            
            ;; Export the container
            (let ((export-command (string-append "docker save " image-name ":" image-tag " > " output "/container.tar")))
              (system* "sh" "-c" export-command))
            
            ;; Return the output path
            output)))))
     #:outputs '("out")
     #:implicit-inputs? #f
     #:modules '((guix build utils))
     #:imported-modules '((guix build utils))
     #:properties
     (list
      (property 'container-name "docker-container")
      (property 'container-type 'oci)))))

(define (docker-build name
                      #:base-image base-image
                      #:image-name image-name
                      #:image-tag image-tag
                      #:dockerfile dockerfile
                      #:inputs inputs
                      #:phases phases
                      #:outputs outputs
                      #:modules modules
                      #:imported-modules imported-modules
                      #:properties properties)
  "Build a Docker/OCI container with the given NAME.
BASE-IMAGE is the base image to use (default: \"alpine:latest\").
IMAGE-NAME is the name of the resulting image (default: \"my-container\").
IMAGE-TAG is the tag for the resulting image (default: \"latest\").
DOCKERFILE is the path to a custom Dockerfile to use (optional).
INPUTS are the build inputs.
PHASES are the build phases.
OUTPUTS are the build outputs.
MODULES are the modules to use.
IMPORTED-MODULES are the imported modules.
PROPERTIES are the build properties."
  (let ((build-system (docker-build-system)))
    (build-system-build build-system
                        #:name name
                        #:base-image base-image
                        #:image-name image-name
                        #:image-tag image-tag
                        #:dockerfile dockerfile
                        #:inputs inputs
                        #:phases phases
                        #:outputs outputs
                        #:modules modules
                        #:imported-modules imported-modules
                        #:properties properties)))

;;; docker.scm ends here
