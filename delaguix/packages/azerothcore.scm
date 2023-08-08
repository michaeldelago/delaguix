(define-module (delaguix packages azerothcore)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define (normalize-module-sql path)
  (let* ((database-names (list "auth" "world" "characters"))
         (sources (map (lambda (x)
                         (string-append path "/sql/" x) database-names)))
         (destinations (map (lambda (x)
                              (string-append path "/data/sql/db-" x)
                              database-names))))
    (map (lambda (src dest)
           (when (file-exists? src)
             (rename-file src dest))) sources destinations)))

(define-public azerothcore-wotlk
  (package
    (name "azerothcore-wotlk")
    (version "9.230808")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     ;; AzerothCore doesn't tag things very often. 
                     ;; I have this fork on a timer to sync the repo and 
                     ;; tag with the database version and date
                    (url "https://github.com/michaeldelago/azerothcore-wotlk")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1j09hsyw9grzn50rj0i0zam0dql3mkxvfj9n7lhw5mn1wmnf4kgy"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (outputs '("out"))
    (propagated-inputs (list mysql wowgaming-client-data))
    (native-inputs `(("git" ,git)
                     ("lbzip2" ,lbzip2)
                     ("boost" ,boost)
                     ("readline" ,readline)
                     ("openssl-3.0" ,openssl-3.0)
                     ("ncurses" ,ncurses)
                     ("clang" ,clang)
                     ("libtool" ,libtool)
                     ("zlib" ,zlib)
                     ("gcc-toolchain" ,gcc-toolchain)
                     ("glibc-locales" ,glibc-locales)
                     ("python-wrapper" ,python-wrapper)))
    (arguments
     `(#:tests? #f
       #:configure-flags (list "-DWITH_WARNINGS=1"
                               ;; Tools don't need to be built since
                               "-DTOOLS_BUILD=none"
                               "-DSCRIPTS=static"
                               "-DMODULES=dynamic"
                               "-DCMAKE_C_COMPILER=clang"
                               "-DCMAKE_CXX_COMPILER=clang++"
                               (string-append "-DMYSQL_CONFIG="
                                              (assoc-ref %build-inputs "mysql")
                                              "/bin/mysql_config"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-sql
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (source-sql-dir (string-append (assoc-ref
                                                             %build-inputs
                                                             "source") "/data"))
                             (data-sql-dir (string-append out "/data")))
                        ;; we don't really care about the SQL archives
                        (delete-file-recursively (string-append source-sql-dir
                                                  "/sql/archive"))
                        (copy-recursively source-sql-dir data-sql-dir) #t)))
                  (add-after 'install 'init-config
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (auth-conf (string-append out
                                         "/etc/authserver.conf.dist"))
                             (world-conf (string-append out
                                          "/etc/worldserver.conf.dist"))
                             (client-data-path (assoc-ref inputs
                                                "wowgaming-client-data"))
                             (mysql-exe (string-append (assoc-ref
                                                        %build-inputs "mysql")
                                                       "/bin/mysql")))
                        (substitute* world-conf
                          (("^DataDir.*")
                           (string-append "DataDir = \"" client-data-path "\"")))
                        (substitute* (list auth-conf world-conf)
                          (("^SourceDirectory.*")
                           (string-append "SourceDirectory = \"" out "\""))
                          (("^MySQLExecutable.*")
                           (string-append "MySQLExecutable = \"" mysql-exe
                                          "\""))) #t))))))
    (home-page "https://www.azerothcore.org/")
    (synopsis "Modular World of Warcraft: Wrath of the Lich King Emulator")
    (description
     "AzerothCore is an open-source game-server application for World of Warcraft, 
supporting the 3.3.5a game version.")
    (license license:gpl2)))

(define-public wowgaming-client-data
  (package
    (name "wowgaming-client-data")
    (version "v16")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/wowgaming/client-data/releases/download/"
                    version "/data.zip"))
              (sha256
               (base32
                "1sbx7hyv3vs05yqgnjnj6i9hzgbib0b737zy573qz42yl9v6rknc"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)
                         (ice-9 format)))))
    (build-system copy-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'create-version-file
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (file (string-append out "/data-version"))
                             (mode 420))
                        (call-with-output-file file
                          (lambda (port)
                            (format port "INSTALLED_VERSION=~a"
                                    '("v16"))))
                        (chmod file mode) #t))))))
    (home-page "https://github.com/wowgaming/client-data")
    (synopsis "World of Warcraft Client data")
    (description
     "World of Warcraft client data used in operation of the game server. Contains DBC Files, MPQ files, and Map files.")
    (license license:cc-by-sa3.0)))

(define-public mod-quest-count-level
  (let ((commit "dce8d35688783128cb5f0c5f7916f1ddb91965c9")
        (revision "1"))
    (package
      (inherit azerothcore-wotlk)
      (name "mod-quest-count-level")
      (version (git-version "master" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://github.com/michaeldelago/mod-quest-count-level")
                      (commit commit)))
                (sha256
                 (base32
                  "1mvfqp8vwla04z9928z1dxh1b2nsqn7rmvp1p2vb6zc3w7mlx58n"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet #~(begin
                             ;; There was a change with how database files are referenced by
                             ;; the migration tooling, though many modules have not been
                             ;; properly updated. This should make it so that it's normalized
                             ;; and all modules use the proper pathing.
                             (let* ((database-names (list "auth" "world"
                                                          "characters"))
                                    (sources (map (lambda (x)
                                                    (string-append "sql/" x))
                                                  database-names))
                                    (destinations (map (lambda (x)
                                                         (string-append
                                                          "data/sql/db-" x))
                                                       database-names)))
                               (map (lambda (src dest)
                                      (when (file-exists? src)
                                        (mkdir-p dest)
                                        (copy-recursively src dest)
                                        (delete-file-recursively src)))
                                    sources destinations))))))
      (native-inputs `(("azerothcore-wotlk" ,(package-source azerothcore-wotlk))
                       ,@(package-native-inputs azerothcore-wotlk)))
      (build-system cmake-build-system)
      (arguments
       `(#:make-flags (let ((module-name (string-append "mod_"
                                                        ,name))
                            (make-jobs (number->string (max 2
                                                            (parallel-job-count)))))
                        (list module-name "-j" make-jobs))
         #:tests? #f
         #:validate-runpath? #f
         #:configure-flags (list "-DWITH_WARNINGS=1"
                                 "-DTOOLS_BUILD=none"
                                 "-DSCRIPTS=static"
                                 "-DMODULES=dynamic"
                                 "-DCMAKE_C_COMPILER=clang"
                                 "-DCMAKE_CXX_COMPILER=clang++"
                                 (string-append "-DMYSQL_CONFIG="
                                                (assoc-ref %build-inputs
                                                           "mysql")
                                                "/bin/mysql_config"))
         #:phases (modify-phases %standard-phases
                    (add-before 'unpack 'grab-acore-source
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((source-acore (assoc-ref %build-inputs
                                                       "azerothcore-wotlk")))
                          (copy-recursively source-acore "source")
                          (chdir "source/modules")) #t))
                    (add-after 'unpack 'rename-module
                      (lambda* _
                        (chdir "..")
                        (copy-recursively "source"
                                          ,name)
                        (delete-file-recursively "source")
                        (chdir "..") #t))
                    (replace 'install
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin"))
                               (module-name (string-append "libmod_"
                                                           ,name ".so"))
                               (data-sql-dir (string-append out "/data"))
                               (module-source-dir (string-append
                                                   "../source/modules/"
                                                   ,name)))
                          (with-directory-excursion "modules"
                            (install-file module-name bin))
                          (with-directory-excursion module-source-dir
                            (display (invoke "ls" "-lah"))
                            (copy-recursively "data" data-sql-dir)) #t)))))))))
