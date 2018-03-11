
(define-library (package)
  (export install uninstall build ls installed? build? check?)

  (import (gambit)
          (prefix (git-scheme) git-)
          (rename (prefix (version) version-)
                  (version-version>? version>?))
          #;(rename (prefix (module) module-)
                  (module-url-parts->module-type url-parts->module-type)))

  (begin

    (define-type module-type
      constructor: make
      (proto module-proto)
      (web-src module-web-src)
      (name module-name)
      (version module-version)
      (submodule module-submodule))

    (define (url-parts->module-type proto url-part delim)
      (define (parse-module-submodule url path version)
        (let loop ((rest url) (result ""))
          (if (pair? rest)
            (loop (cdr rest) (##path-expand (car rest) result))
            (make proto delim path version result))))

      (define (parse-module-version url path)
        (if (pair? url)
          (let ((delim-val (car url))
                (rest (cdr url)))
            (cond
              ((string=? delim-val delim)
               (if (pair? rest)
                 (parse-module-submodule (cdr rest) path (car rest))
                 (error "Missing version")))
              (else
                (parse-module-submodule url path "master"))))
          (make proto delim path "master" "")))

      (define (parse-module-url url)
        (if (>= (length url) 3) ; 3 is minimum size
          (let ((hostname (list-ref url 0))
                (user (list-ref url 1))
                (repo-name (list-ref url 2))
                (rest (list-tail url 3)))
            (parse-module-version rest
                                  (##path-expand repo-name (##path-expand user hostname))))
          (error "Not a valid repo name")))

      (parse-module-url url-part))

    (define (err)
      (error "not implemented"))

    (define location (getenv "R7RS_LIBRARY_LOCATION"))

    (define (has-prefix? str prefix)
      (let ((len-str (string-length str))
            (len-prefix (string-length prefix)))
        (and (>= len-str len-prefix)
             (string=? (substring str 0 len-prefix) prefix)
             (substring str len-prefix len-str))))

    (define (strip-protocol package-name)
      (let ((rest (or (has-prefix? package-name "http://")
                      (has-prefix? package-name "https://"))))
        rest))

    (define (install package-url-parts)
      (let* ((module (url-parts->module-type
                 (car package-url-parts)
                 (cdr package-url-parts) "tree"))
             (fs-path (##path-expand
                       (module-version module)
                       (module-name module)))
             (url (string-append (module-proto module) "//" (module-name module))))
        (let ((p (open-process (list path: "git"
                                     arguments: (list "clone" url fs-path)
                                     directory: location))))
          (= (process-status p) 0)))
      #;(let ((name-version (path-strip-directory package-name)))
        (let ((p (open-process (list path: "git"
                                     arguments: (list "clone" package-name (strip-protocol package-name))
                                     directory: (directory)
                                     stdout-redirection: #t
                                     stderr-redirection: #f))))
          (read-all p)
          (not (= (process-status p) 0)))))
 
    (define (build package-name)
      (err)
      #;(let* ((name-version (path-strip-directory package-name))
             (dir (string-append directory name-version))
             (build-file (string-append dir "/compile.sh")))
        (process-status
          (open-process
            (list path: build-file
                  arguments: (list "dyn")
                  directory: dir)))))

    (define (check? package-url-parts)
      ;; Encode tree in config file.
      (let* ((module (url-parts->module-type (car package-url-parts) (cdr package-url-parts) "tree")))
        (println module)
        (let ((version-str (module-version module)))
          (if (string=? version-str "master")
            ;; For latest version release
            (let* ((relative-path
                    (##path-expand version-str (module-name module)))
                   (absolute-path (##path-expand relative-path location)))
              (println relative-path)
              (println absolute-path)
              ;(error "TODO: implement it version/master")
              (file-exists? absolute-path))

            (let* ((base-version (version-parse version-str))
                   (relative-path (##path-expand (##number->string (version-major base-version)) (module-name module)))
                   (absolute-path (##path-expand relative-path location)))
              (println base-version)
              (println relative-path)
              (println absolute-path)
              (error "TODO: implement it arbitrary version"))))))

    (define (uninstall package-name)
      (let* ((name-version (path-strip-directory package-name))
             (package-dir (string-append location name-version)))
        (let ((p (open-process (list
                                 path: "rm"
                                 arguments: (list "-rf" package-dir)))))
          (println "Uninstall package-dir")
          (process-status p))))

    (define (installed? package-name)
      (let* ((name-version (path-strip-directory package-name))
             (package (string-append location name-version)))
        (file-exists? package)))

    (define (ls)
      (directory-files location))

    (define (search pattern)
      (err))))

#;(define-type package
  id: ea010c3d-b074-4892-9830-17e05ea218fc
  name
  version
  repo)

#;(define-macro (err)
  `(error "not implemented"))


;; Common function
; package-install
; package-list
; package-uninstall
; package-search
; package-directory
; package-directory-set!
; package-resolve


