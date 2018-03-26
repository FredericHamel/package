
(define-library (package)
  (export install uninstall build ls
          installed? build? check?
          update parse-module-name

          module-proto
          module-web-src
          module-path
          module-version
          module-submodule)

  (import (gambit)
          (prefix (https://github.com/FredericHamel/git-scheme/tree/1.0.0) git-)
          (rename (prefix (https://github.com/FredericHamel/version/tree/1.0.0) version-)
                  (version-version->string version->string)
                  (version-version>? version>?))
          #;(rename (prefix (module) module-)
                  (module-url-parts->module-type url-parts->module-type)))

  (begin

    (define-type module-type
      constructor: make
      (proto module-proto)
      (web-src module-web-src)
      (path module-path)
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

    (define (parse-module-name name-parts)
      (url-parts->module-type (car name-parts)
                              (cdr name-parts)
                              "tree"))

    (define (install module #!key (exact-version? #t))
      (let* ((version-str (module-version module))
             (base-path (##path-expand (module-path module) location))
             (url (string-append (module-proto module) "//" (module-path module)))
             (clone-path (##path-expand ".clone" base-path)))
        ;; Download repo from github if not already installed.
        (or (file-exists? clone-path)
            (git-clone url clone-path))
        (if (string=? version-str "master")
          ;; Install master version
          (let ((archive (git-archive clone-path "master")))
            (git-extract-archive (##path-expand archive clone-path) base-path))

          ;; Install max version in same major version.
          (let ((base-version (version-parse version-str)))
            (if (file-exists? clone-path)
              (let ((max-tag (if exact-version? base-version (git-max-tag clone-path base-version))))
                (begin
                  ;; base-version
                  (let* ((archive (git-archive clone-path (version->string max-tag))))
                    (if archive
                      (let ((archive-path (##path-expand archive clone-path)))
                        (git-extract-archive archive-path base-path)
                        (delete-file archive-path))
                      (error "[package]: Unexpected error")))))
              (if exact-version?
                (let ((archive (git-archive clone-path (version->string max-version))))
                  (if archive
                    (let ((archive-path (##path-expand archive clone-path)))
                      (git-extract-archive archive-path base-version)
                      (delete-file archive-path))
                    (error "[package]: Unexpected error")))
                (let loop ((max-version base-version)
                           (tags (git-ls-remote-tag url)))
                  (if (pair? tags)
                    (let ((version (version-parse (car tags))))
                      (if (version>? version base-version)
                        (loop version (cdr tags))
                        (loop base-version (cdr tags))))
                    (begin
                      (let ((archive (git-archive clone-path (version->string max-version))))
                        (if archive
                          (let ((archive-path (##path-expand archive clone-path)))
                            (git-extract-archive archive-path base-version)
                            (delete-file archive-path))
                          (error "[package]: Unexpected error"))))))))))))

    (define (build module)

      (err)
      #;(let* ((name-version (path-strip-directory package-name))
             (dir (string-append directory name-version))
             (build-file (string-append dir "/compile.sh")))
        (process-status
          (open-process
            (list path: build-file
                  arguments: (list "dyn")
                  directory: dir)))))

    ;; Check if repo exists
    ;; Return (vector
    (define (check? module)
      ;; Encode tree in config file.
        (println module)
        (let ((version-str (module-version module)))
          (if (string=? version-str "master")
            ;; For latest version release
            (let* ((relative-path
                    (##path-expand version-str (module-path module)))
                   (absolute-path (##path-expand relative-path location)))
              (println relative-path)
              (println absolute-path)
              ;(error "TODO: implement it version/master")
              (file-exists? absolute-path))

            (let* ((base-version (version-parse version-str))
                   (relative-path (##path-expand (##number->string (version-major base-version)) (module-path module)))
                   (absolute-path (##path-expand relative-path location)))
              (println base-version)
              (println relative-path)
              (println absolute-path)
              (error "TODO: implement it arbitrary version")))))

    (define (uninstall module)
      (let ((base-directory (##path-expand (module-path module) location)))
        (let ((module-directory (##path-expand (module-version module) base-directory)))
          (if (file-exists? module-directory)
            (let loop ((cur-dir module-directory)
                       (cur-dir-port (open-directory (list path: module-directory ignore-hidden: 'dot-and-dot-dot)))
                       (dir-stack '())
                       (dir-stack-to-delete '()))
              (let ((file (read cur-dir-port)))
                (if (string? file)
                  (let* ((filepath (##path-expand file cur-dir))
                         (type (file-type filepath)))
                    (cond
                      ((eq? type 'directory)
                       (loop cur-dir cur-dir-port (cons filepath dir-stack) dir-stack-to-delete))
                      (else
                        (delete-file filepath)
                        (loop cur-dir cur-dir-port dir-stack dir-stack-to-delete))))
                  (begin
                    (close-port cur-dir-port)
                    (if (pair? dir-stack)
                      (let ((new-dir (car dir-stack))
                            (rest-dir (cdr dir-stack)))
                        (loop new-dir (open-directory (list path: new-dir ignore-hidden: 'dot-and-dot-dot))
                              rest-dir (cons cur-dir dir-stack-to-delete)))
                      ;; Exit for the test
                      (begin
                        (for-each (lambda (dir)
                                    (delete-directory dir))
                                  dir-stack-to-delete)
                        (delete-directory cur-dir)))))))))))

    (define (installed? module)
      (let* ((base-directory (##path-expand (module-path module) location))
             (module-directory (##path-expand (module-version module) base-directory)))
        (and (file-exists? module-directory)
             module-directory)))

    (define (update module)
      (let* ((base-directory (##path-expand (module-path module) location))
             (clone-directory (##path-expand ".clone" base-directory)))
        (if (file-exists? clone-directory)
          (git-pull clone-directory)
          (error "package not installed"))))

    (define (search pattern)
      (err))))

;; Common function
; package-install
; package-list
; package-uninstall
; package-search
; package-directory
; package-directory-set!
; package-resolve


