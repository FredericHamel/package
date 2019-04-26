
(include "~~lib/_module#.scm")

(define (string->modref url)
  (##string->modref url))

(define (module-path user?)
  (path-expand
    (if user? "~~userlib" "~~lib")))

(define (https-proto mod)
  (string-append "https://" mod))

(define (ssh-proto mod)
  (string-append "ssh://git@" mod))

(define module-default-proto https-proto)

(define (module-default-proto-set! proto)
  (set! module-default-proto proto))

(define tree-master "tree/master")
(define (tree-master-set! x)
  (set! tree-master x))

(define debug-mode? #f)
(define (debug-mode?-set! x)
  (set! debug-mode? x))

(define (install mod user? #!optional (prompt? #f) (proto #f))
  (define (join-rev path lst)
    (if (pair? lst)
      (join-rev
        (path-expand path (car lst))
        (cdr lst))
      path))

  (let ((modref (string->modref mod)))
    (and modref
         (pair? (macro-modref-host modref))
         (let* ((repo-path (module-path user?))

                (module-name
                  (let loop ((path (macro-modref-path modref)))
                    (if (pair? (cdr path))
                      (loop (cdr path))
                      (car path))))

                ;; null? if no tag specify
                (tag (macro-modref-tag modref))

                ;; url without the protocol
                (base-url (join-rev module-name
                                    (macro-modref-host modref)))

                ;; url used to clone the repo.
                (url ((or proto module-default-proto) base-url))

                (archive-path
                  (path-expand base-url repo-path))

                (supplied-version? (pair? tag))

                ;; Path on the file system where to clone
                (clone-path (path-expand
                              tree-master
                              archive-path))

                (install-path (and supplied-version?
                                   (let ((version (join-rev (car tag) (cdr tag))))
                                     (and
                                       (not (string=? tree-master version))
                                       (path-expand version archive-path))))))

           (if install-path
             (and (not (file-exists? install-path))
                  (or (file-exists? clone-path)
                      (git-clone url clone-path prompt?: prompt?))
                  (let ((archive-name
                          (git-archive (car tag) install-path clone-path)))
                    (and archive-name
                         (git-extract-archive
                           archive-name
                           (path-directory install-path)))))

             (and (not (file-exists? clone-path))
                  (git-clone url clone-path prompt?: prompt?)))))))


;; Return #f if module is not hosted
(define (uninstall module user?)

  ;; return the prefix if prefix/folder exists else #f.
  (define (start-width? folder prefix)
    (and (file-exists? (path-expand folder prefix)) prefix))

  (define (delete-single-folder folder)
    (if debug-mode?
      (println "rmdir " folder))
    (delete-directory folder))

  (define (delete-single-file file)
    (if debug-mode?
      (println "rm " file))
    (delete-file file))

  (define (delete-folder-tree folder)
    (let loop ((port (open-directory (list path: folder ignore-hidden: 'dot-and-dot-dot)))
               (folder-name folder)
               (folder-stack '())
               (post-folder-stack '()))
      (let ((file (read port)))
        (if (eof-object? file)
          (let ()
            (close-input-port port)
            (if (pair? folder-stack)
              (loop (open-directory
                      (list path: (car folder-stack) ignore-hidden: 'dot-and-dot-dot))
                    (car folder-stack)
                    (cdr folder-stack)
                    (cons folder-name post-folder-stack))

              (let ()
                (delete-single-folder folder-name)
                (for-each
                  (lambda (post-folder)
                    (delete-single-folder post-folder))
                  post-folder-stack)
                #t)))

          (let* ((filepath (path-expand file folder-name))
                 (type (file-type filepath)))
            (cond
              ((eq? type 'directory)
               (loop port folder-name (cons filepath folder-stack) post-folder-stack))
              (else
                (delete-single-file filepath)
                (loop port folder-name folder-stack post-folder-stack))))))))


  (let ((modref (##string->modref module)))
    (and modref
         (pair? (macro-modref-host modref))
         (let ((prefix (start-width? module (module-path user?))))
           (and prefix
                (let loop ((modref-path (macro-modref-path modref)))
                  (if (pair? (cdr modref-path))
                    (loop (cdr modref-path))
                    (macro-modref-path-set! modref modref-path)))

                (delete-folder-tree (path-expand (##modref->string modref) prefix)))))))

(define (installed? module)
  (let ((modref (##string->modref module)))
    (and (pair? (macro-modref-host modref))
         (null? (macro-modref-tag modref))
         (macro-modref-tag-set! modref (list tree-master)))

    (let ((result (##search-module modref)))
      (and (vector? result)
           (let ((port (vector-ref result 4)))
             (close-input-port port) #t)))))

(define (update mod)
  (define (start-width? folder prefix)
    (and (file-exists? (path-expand folder prefix)) prefix))

  (let ((modref (##string->modref mod)))
    (and (pair? (macro-modref-host modref))
         (null? (macro-modref-tag modref))
         (let ()
           (macro-modref-tag-set! modref (list tree-master))
           (let ((module-master-path (##modref->string modref)))
             (let ((prefix (or (start-width? module-master-path (module-path #f))
                               (start-width? module-master-path (module-path #t)))))
               (and prefix
                    (git-pull (path-expand module-master-path prefix)))))))))

(define (install-hook modref)
  (let ((mod-name (##modref->string modref)))
    (let ((result (install mod-name #t #t)))
      (if debug-mode?
        (println "install-hook==>" result))
      (and result (##search-module modref)))))

(##install-module-set! install-hook)
