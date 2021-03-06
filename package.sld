
;; Common function
; package-install
; package-installed?
; package-uninstall
; package-update

(define-library (package)
  (export install installed?
          uninstall update
          https-proto git-proto

          module-default-proto
          module-default-proto-set!

          module-path)

  (import (gambit)
          (prefix (git-scheme) git-))

  (include "package.scm"))


