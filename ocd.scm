;;; OCD - Egg for running tests repeatedly.

;;; Get the file listing

(use files)                             ; Files and pathname operations
(use posix)
(use srfi-69)                           ;Hash tables

(define ocd-root-directory (make-parameter (current-directory)))

(define (compile-files-list path)
  (let ([ht (make-hash-table string= string-hash 100)])
    (walk-directories! ht path)
    ht))

(define (walk-directories! ht path)
  ;(print "Entering directory" path)
  (if (directory? path)  ;; Stop condition 
      (let ([listing (directory path)])
        (for-each (lambda (d)
                    (handle-exceptions exn (begin
                                             (print exn)
                                             '()) 
                                       (let ([absolute-path (normalize-pathname (make-absolute-pathname path d))])
                                         (if (directory? absolute-path)
                                             (walk-directories ht absolute-path)
                                             (hash-table-set! ht absolute-path (file-modification-time absolute-path)))))) listing))
      path))

;;; Walk the trees and return files that have been modified.
(define (get-modified before after)
  #f)

(print (hash-table->alist (compile-files-list (current-directory))))
