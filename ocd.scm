;;; OCD - Egg for running tests repeatedly.

;;; Get the file listing

(use files)                             ; Files and pathname operations
(use posix)
(use srfi-69)                           ; uHash tables

(define ocd-root-directory (make-parameter (current-directory)))
(define ocd-delay (make-parameter 1000))

(define (print-exception exn)
  (print "Exception:"
         ((condition-property-accessor 'exn 'message) exn)
         ((condition-property-accessor 'exn 'arguments) exn)))

(define (compile-files-list path)
  (let ([ht (make-hash-table string= string-hash 100)])
    (walk-directories! ht path)
    ht))

(define (walk-directories! ht path)
  ;(print "Entering directory" path)
  (if (directory? path)  ;; Stop condition 
      (let ([listing (directory path)])
        (for-each (lambda (d)
                    (handle-exceptions exn (print-exception exn)  ; Print the exception, ignore the file and continue.
                                       (let ([absolute-path (normalize-pathname (make-absolute-pathname path d))])
                                         (if (directory? absolute-path)
                                             (walk-directories! ht absolute-path)
                                             (hash-table-set! ht absolute-path (file-modification-time absolute-path)))))) listing))
      path))

;;; Walk the trees and return files that have been modified.
(define (get-modified before after)
  (hash-table-map before
                  (lambda (file before-date)
                    (let ([after-date (hash-table-ref after key)])
                      (if (not (eq? before-date after-date))
                          file)))))

(define (main-loop)
  (let ([before (compile-files-list (ocd-root-directory))])
    (sleep 1000)
    (let ([after (compile-files-list (ocd-root-directory))])
      (print (get-modified before after)))))

                                        ; (print (hash-table->alist (compile-files-list (current-directory))))
(main-loop)
