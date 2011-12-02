;;; OCD - Egg for running tests repeatedly.

;;; Get the file listing

(use files)                             ; Files and pathname operations
(use posix)
(use srfi-69)                           ; uHash tables
(use srfi-1)


(define ocd-root-directory (make-parameter (current-directory)))
(define ocd-delay (make-parameter 2))
(define ocd-run-command (make-parameter "make"))
(define ocd-filename-filter (make-parameter '("*.scm")))

(define (print-exception exn)
  (print "Exception:"
         ((condition-property-accessor 'exn 'message) exn)
         ((condition-property-accessor 'exn 'arguments) exn)))

(define (load-ocdrc)
  (let ([ocd-file ".ocdrc"])
    (if (file-exists? ocd-file)
        (load ocd-file))))

(define-syntax with-directory
  (syntax-rules ()
    ([_ directory forms...] (let ([cd (current-directory)])
                                  (change-directory directory)
                                  forms...
                                  (change-directory cd)))))

(define (compile-files-list path)
  (let ([ht (make-hash-table string= string-hash 100)])
    (walk-directories! ht path)
    ht))

(define (walk-directories! ht path)
  ;(print "Entering directory" path)
  (if (directory? path)  ;; Stop condition
      (with-directory path
                      (let ([listing (apply glob (ocd-filename-filter))])
                        (for-each (lambda (d)
                                    (handle-exceptions exn (print-exception exn) ; Print the exception, ignore the file and continue.
                                                       (let ([absolute-path (normalize-pathname (make-absolute-pathname path d))])
                                                         (if (directory? absolute-path)
                                                             (walk-directories! ht absolute-path)
                                                             (hash-table-set! ht absolute-path (file-modification-time absolute-path)))))) listing)))
      path))

;;; Walk the trees and return files that have been modified.
(define (get-modified before after)
  (filter (lambda (x) (not (eqv? #f x))) ; Returns everything that is not #f. eqv compact in Ruby.
          (hash-table-map before
                          (lambda (file before-date)
                            (let ([after-date (hash-table-ref after file)])
                              (if (not (eqv? before-date after-date))
                                  file
                                  #f))))))

(define (main-loop before)
  (sleep (ocd-delay))
  (let ([after (compile-files-list (ocd-root-directory))])
    (let ([modified (get-modified before after)])
      (unless (null? modified)
        (begin
          (print "Files changed: " modified)
          (files-changed modified))))
    (main-loop after)))

(define (files-changed files)
  (print "Running: " (ocd-run-command))
  (print "Status:" (system (ocd-run-command))))
                                        ; (print (hash-table->alist (compile-files-list (current-directory))))
(load-ocdrc)
(main-loop (compile-files-list (ocd-root-directory)))
