;;; OCD - Egg for running tests repeatedly.

(module ocd
  (ocd-filename-filter
   ocd-root-directory
   ocd-delay
   ocd-run-command
   ocd-start!
   ocd-load-ocdrc
   ) 

  (import scheme chicken)
  (use files posix srfi-69 srfi-1 srfi-13 srfi-18)

  (define ocd-root-directory (make-parameter (current-directory)))
  (define ocd-delay (make-parameter 2))
  (define ocd-run-command (make-parameter "make"))
  (define ocd-filename-filter (make-parameter '("*.scm")))
  (define last-control/c 0)
  (define force-run? #f)

  (define (print-exception exn)
    (print "Exception:"
           ((condition-property-accessor 'exn 'message) exn)
           ((condition-property-accessor 'exn 'arguments) exn)))

  (define (ocd-load-ocdrc)
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
    (if (directory? path)               ;; Stop condition
        (with-directory path
                        (let ([listing (apply glob (ocd-filename-filter))])
                          (for-each
                           (lambda (d)
                             (handle-exceptions exn (print-exception exn) ; Print the exception, ignore the file and continue.
                                                (let ([absolute-path (normalize-pathname (make-absolute-pathname path d))])
                                                  (if (directory? absolute-path)
                                                      (walk-directories! ht absolute-path)
                                                      (hash-table-set! ht absolute-path (file-modification-time absolute-path)))))) listing)))
        path))

;;; Walk the hash and return files that have been modified.
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
        (if (or force-run? (not (null? modified)))
          (begin
            (print "Files changed: " modified)
            (set! force-run? #f)
            (files-changed modified))))
      (main-loop after)))

  (define (files-changed files)
    (print "Running: " (ocd-run-command))
    (print "Status:" (system (ocd-run-command))))

  (define (set-control/c-handler!)
    (let self ()
      (set-signal-handler! signal/int
                           (lambda (sig)
                             (set-signal-handler! signal/int
                                                  (lambda (sig)
                                                    (print "Terminating due to user request.")
                                                    (exit 0)))
                             (print "Control+C pressed. Press it again within two seconds to interrupt")
                             (sleep 2)
                             (print "Running the build again.")
                             (set! force-run? #t)
                             (self)))))
  
  (define (ocd-start!)
    (set-control/c-handler!)
    (main-loop (compile-files-list (ocd-root-directory))))
)
