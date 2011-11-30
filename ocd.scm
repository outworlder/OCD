;;; OCD - Egg for running tests repeatedly.

;;; Get the file listing

(use files)                             ; Files and pathname operations
(use posix)

(define ocd-root-directory (make-parameter (current-directory)))

(define (compile-files-list path)
  ;(print "Entering directory" path)
  (if (directory? path)  ;; Stop condition 
      (let ([listing (directory path)])
        (map (lambda (d)
               (handle-exceptions exn (begin
                                        (print exn)
                                        '()) 
                                  (let ([absolute-path (normalize-pathname (make-absolute-pathname path d))])
                                    (if (directory? absolute-path)
                                        (compile-files-list absolute-path)
                                        (list absolute-path (file-modification-time absolute-path)))))) listing))
      path))

;;; Walk the trees and return files that have been modified.
(define (get-modified before after)
  #f)

(compile-files-list (current-directory))
