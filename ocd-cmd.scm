(require-library ocd)
(import ocd)

(use ports)

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "usage:" (program-name) " <directory>"))))

(ocd-load-ocdrc)
(ocd-start!)
