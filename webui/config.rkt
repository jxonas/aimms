#lang racket

(provide hostname port-no)

(define hostname (make-parameter "localhost"))
(define port-no (make-parameter 12001))

(define config-file 
  (build-path (find-system-path 'home-dir) "widget.conf"))

(when (file-exists? config-file)
  (for ([x (in-list (file->lines config-file))])
    (match x
      [(pregexp "^hostname\\s*=\\s*(\\S+)\\s*$" (list _ s)) (hostname s)]
      [(pregexp "^port\\s*=\\s*(\\S+)\\s*$" (list _ s)) (port-no (string->number s))]
      [else (void)])))


