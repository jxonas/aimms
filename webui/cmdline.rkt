#lang racket/base

(require "widget.rkt"
         racket/match
         racket/cmdline)

(provide (all-defined-out))

;; Parameters 

; One of: RAW, INFO, DELETE, CREATE, DUPLICATE, CLEANUP
(define action (make-parameter #f))

; Used in conjunction with actions CREATE and DUPLICATE
(define id (make-parameter #f))

; Used in conjunction with action DUPLICATE
(define name (make-parameter #f))

(define type (make-parameter #f))
(define index (make-parameter #f))

; Listof String
; Each string should 'key=value' or 'key="value"'
(define changes (make-parameter '()))


;; Command line

(define widgets
  (command-line
   #:program "WebUI Widget Inspector"
   
   #:once-each
   [("--host")  host "Widget server hostname"     (hostname host)]
   [("--port")  port "Widget server port number"  (port-no (string->number port))]
   [("--id")   value "Necessary with --create"    (id value)]
   [("--name") value "Necessary with --duplicate" (name value)]
   
   #:once-any
   [("--raw")       "Display raw widget"         (action 'RAW)]
   [("--info")      "Display widget information" (action 'INFO)]
   [("--cleanup")   "Cleanup widget on server"   (action 'CLEANUP)]
   [("--delete")    "Delete widget from server"  (action 'DELETE)]
   [("--create")    "Creates new widget"         (action 'CREATE)]
   [("--duplicate") "Duplicates widget"          (action 'DUPLICATE)]
   [("--set-type") Value "Set widget type"       (action 'SET-TYPE) (type Value)]
   [("--to-legend") Index "Transform widget in legend" (action 'TO-LEGEND) (index Index)]
   
   #:multi
   [("--set") key=value "Sets key to value" 
              (changes (cons (list 'set key=value) (changes)))]
   [("--drop") key "Removes property from widget"
               (changes (cons (list 'drop key) (changes)))]
   
   #:ps 
   ""
   "This is still in beta, so use it at your own risk."
   "Issues? Send an email to jonas.rodrigues@unisoma.com."
   
   #:args Widgets Widgets))


; CREATE action
(define (CREATE widgets)
  (cond 
    [(> (length widgets) 1)
     (displayln "You are able to create just on widget at a time.")]
    [(not (id))
     (displayln (format "You must inform the new widget --id"))]
    [else
     (create (car widgets) (id) (parse-changes))]))

; DUPLICATE action
(define (DUPLICATE widgets)
  (cond 
    [(> (length widgets) 1)
     (displayln "You are able to duplicate just on widget at a time.")]
    [(not (name))
     (displayln (format "You must inform the --name for the new widget."))]
    [else
     (duplicate (car widgets) (name) (parse-changes))]))


; UPDATE widget on server based on (changes)
(define (UPDATE widgets)
  (define changes (parse-changes))
  (for ([wname (in-list widgets)])
    (update wname changes)))

; Set widgets type to (type)
(define (SET-TYPE widgets)
  (define typestr (format "literal:\"~a\"" (type)))
  (for-each
   (lambda (widget)
     (update widget `((set data-aimms.widget.type ,typestr))))
   widgets))

(define (CLEANUP widgets)
  (for ([widget (in-list widgets)])
    (define w (get-widget w))
    (when w
      (cleanup w)
      (put-widget w))))

(define (TO-LEGEND widgets)
  (for-each
   (lambda (widget)
     (update widget 
             `((set data-aimms.widget.type "literal:\"legend\"")
               (set data-contents ,(format "aimms:{\"contents\":[\"~a\"]}" (index)))
               (set data-contents.partition ,(format "literal:{\"list\":[\"~a\"]}" (index))))))
   widgets))

; (changes) -> (Listof (or key (key "value")))
(define (parse-changes)
  (for/list ([change (in-list (changes))])
    (match-define (list type x) change)
    (case type
      [(set)  (cons 'set (parse-key=value x))]
      [(drop) (list 'drop (string->symbol x))]
      )))

; "key=value" -> key "value"
(define (parse-key=value key=value)
  (match key=value
    [(pregexp "^\\s*([^=]+)\\s*=\\s*(.*)$"
              (list _ key value))
     (list (string->symbol key) value)]
    [error 'parse-key=value "Option setting should follow the format key=value. Given '~a'" key=value]))


;; Execution

(case (action)
  [(   RAW)  (for-each    raw widgets)]
  [(  INFO)  (for-each   info widgets)]
  [(DELETE)  (for-each delete widgets)]
  
  [(CREATE)    (CREATE widgets)]
  [(CLEANUP)   (CLEANUP widgets)]
  [(SET-TYPE)  (SET-TYPE widgets)]
  [(DUPLICATE) (DUPLICATE widgets)]
  [(TO-LEGEND) (TO-LEGEND widgets)]
  
  [else
   (cond
     [(not (null? (changes))) (UPDATE widgets)]
     
     ; Fallback: print widgets information
     [else (for-each info widgets)])])