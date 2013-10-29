#lang racket/base

(require "config.rkt"
         "util.rkt"
         
         racket/match
         net/http-client
         racket/port
         sxml)

(provide (struct-out widget)
         
         ; Raw HTTP interface
         get-widget
         delete-widget
         put-widget
         widget-sxml
         set-widget-sxml!
         
         ; Nicer HTTP interface
         info
         raw
         delete
         create
         duplicate
         update
         
         ; Widget struct manipulation
         widget-property
         widget-properties
         set-widget-properties!
         drop-widget-properties!
         
         ; Utilities
         widget->url
         widget->string
         widget->html
         (all-from-out "util.rkt")
         
         ; Cleanup
         cleanup
         
         ; hostname and port-no parameters
         (all-from-out "config.rkt"))


(struct hidden (value) #:mutable)
(struct widget (name serial sxml%) #:transparent #:mutable)

(define (widget-sxml w)
  (hidden-value (widget-sxml% w)))

(define (set-widget-sxml! w value)
  (set-hidden-value! (widget-sxml% w) value))

(define (widget-property widget property)
  (define element (car (sxml:content (widget-sxml widget))))
  (sxml:attr element property))

(define (widget-properties widget)
  (define element (car (sxml:content (widget-sxml widget))))
  (sxml:attr-list element))

(define (set-widget-properties! widget properties)
  (define element
    (for/fold ([element (car (sxml:content (widget-sxml widget)))])
      ([property (in-list properties)])
      (sxml:set-attr element property)))
  (set-widget-sxml! widget `(*TOP* ,element)))

(define (drop-widget-properties! widget properties)
  (define new-sxml
    (for/fold ([sxml (widget-sxml widget)])
      ([property (in-list properties)])
      ((sxml:modify (list (format "// @ ~a" property) 'delete))
       sxml)))
  (set-widget-sxml! widget new-sxml))


;; widget -> string
(define (widget->string widget)
  (format "Widget '~a', id '~a', serial ~a."
          (widget-name widget)
          (widget-property widget 'id)
          (widget-serial widget)))

; widget -> html
(define (widget->html widget)
  (srl:sxml->html (widget-sxml widget)))


;; raw HTTP interface

;; string -> widget
(define (get-widget widget-name [parse #t])
  (define url (widget->url widget-name))
  (define-values (status headers in)
    (http-sendrecv (hostname) url #:port (port-no) #:method 'GET))
  (cond
    [(equal? 404 (status-code status)) #f]
    [parse
     (define sxml (ssax:xml->sxml in '()))
     (define heads (parse-header headers))
     (widget widget-name (string->number (car (hash-ref heads 'Serial '(0)))) (hidden sxml))]
    [else (values (port->string in) status)]))


;; widget -> (void)
(define (put-widget widget)
  (define url (widget->url (widget-name widget)))
  (define serial (add1 (widget-serial widget)))
  (cleanup widget)
  (define-values (status headers in)
    (http-sendrecv (hostname) url
                   #:port (port-no)
                   #:method 'PUT
                   #:headers (list (format "Serial : ~a" serial))
                   #:data (srl:sxml->html (widget-sxml widget))))
  (cond
    [(equal? 200 (status-code status)) (void)]
    [else (displayln status)]))


;; string -> (void)
(define (delete-widget widget-name)
  (define url (widget->url widget-name))
  (define-values (status headers in)
    (http-sendrecv (hostname) url 
                   #:port (port-no)
                   #:method 'DELETE))
  (cond
    [(equal? 200 (status-code status)) (void)]
    [else (displayln status)]))

;; Widget -> URL
(define (widget->url widget)
  (format "http://~a:~a/WidgetStore/~a"
          (hostname) (port-no) widget))

;; header-line -> (cons key value)
(define (parse-header-line line)
  (match (bytes->string/utf-8 line)
    [(pregexp "^\\s*([^:]+)\\s*:\\s*(.*)$"
              (list _ key value))
     (values (string->symbol key) value)]))

(define (parse-header lines)
  (for/fold ([hash (make-immutable-hasheq)])
    ([line (in-list lines)])
    (define-values (key value) (parse-header-line line))
    (if (hash-ref hash key #f)
        (hash-set hash key (cons value (hash-ref hash key)))
        (hash-set hash key (cons value '())))))

;; Status line parsing
(define (parse-status-line status)
  (match (bytes->string/utf-8 status)
    [(pregexp "^HTTP/(\\d\\.\\d) (\\d+) (.*)$" 
              (list _ version code message))
     (values version (string->number code) message)]))

;; Status line return code
(define (status-code status)
  (define-values (version code message)
    (parse-status-line status))
  ; ignore version message
  code)


;; Nicer HTTP interface

;; Actions

(define (call-with-widget name fn)
  (define widget (get-widget name))
  (if widget
      (fn widget)
      (displayln (format "Widget ~a not found." name))))

; Displays widget name, id and serial
(define (info w)
  (with-existing-widget
   (widget w)
   (displayln (widget->string widget))))

; Displays raw html representation
(define (raw w)
  (with-existing-widget
   (widget w)
   (displayln 
    (pprint-html-string 
     (widget->html widget)))))

; DELETE widget from server
(define (delete w)
  (with-existing-widget
   (widget w)
   (delete-widget (widget-name widget))))

(define (map-changes! widget changes)
  (for ([change (in-list changes)])
    (match change
      [(list 'set key value)
       (set-widget-properties! widget (list (list key value)))]
      [(list 'drop key)
       (drop-widget-properties! widget (list key))])))

; CREATE create a new widget
(define (create wname id [properties #f])
  (with-fresh-widget 
   (new wname id)
   (when properties (map-changes! new properties))
   (put-widget new)))

; DUPLICATE widget
(define (duplicate wname newname [properties #f])
  (cond 
    [(widget? (get-widget newname))
     (displayln (format "Widget '~a' already exists." newname))]
    [else
     (with-existing-widget 
      (widget wname)
      (set-widget-name! widget newname)
      (set-widget-serial! widget 0)
      (set-widget-properties! widget `((data-widget.uri ,(widget->url newname))))
      (when properties (map-changes! widget properties))
      (put-widget widget))]))


; UPDATE widget properties
(define (update wname changes)
  (call-with-widget 
   wname
   (lambda (widget)
     (map-changes! widget changes)
     (put-widget widget))))
  

(define (ensure-widget w)
  (cond
    [(widget? w) w]
    [(string? w) (get-widget w)]
    [(symbol? w) (get-widget (symbol->string w))]
    [else (error 'invalid-widget-reference)]))

(define-syntax-rule (with-existing-widget (widget w) body ...)
  (begin
    (define widget (ensure-widget w))
    (cond 
      [(widget? widget)
       body ...]
      [else (displayln (format "'~a' is not an existing widget." w))])))

(define-syntax-rule (with-fresh-widget (new wname id) body ...)
  (begin
    (cond [(widget? (get-widget wname))
           (displayln (format "Widget '~a' already exists." wname))]
          [else
           (define new (make-fresh-widget wname id))
           body ...])))
           
(define (make-fresh-widget wname id)
  (widget wname 0
          `(*TOP*
            (div (@ (id ,id) 
                    (data-widget.uri ,(widget->url wname))
                    (data-aimms.widget.type "literal:\"uninitializedwidget\""))))))

;; Cleaning up before send to server

(require json)

(define (parse-literal str) 
  (string->jsexpr str))

(define (parse-aimms str) str)

(define (jsexpr->literal jsexpr)
  (format "literal:~a" (jsexpr->string jsexpr)))

(define (parse-webui-js str)
  (match str
    [(pregexp "^literal:(.*)$" (list _ literal))
     (list 'literal (parse-literal literal))]
    [(pregexp "^aimms:(.*)$" (list _ aimms))
     (list 'aimms (parse-aimms aimms))]))

(define (parse-type w)
  (define typestr (widget-property w 'data-aimms.widget.type))
  (when typestr
    (match-define (list (== 'literal) str)
      (parse-webui-js typestr))
    (string->symbol str)))

(define (cleanup w)
  (case (parse-type w)
    [(page) (cleanup-page-widget w)]
    [else (void)]))

(define (cleanup-page-widget w)
  (define widgets (widget-property w 'data-widgets))
  (when widgets
    (match-define (list (== 'literal) jswidgets)
      (parse-webui-js widgets))
    (set-widget-properties! 
     w (list (list 'data-widgets
                   (jsexpr->literal 
                    (map (lambda (x) (hash-remove x '___OBJECT_ID___)) jswidgets)))))))
