#lang racket

(provide (all-defined-out))

;; Testing setup

(module+ test (require rackunit))

;; Parameters

(define mode (make-parameter "http"))
(define host (make-parameter "localhost"))
(define port (make-parameter 12001))

;; Widgets

(struct widget (name serial raw) #:prefab #:mutable)

; (or widget? symbol? string? bytes?) -> string?
; Always return a string
(define (ensure-wname w)
  (cond [(widget? w) (ensure-wname (widget-name w))]
        [(symbol? w) (symbol->string w)]
        [(string? w) w]
        [(bytes? w) (bytes->string/utf-8 w)]
        [else (error 'ensure-wname "Cannot infer widget name from ~a." w)]))

(module+ test
  (check-equal? "widget" (ensure-wname "widget"))
  (check-equal? "tehdiw" (ensure-wname 'tehdiw))
  (check-equal? "wgname" (ensure-wname (widget "wgname" #f #f)))
  (check-exn exn:fail? (lambda () (ensure-wname 12)))
  (check-exn exn:fail? (lambda () (ensure-wname (widget 42 #f #f)))))


;; HTTP stuff

(require net/http-client)

; wname? -> string?
; Example: "SomeValue" -> "http://localhost:12001/widgetStore/SomeValue"
(define (widget-store-url wname)
  (format "~a://~a:~a/WidgetStore/~a"
          (mode) ; parameter
          (host) ; parameter
          (port) ; parameter
          (ensure-wname wname)))

(module+ test
  (parameterize ([mode "xyzw"]
                 [host "distanthost"]
                 [port 1234])
    (check-equal? "xyzw://distanthost:1234/WidgetStore/Frubas"
                  (widget-store-url "Frubas"))))


(define (widget-store-request uri
                              #:method  [method #"GET"]
                              #:headers [headers '()]
                              #:data    [data #f])
  (define-values (response-status response-headers in)
    (http-sendrecv (host) uri
                   #:port (port)
                   #:version #"1.1"
                   #:method method
                   #:headers headers
                   #:data data))
  (values (parse-http-status response-status)
          (parse-http-headers response-headers)
          (port->string in)))

;; bytes -> (list number string)
(define (parse-http-status status)
  (define m (regexp-match #px"^HTTP/\\d.\\d (\\d+) (.*)$" status))
  (if m
      (list (string->number (bytes->string/utf-8 (cadr m))) 
            (string-trim (bytes->string/utf-8 (caddr m))))
      (error 'parse-http-status "Unknown HTTP response: ~a." status)))

(module+ test
  (check-equal? (parse-http-status #"HTTP/1.1 404 Not Found.") '(404 "Not Found."))
  (check-equal? (parse-http-status #"HTTP/1.1 123 (Okidoki) ") '(123 "(Okidoki)")))


(define (parse-http-headers headers)
  (for/fold ([parsed (make-immutable-hasheq)])
    ([header (in-list headers)])
    (define-values (key value) (parse-http-header header))
    (if (hash-ref parsed key #f)
        (hash-set parsed key (cons value (hash-ref parsed key)))
        (hash-set parsed key (list value)))))

(define (parse-http-header header)
  (define m (regexp-match #px"^([^:]+):(.*)$" (ensure-string header)))
  (if m
      (values (string->symbol (string-trim (cadr m)))
              (string-trim (caddr m)))
      (error 'parse-http-header "Unable to parse header: '~a'" header)))

(define (sole-header-value headers key [default #f])
  (define values (hash-ref headers key #f))
  (if values
      (car values)
      default))

;; (or string bytes symbol) -> string/utf-8
(define (ensure-string x)
  (cond [(string? x) x]
        [(bytes? x) (bytes->string/utf-8 x)]
        [symbol? x] (symbol->string x)))


;; Get and parse widget

(require sxml)

(define (get-raw-widget wname)
  (define-values (status headers data)
    (widget-store-request (widget-store-url wname)))
  (if (not (= 404 (car status)))
      (values data (string->number (sole-header-value headers 'Serial "0")))
      (values #f 0)))

(define (get-widget wname)
  (define-values (raw serial) (get-raw-widget wname))
  (and raw (widget wname serial (ssax:xml->sxml (open-input-string raw) '()))))

(define (widget-exists? wname)
  (define-values (raw serial) (get-raw-widget wname))
  (not (not raw)))

(define (stringable? x)
  (or (string? x) (bytes? x) (symbol? x)))


(define (ensure-widget w)
  (cond 
    [(widget? w) w]
    [(stringable? w) (get-widget (ensure-string w))]
    [else (error 'ensure-widget "Can't get widget from '~a'." w)]))

;; SXML related


(define (sxml-element w)
  (car (sxml:content (widget-raw w))))

(define (widget-properties w)
  (sxml:attr-list (sxml-element (ensure-widget w))))

(define (widget-property w property)
  (sxml:attr (sxml-element (ensure-widget w)) property))

(define (set-widget-property w property value)
  (define ww (ensure-widget w))
  (struct-copy widget ww
               [raw `(*TOP* 
                      ,(sxml:set-attr 
                        (sxml-element ww) 
                        (list property value)))]))

(define (set-widget-property! w property value)
  (set-widget-raw! w
                   `(*TOP* 
                     ,(sxml:set-attr 
                       (sxml-element w) 
                       (list property value)))))

(define (set-widget-properties w properties)
  (for/fold [(widget (ensure-widget w))]
    ([property (in-list properties)])
    (match-define (list key value) property)
    (set-widget-property widget key value)))

(define (set-widget-properties! w properties)
  (for ([property (in-list properties)])
    (match-define (list key value) property)
    (set-widget-property! w key value)))


;; Put widget

(define (put-raw-widget wname serial html)
  (define-values (status _ __)
    (widget-store-request (widget-store-url wname)
                          #:method 'PUT
                          #:headers (list (format "Serial : ~a" (add1 serial)))
                          #:data html))
  status)

(define (put-widget w)
  (put-raw-widget 
   (widget-name w) 
   (widget-serial w)
   (srl:sxml->html (widget-raw w))))


(define-syntax (with-widget stx)
  (syntax-case stx ()
    [(_ (var wname) body ...)
     #'(let ([var (get-widget wname)])
         (when var
           body ...
           (put-widget var)
           (void)))]))
