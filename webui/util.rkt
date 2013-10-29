#lang at-exp racket/base

(provide pprint-html-string
         legend)


(define r string-append)

(define (pprint-html-string str)
  (regexp-replaces str
    '([#rx"&quot;" "'"]
      [#rx"&lt;" "<"]
      [#rx"&gt;" ">"])))


(define (legend wname index)
  @r{$("[data-widget\\.uri='http://localhost:12001/WidgetStore/@|wname|']").awf.specifiedOptions('contents.partition', 'literal:{"list" : ["@|index|"]}').specifiedOptions('aimms.widget.type', 'literal:"legend"').specifiedOptions('contents', 'aimms:{"contents": ["@|index|"]}')})
