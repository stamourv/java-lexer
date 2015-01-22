#lang s-exp syntax/module-reader
--ignored--
#:read java-read
#:read-syntax java-read-syntax
#:info java-get-info

(require scribble/core)
(require profj/parsers/lexer)

(define (java-read in)
  (syntax->datum (java-read-syntax #f in)))

(define (java-read-syntax src in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    (if (eq? type 'eof)
        eof
        (datum->syntax #f
                       type
                       (list src #f #f start (- end start))))))

;; convert profj types to racket-lexer types
(define (convert-type type lexeme)
  (case type
    [(profj-type profj-prim-type)        'symbol]
    [(profj-string)                      'string]
    [(profj-literal literal)             'constant]
    [(profj-constant)                    'symbol]
    [(profj-comment profj-block-comment) 'comment]
    [(default)                           'parenthesis] ; includes semi
    [(profj-keyword keyword)
     ;; includes both keywords like `static` and parentheses, etc.
     (if (regexp-match #px"[[:alpha:]]" lexeme)
         'symbol
         'parenthesis)]
    [else type]))

(define (get-color-lexer in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    (values lexeme
            (convert-type type lexeme)
            data
            start
            end)))

(define (java-get-info key default default-filter)
  (case key
    [(color-lexer)
     get-color-lexer]
    [else
     (default-filter key default)]))
