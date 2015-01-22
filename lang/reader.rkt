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

(define (type->style type)
  (lambda (str)
    (let ((style
           (case type
             [(identifier)
              (let [(first-char (string-ref str 0))]
                (cond
                  [(char=? first-char #\I)
                   (make-style "profj-type" (list 'tt-chars))]
                  [(and (> (string-length str) 1) (string=? str (string-upcase str)))
                   (make-style "profj-constant" (list 'tt-chars))]
                  [(and (char>=? first-char #\A) (char<=? first-char #\Z))
                   (make-style "profj-type" (list 'tt-chars))]
                  [else (make-style "profj-identifier" (list 'tt-chars))]))]
             [else (make-style (format "profj-~a" type) (list 'tt-chars))])))
      (make-element style str))))
                    
(define (get-color-lexer in)
  (let-values ([(lexeme type data start end) (get-syntax-token in)])
    (values lexeme
            (if (or (eq? type 'newline) (eq? type 'eof))
                type
                (type->style type))
            data start end)))

(define (java-get-info key default default-filter)
  (case key
    [(color-lexer)
     get-color-lexer]
    [else 
     (default-filter key default)]))
