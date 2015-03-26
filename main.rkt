#lang racket/base

(require scribble/core scribble/manual)

(provide (all-defined-out))

;; from Ben Lerner
(define-syntax-rule (java-block arg args ...)
  (codeblock #:keep-lang-line? #f #:context #'arg "#lang java-lexer\n" arg args ...))
(define-syntax-rule (java arg args ...)
  (make-element (make-style "RktBlk" '(tt-chars)) (code #:lang "java-lexer" arg args ...)))
