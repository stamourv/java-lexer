#lang racket/base

(require scribble/core scribble/manual)

(provide (all-defined-out))

;; from Ben Lerner. depends on the `java` collection, which wraps profj stuff
(define-syntax-rule (java-block arg args ...)
  (codeblock #:keep-lang-line? #f #:context #'arg "#lang java\n" arg args ...))
(define-syntax-rule (java arg args ...)
  (make-element (make-style "RktBlk" '(tt-chars)) (code #:lang "java" arg args ...)))
