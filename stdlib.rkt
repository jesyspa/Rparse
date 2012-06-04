#lang racket

(require "reval.rkt"
         "rparse.rkt"
         "env.rkt")

(provide (contract-out
          [rnumber? (any/c . -> . boolean?)]
          [rsymbol? (any/c . -> . boolean?)]
          [rmagic? (any/c . -> . boolean?)]
          [rlist? (any/c . -> . boolean?)]
          [rboolean? (any/c . -> . boolean?)]))

(define (rnumber? x)
  (and (list? x)
       (= (length x) 2)
       (equal? (first x) 'number)
       (number? (second x))))

(define (rsymbol? x)
  (and (list? x)
       (= (length x) 2)
       (equal? (first x) 'symbol)
       (string? (second x))))

(define (rmagic? x)
  (and (list? x)
       (= (length x) 1)
       (equal? (first x) 'magic)))

(define (rlist? x)
  (and (list? x)
       (equal? (first x) 'list)))

(define (rboolean? x)
  (or (equal? x '(boolean #f))
      (equal? x '(boolean #t))))

(define-syntax (rdefine stx)
  (syntax-case stx ()
    [(_ (rtype fname ...)
        stmts ...)
     #'(define (fname ...)
         (list 'rtype (begin stmts ...)))]))

(rdefine (number r-+ x y)
  (+ (second x) (second y)))

(rdefine (number r-- x y)
  (- (second x) (second y)))

(rdefine (number r-/ x y)
  (/ (second x) (second y)))

(rdefine (number r-* x y)
  (* (second x) (second y)))

(rdefine (boolean r-number? x)
  (rnumber? x))

(rdefine (boolean r-symbol? x)
  (rsymbol? x))

(rdefine (boolean r-magic? x)
  (rmagic? x))

(rdefine (boolean r-list? x)
  (rlist? x))

(rdefine (boolean r-boolean? x)
  (rboolean? x))

(define (r-quote x)
  x)

(define (r-quasiquote tree)
  (define (r-quasiquote-work x)
    (cond
      [(not (rlist? x)) (list x)]
      [(empty? (cdr x)) (list x)]
      [(equal? (second x) '(symbol "unquote")) (list (reval (third x) (make-std-env)))]
      [(equal? (second x) '(symbol "unquote-splice")) (cdr (reval (third x) (make-std-env)))]
      [else (list `(list ,@(apply append (map r-quasiquote-work (cdr x)))))]))
  (first (r-quasiquote-work tree)))

(define (make-std-env)
  (let ([e (make-env #f)])
    (env-define! e "+" r-+)
    (env-define! e "-" r--)
    (env-define! e "/" r-/)
    (env-define! e "*" r-*)
    (env-define! e "number?" r-number?)
    (env-define! e "symbol?" r-symbol?)
    (env-define! e "magic?" r-magic?)
    (env-define! e "list?" r-list?)
    (env-define! e "boolean?" r-boolean?)
    (env-define! e "quote" r-quote)
    (env-define! e "quasiquote" r-quasiquote)
    e))

;;; Tests

(require rackunit)

(provide stdlib-tests)
(define (reval-string string env)
  (reval (rparse-string string) env))

(define stdlib-tests
  (test-suite
   "Test the standard library functions."
   (test-case
    "Test basic arithmetic"
    (let ([e (make-std-env)])
      (check-equal? (reval-string "(+ 2 2)" e) '(number 4))
      (check-equal? (reval-string "(- 3 2)" e) '(number 1))
      (check-equal? (reval-string "(/ 2 1)" e) '(number 2))
      (check-equal? (reval-string "(* 2 3)" e) '(number 6))))
   
   (test-case
    "Test basic test functions."
    (let ([e (make-std-env)])
      (check-equal? (reval-string "(number? 5)" e) '(boolean #t))
      (check-equal? (reval-string "(magic? ~)" e) '(boolean #t))
      (check-equal? (reval-string "(symbol? 'x)" e) '(boolean #t))
      (check-equal? (reval-string "(list? '())" e) '(boolean #t))
      (check-equal? (reval-string "(boolean? (number? 5))" e) '(boolean #t))
      (check-equal? (reval-string "(number? +)" e) '(boolean #f))
      (check-equal? (reval-string "(symbol? 5)" e) '(boolean #f))))
   
   (test-case
    "Test quasiquote functionality."
    (let ([e (make-std-env)])
      (check-equal? (reval-string "`(a b)" e) '(list (symbol "a") (symbol "b")))
      (check-equal? (reval-string "`(a ,(+ 1 1))" e) '(list (symbol "a") (number 2)))
      (check-equal? (reval-string "`(,@'(1 2))" e) '(list (number 1) (number 2)))))))