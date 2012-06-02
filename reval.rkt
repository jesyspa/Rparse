#lang racket

(require "env.rkt"
         "rparse.rkt")

(provide (contract-out
          [reval (rtree? env? . -> . rtree?)]))

(define (reval tree env)
  (cond
    [(equal? (first tree) 'symbol) (env-lookup env (second tree))]
    [(equal? (first tree) 'list)
     (if (equal? (second tree) '(magic))
         (apply (reval (third tree) env) (cdddr tree))
         (apply (reval (second tree) env) (map (curryr reval env) (cddr tree))))]
    [else tree]))

;;; Tests

(require rackunit)

(provide reval-tests)

(define reval-tests
  (test-suite
   "Ensure reval works correctly."
   
   (test-case
    "Ensure numbers are returned as-is."
    (let ([i '(number 5)])
      (check-equal? (reval i (make-env #f)) i)))
   
   (test-case
    "Ensure symbols are looked up."
    (let ([e (make-env #f)])
      (env-define! e "x" 5)
      (check-equal? (reval '(symbol "x") e) 5)))
   
   (test-case
    "Ensure function calls without magic or arguments work."
    (let* ([i 0]
           [f (lambda () (set! i (+ 1 i)))]
           [e (make-env #f)])
      (env-define! e "f" f)
      (reval '(list (symbol "f")) e)
      (check-equal? i 1)))
   
   (test-case
    "Ensure function calls without magic buth with arguments work."
    (let* ([i 0]
           [f (lambda (x) (check-equal? x '(number 1)) (set! i (second x)))]
           [e (make-env #f)])
      (env-define! e "f" f)
      (reval '(list (symbol "f") (number 1)) e)
      (check-equal? i 1)))
   
   (test-case
    "Ensure function calls with magic work."
    (let* ([i 0]
           [f (lambda (x) (check-equal? x '(symbol "x")) (set! i (+ 1 i)))]
           [e (make-env #f)])
      (env-define! e "f" f)
      (reval '(list (magic) (symbol "f") (symbol "x")) e)
      (check-equal? i 1)))))