#lang racket

(require unstable/contract)

(provide (contract-out
          [env? (any/c . -> . boolean?)]
          [make-env ((option/c env?) . -> . env?)]
          [env-lookup (env? string? . -> . any/c)]
          [env-define! (env? string? any/c . -> . any/c)]
          [env-parent (env? . -> . (option/c env?))]))
         
(struct env (parent values))

(define (make-env parent)
  (env parent (make-hash)))

(define (get-hash-with e str)
  (cond
    [(hash-has-key? (env-values e) str) (env-values e)]
    [(env-parent e) (get-hash-with (env-parent e) str)]
    [else #f]))

(define (env-define! e str value)
  (hash-set! (env-values e) str value))

(define (env-lookup e str)
  (hash-ref (get-hash-with e str) str))

;;; Tests

(require rackunit)

(provide env-tests)

(define env-tests
 (test-suite
  "Tests for the environment."
  
  (test-case
   "Check that creation is possible."
   (check-equal? (env? (make-env #f)) #t))
  
  (test-case
   "Check that adding a value works."
   (let ([e (make-env #f)])
     (env-define! e "x" 5)
     (check-equal? (hash-ref (env-values e) "x") 5)))
  
  (test-case
   "Check that looking values up works."
   (let ([e (make-env #f)])
     (env-define! e "x" 5)
     (check-equal? (env-lookup e "x") 5)))))