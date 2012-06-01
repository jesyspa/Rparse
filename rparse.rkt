#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

(define-tokens rparse-tok
  (number symbol))

(define-empty-tokens rparse-empty-tok
  (lparen rparen comma dot at tilde))

(define rparse-lexer
  (lexer
    [(re-+ numeric) (token-number (string->number lexeme))]
    [(re-+ symbolic) (token-symbol lexeme)]
    [#\( (token-lparen)]
    [#\) (token-rparen)]
    [#\, (token-comma)]
    [#\. (token-dot)]
    [#\@ (token-at)]
    [#\~ (token-tilde)]))


;;; Tests

(require rackunit)

(provide lex-tests)

(define lex-tests
  (test-suite
   "Tests for the lexer."
   
   (test-case
    "Numbers are lexed correctly."
    (let ([result (rparse-lexer (open-input-string "5"))])
      (check-equal? (token-name result) 'number)
      (check-equal? (token-value result) 5))
    (let ([result (rparse-lexer (open-input-string "103"))])
      (check-equal? (token-name result) 'number)
      (check-equal? (token-value result) 103)))
   
   (test-case
    "Symbols are lexed correctly."
    (let ([result (rparse-lexer (open-input-string "x"))])
      (check-equal? (token-name result) 'symbol)
      (check-equal? (token-value result) "x")))))