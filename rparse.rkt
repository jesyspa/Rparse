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
   [(re-: (re-- (re-or alphabetic punctuation symbolic)
                #\( #\) #\, #\. #\@ #\~)
          (re-* (re-or (re-- (re-or alphabetic punctuation symbolic)
                             #\( #\) #\, #\. #\@ #\~)
                       numeric))) (token-symbol lexeme)]
   [#\( (token-lparen)]
   [#\) (token-rparen)]
   [#\, (token-comma)]
   [#\. (token-dot)]
   [#\@ (token-at)]
   [#\~ (token-tilde)]))


;;; Tests

(require rackunit)

(provide lex-tests)

(define-syntax-rule (parse-check-equal? string type value)
  (let ([result (rparse-lexer (open-input-string string))])
    (check-equal? (token-name result) 'type)
    (check-equal? (token-value result) value)))

(define lex-tests
  (test-suite
   "Tests for the lexer."
   
   (test-case
    "Numbers are lexed correctly."
    (parse-check-equal? "5" number 5)
    (parse-check-equal? "100" number 100)
    (parse-check-equal? "0" number 0))
   
   (test-case
    "Symbols are lexed correctly."
    (parse-check-equal? "x" symbol "x")
    (parse-check-equal? "+" symbol "+")
    (parse-check-equal? "-" symbol "-")
    (parse-check-equal? "*" symbol "*")
    (parse-check-equal? "/" symbol "/")
    (parse-check-equal? "%" symbol "%")
    (parse-check-equal? "?" symbol "?")
    (parse-check-equal? "a5" symbol "a5"))
   
   (test-case
    "Literals are lexed correctly."
    (parse-check-equal? "(" lparen #f)
    (parse-check-equal? ")" rparen #f)
    (parse-check-equal? "," comma #f)
    (parse-check-equal? "." dot #f)
    (parse-check-equal? "@" at #f)
    (parse-check-equal? "~" tilde #f))))