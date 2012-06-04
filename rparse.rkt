#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in re- parser-tools/lex-sre))

(provide (contract-out
          [rtree? (any/c . -> . boolean?)]
          [rparse (input-port? . -> . rtree?)]
          [rparse-string (string? . -> . rtree?)]))


(define (rtree? tree)
  (cond
    [(not (list? tree)) #f]
    [(equal? (first tree) 'number) (and (= (length tree) 2)
                                        (number? (second tree)))]
    [(equal? (first tree) 'symbol) (and (= (length tree) 2)
                                        (string? (second tree)))]
    [(equal? (first tree) 'magic) (= (length tree) 1)]
    [(equal? tree '(boolean #f)) #t]
    [(equal? tree '(boolean #t)) #t]
    [(equal? (first tree) 'list) (andmap rtree? (cdr tree))]
    [else #f]))

;; Token and regex definitions

(define-tokens rparse-tok
  (number symbol))

(define-empty-tokens rparse-empty-tok
  (lparen rparen comma dot at tilde quote qquote eof))

(define-lex-abbrev not-allowed
  (re-or #\( #\) #\, #\. #\` #\' #\@ #\~))

(define-lex-abbrev allowed-any-char
  (re-- (re-or alphabetic punctuation symbolic)
        not-allowed))

(define-lex-abbrev allowed-nonfirst-char
  (re-* (re-or allowed-any-char numeric)))

;; Lexer and parser

(define rparse-lexer
  (lexer
   [(re-+ numeric) (token-number (string->number lexeme))]
   [(re-: allowed-any-char allowed-nonfirst-char)
    (token-symbol lexeme)]
   [#\( (token-lparen)]
   [#\) (token-rparen)]
   [#\, (token-comma)]
   [#\. (token-dot)]
   [#\' (token-quote)]
   [#\` (token-qquote)]
   [#\@ (token-at)]
   [#\~ (token-tilde)]
   [blank (rparse-lexer input-port)]
   [(eof) (token-eof)]))

(define rparse-parse
  (parser
   (grammar
    (expr ((number) `(number ,$1))
          ((symbol) `(symbol ,$1))
          ((tilde) `(magic))
          ((quote expr) `(list (magic) (symbol "quote") ,$2))
          ((qquote expr) `(list (magic) (symbol "quasiquote") ,$2))
          ((comma expr) `(list (symbol "unquote") ,$2))
          ((comma at expr) `(list (symbol "unquote-splice") ,$3))
          ((lparen list rparen) $2))
    (list ((list dot rhslist) `(list ,@(cons $1 $3)))
          ((rhslist) `(list ,@$1)))
    (rhslist ((expr rhslist) (cons $1 $2))
             (() '())))
   (tokens rparse-tok rparse-empty-tok)
   (start expr)
   (end eof)
   (error (lambda (tok-ok? tok-name tok-value)
            (printf "Error: ~a ~a ~a" tok-ok? tok-name tok-value)))))

(define (rparse input-port)
  (rparse-parse (λ () (rparse-lexer input-port))))

(define (rparse-string str)
  (rparse (open-input-string str)))

;;; Tests

(require rackunit)

(provide rtree-tests
         lex-tests
         parse-tests)

(define rtree-tests
  (test-suite
   "Ensure that only valid ASTs are accepted."
   
   (test-case
    "Ensure only valid numbers are accepted as such."
    (check-equal? (rtree? '(number 5)) #t)
    (check-equal? (rtree? '(number 5 6)) #f)
    (check-equal? (rtree? '(number)) #f)
    (check-equal? (rtree? '(number "x")) #f))
   
   (test-case
    "Ensure only valid symbols are accepted as such."
    (check-equal? (rtree? '(symbol "x")) #t)
    (check-equal? (rtree? '(symbol "x" "y")) #f)
    (check-equal? (rtree? '(symbol)) #f)
    (check-equal? (rtree? '(symbol 5)) #f))
   
   (test-case
    "Ensure valid lists are accepted."
    (check-equal? (rtree? '(list)) #t)
    (check-equal? (rtree? '(list (number 5))) #t))
   
   (test-case
    "Ensure invalid lists are rejected."
    (check-equal? (rtree? '(list 5)) #f))
   
   (test-case
    "Ensure only true magic is accepted."
    (check-equal? (rtree? '(magic)) #t)
    (check-equal? (rtree? '(magic 5)) #f))))

(define (lex-check-equal? string type value)
  (let ([result (rparse-lexer (open-input-string string))])
    (check-equal? (token-name result) type)
    (check-equal? (token-value result) value)))

(define lex-tests
  (test-suite
   "Tests for the lexer."
   
   (test-case
    "Numbers are lexed correctly."
    (lex-check-equal? "5" 'number 5)
    (lex-check-equal? "100" 'number 100)
    (lex-check-equal? "0" 'number 0))
   
   (test-case
    "Symbols are lexed correctly."
    (lex-check-equal? "x" 'symbol "x")
    (lex-check-equal? "+" 'symbol "+")
    (lex-check-equal? "-" 'symbol "-")
    (lex-check-equal? "*" 'symbol "*")
    (lex-check-equal? "/" 'symbol "/")
    (lex-check-equal? "%" 'symbol "%")
    (lex-check-equal? "?" 'symbol "?")
    (lex-check-equal? "a5" 'symbol "a5"))
   
   (test-case
    "Literals are lexed correctly."
    (lex-check-equal? "(" 'lparen #f)
    (lex-check-equal? ")" 'rparen #f)
    (lex-check-equal? "," 'comma #f)
    (lex-check-equal? "." 'dot #f)
    (lex-check-equal? "'" 'quote #f)
    (lex-check-equal? "`" 'qquote #f)
    (lex-check-equal? "@" 'at #f)
    (lex-check-equal? "~" 'tilde #f))))

(define parse-tests
  (test-suite
   "Tests for the parser"
   
   (test-case
    "Ensure symbols are parsed correctly."
    (check-equal? (rparse-string "x") '(symbol "x")))
   
   (test-case
    "Ensure numbers are parsed correctly."
    (check-equal? (rparse-string "5") '(number 5)))
   
   (test-case
    "Ensure that lists are parsed correctly."
    (check-equal? (rparse-string "()") '(list))
    (check-equal? (rparse-string "(5 5)") '(list (number 5) (number 5)))
    (check-equal? (rparse-string "(5 . 5)") '(list (list (number 5)) (number 5)))
    (check-equal? (rparse-string "(. 5)") '(list (list) (number 5)))
    (check-equal? (rparse-string "(5 .)") '(list (list (number 5)))))
   
   (test-case
    "Ensure quotes are parsed correctly."
    (check-equal? (rparse-string "'()") '(list (magic) (symbol "quote") (list)))
    (check-equal? (rparse-string "`()")
                  '(list (magic) (symbol "quasiquote") (list))))
   
   (test-case
    "Ensure unquotes are parsed correctly."
    (check-equal? (rparse-string ",x") '(list (symbol "unquote") (symbol "x")))
    (check-equal? (rparse-string ",@x") '(list (symbol "unquote-splice") (symbol "x"))))))