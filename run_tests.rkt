#lang racket

(require rackunit/text-ui
         "rparse.rkt"
         "env.rkt"
         "reval.rkt"
         "stdlib.rkt")

(run-tests lex-tests)
(run-tests parse-tests)
(run-tests env-tests)
(run-tests rtree-tests)
(run-tests reval-tests)
(run-tests stdlib-tests)
