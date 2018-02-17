#lang racket/base
(require racket/contract racket/function thread-utils)
(provide/contract [write/flush ((any/c) (output-port?) . ->* . void?)]
                  [read/timeout (() (input-port? timeout/c) . ->* . any/c)]
                  [write/flush/timeout ((any/c) (output-port? timeout/c) . ->* . void?)])

(define (write/flush data [out (current-output-port)])
  (write data out)
  (display "\n" out)
  (flush-output out))

(define (read/timeout [in (current-input-port)] [timeout 30])
  (until-timeout (thunk (read in)) timeout eof))

(define (write/flush/timeout data [out (current-output-port)] [timeout 30])
  (until-timeout (thunk (write/flush data out)) timeout void))



  