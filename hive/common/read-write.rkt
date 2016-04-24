#lang racket/base
(require racket/contract)
(provide/contract [write/flush (any/c output-port? . -> . void?)]
                  [read/timeout (() (input-port? (or/c #f (and/c real? (not/c negative?)) (-> any))) . ->* . any/c)])

(define (write/flush data port)
  (write data port)
  (display "\n" port)
  (flush-output port))

(define (read/timeout [in (current-input-port)] [timeout 30])
  (call-in-nested-thread
   (λ ()
     (define master (current-thread))
     (define slave (thread (λ () (thread-send master (read in)))))
     (cond
       [(sync/timeout timeout slave)
        (thread-receive)]
       [else
        (kill-thread slave)
        eof]))))
  