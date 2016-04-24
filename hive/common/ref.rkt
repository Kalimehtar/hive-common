#lang racket/base
(require racket/contract)
(define id? exact-nonnegative-integer?)

(provide (contract-out
          [struct object ((id id?))]
          [struct ref ((typename symbol?) (id id?))]
          [find-by-ref (id? (listof object?) . -> . (or/c #f object?))]))

(struct object (id))

(struct ref (typename id) #:prefab)

(define (find-by-ref id list-of-objs)
  (findf (λ (o) (= (object-id o) id)) list-of-objs))