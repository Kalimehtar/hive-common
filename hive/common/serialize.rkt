#lang racket/base
(require racket/contract)
(define id? exact-nonnegative-integer?)
(provide (contract-out
          [struct object ((id id?))]
          [struct ref ((typename symbol?) (id id?))]
          [find-by-ref (id? (listof object?) . -> . (or/c #f object?))]
          [serializable? (any/c . -> . boolean?)]
          [deserialize (any/c (-> ref? any/c) . -> . any/c)]
          [serialize ((serializable?) ((serializable? . -> . serializable?)) . ->* . list?)]
          [struct-name (serializable? . -> . symbol?)])
         struct/serialize)

(require racket/function 
         racket/generic
         (for-syntax racket/base
                     racket/struct-info
                     racket/list))

(struct ref (typename id) #:prefab)

(define (find-by-ref id list-of-objs)
  (findf (λ (o) (= (object-id o) id)) list-of-objs))

(define-generics serializable
  (serialize serializable [prepare])
  (struct-name serializable)
  #:defaults
  ([any/c
    (define (serialize s [p identity]) (p s))]))

(define (serialize-ref data)
  (cond
    [(object? data) (ref (struct-name data) (object-id data))]
    [(pair? data) (fcons serialize-ref data)]
    [else data]))      

(define-syntax-rule (make-serializer struct)
  (let* ([accessors (accessors struct)])
    (λ (el [prepare identity])
      (define el* (prepare el))
      (map (λ (accessor) (serialize-ref (accessor el*))) accessors))))

(define-syntax-rule (struct/serialize name rest ...)
  (begin
    (define (%serialize s prep) #f)
    (struct name rest ...
      #:methods gen:serializable
      [(define (serialize s [prepare identity]) (%serialize s prepare))
       (define (struct-name s) 'name)])
    (set! %serialize (make-serializer name))))

(struct/serialize object (id))

(define (deserialize item [deref identity])
  (define (rec x) (deserialize x deref))
  (cond
    [(ref? item) (deref item)]
    [(pair? item) (fcons rec item)]
    [else item]))

(define (fcons proc x)
  (cons (proc (car x)) (proc (cdr x))))

; returns list of accessors for given struct
(define-syntax (accessors stx)
  (syntax-case stx ()
    [(_ id) (datum->syntax stx 
                           `(list ,@(reverse (fourth (extract-struct-info 
                                                      (syntax-local-value #'id))))))]))
