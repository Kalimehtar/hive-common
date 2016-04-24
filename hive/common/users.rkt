#lang racket
(provide (struct-out user))

(require "serialize.rkt" "ref.rkt")

(struct/serialize user object (name password role online) #:mutable)