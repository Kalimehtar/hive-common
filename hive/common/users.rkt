#lang racket
(provide (struct-out user))

(require "serialize.rkt")

(struct/serialize user object (name password role online) #:mutable)