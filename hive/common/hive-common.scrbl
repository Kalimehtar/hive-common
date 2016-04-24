#lang scribble/manual
@(require scribble/eval (for-label racket/base
                                   racket/function
                                   hive/common/read-write
                                   hive/common/serialize))

@title{Hive}

@author+email["Roman Klochkov" "kalimehtar@mail.ru"]

Hive is the framework for a client-server application with persistent object storage on server side.

@(defmodule hive/common)

This package provides functions, that will be used both on client and server sides of the hive application.

@section{Network read and write}

@(defmodule hive/common/read-write)

This module provides read and write for network streams. The connection is unreliable. I know, that TCP is considered
reliable, but in fact, when ISP drops part of packets, it may recover too slow. So, hive uses keepalive packets
and tools to control read timeout in this module.

@defproc[(write/flush [data any/c] [out output-port?]) void?]{
Sends @racket[data] to @racket[out] and flushes @racket[out].}

@defproc[(read/timeout [in input-port? (current-input-port)]
                       [timeout (or/c #f (and/c real? (not/c negative?)) (-> any)) 30]) void?]{
Reads from @racket[in] like @racket[read]. If @racket[read] is not completed in @racket[timeout],
returns @racket[eof]. @racket[timeout] is treated like in @racket[sync/timeout]}

@section{Serializable objects}

@(defmodule hive/common/serialize)

@defstruct[object ([id exact-nonnegative-integer?])]{
  A strucure type for objects, that may be stored in server storage.
Object types are expected to inherit this structure.}

@defstruct[ref ([typename symbol?] [id exact-nonnegative-integer?])]{
  A structure type for references to objects. When object is stored,
all @racket[object]s in its fields are replaced to @racket[ref]s.}

@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require hive/common/serialize)]
@defproc[(find-by-ref [id exact-nonnegative-integer?]
                      [objects (listof object?)]) (or/c #f object?)]{
Returns an object with id equals to @racket[id] from @racket[objects]. If there is no such object, then returns #f.

@examples[#:eval helper-eval
          (find-by-ref 2 (list (object 5) (object 6)))
          (find-by-ref 5 (list (object 5) (object 6)))]}

@defform[(struct/serialize name rest ...)]{Constracts new serializable structure. Has same subforms as @racket[struct]}

@defproc[(serializable? [obj any/c]) boolean?]{Predicate for serializable objects.}

@defproc[(serialize [obj serializable?]
                    [prepare (serializable? . -> . serializable?) identity]) list?]{
Returns serialization of the @racket[obj]'s content.
This serialization is for use with @racket[write] and @racket[read]. NB: it doesn't contains type of @racket[obj].
Type is expected to be known from other sources.
@examples[#:eval helper-eval
          (struct/serialize test (a b) #:transparent)
          (define data (test (object 3) 5))
          (serialize data)]}

@defproc[(deserialize [data any/c]
                      [deref (ref? . -> . any/c)]) any/c]{Returns deserialized content of the serializable object.
Result is expected be used to constract new object with same fields.
@examples[#:eval helper-eval
          data
          (apply test (deserialize (serialize data)
                                   (λ (r) (object (ref-id r)))))]}