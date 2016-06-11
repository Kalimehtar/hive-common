#lang scribble/manual
@require[scribble/eval (for-label racket/base
                                  racket/function
                                  hive/common/read-write
                                  hive/common/serialize
                                  hive/common/users)]

@title{Hive}

@author+email["Roman Klochkov" "kalimehtar@mail.ru"]

Hive is the framework for a client-server application with persistent object storage on server side.

@defmodule[hive/common]

This package provides functions, that will be used both on client and server sides of the hive application.

@section{Network read and write}

@defmodule[hive/common/read-write]

This library provides read and write for network streams. The connection is unreliable. I know, that TCP is considered
reliable, but in fact, when network infrastucture drops part of packets, it may recover too slow. So,
Hive uses keepalive packets and tools to control read timeout.

@defproc[(write/flush [data any/c] [out output-port?]) void?]{
Sends @racket[data] to @racket[out] and flushes @racket[out].}

@defproc[(read/timeout [in input-port? (current-input-port)]
                       [timeout (or/c #f (and/c real? (not/c negative?)) (-> any)) 30]) any]{
Reads from @racket[in] like @racket[read]. If @racket[read] is not completed in @racket[timeout],
returns @racket[eof]. @racket[timeout] is treated like in @racket[sync/timeout]}

@section{Serializable objects}

@defmodule[hive/common/serialize]

This library provide serializable objects. It differs from @racketmodname[racket/serialize] in that it
doesn't deep copy of the object. It rather replaces all field values, that references to other @racket[object]s
to special @racket[ref] structure. This way the library may be used to send object by network or save to file
without saving all linked objects.

@defstruct[object ([id exact-nonnegative-integer?])]{
  A structure type for objects, that may be used in serializable structure.
Object types are expected to inherit this structure.}

@defstruct[ref ([typename symbol?] [id exact-nonnegative-integer?])]{
  A structure type for references to objects. When object is serialize,
all @racket[object]s in its fields are replaced to @racket[ref]s.}

@define[helper-eval (make-base-eval)]
@interaction-eval[#:eval helper-eval
                  (require hive/common/serialize)]
@defproc[(find-by-ref [id exact-nonnegative-integer?]
                      [objects (listof object?)]) (or/c #f object?)]{
Returns an object with id equals to @racket[id] from @racket[objects]. If there is no such object, then returns #f.

@examples[#:eval helper-eval
          (find-by-ref 2 (list (object 5) (object 6)))
          (find-by-ref 5 (list (object 5) (object 6)))]}

@defform[(struct/serialize name rest ...)]{Constracts new serializable structure. Has same subforms as @racket[struct]}

@defproc[(serializable? [obj any/c]) boolean?]{Is a predicate for serializable objects.}

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

@section{Users}

@defmodule[hive/common/users]

@defstruct[(users object) ([name string?] [password string?] [role symbol?] [online #f]) #:mutable]{
Represent user of Hive.
@itemlist[
 @item{@racket[name] - user name, any unicode characters allowed;}
 @item{@racket[password] - user password;}
 @item{@racket[role] - user role. Hive accepts @racket['admin] and @racket['user], but application may
   invent it's own;}
 @item{@racket[online] - #t, iff user is logged on and sending keepalive packets.}]}