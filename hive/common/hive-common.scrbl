#lang scribble/manual
@(require scribble/eval (for-label racket/base hive/common/read-write))

@title{Hive}

@author+email["Roman Klochkov" "kalimehtar@mail.ru"]

Hive is the framework for clent-server application with persistent object storage on server side.

@(defmodule hive/common #:packages ("hive-common"))

This package provides functions, that will be used both on client and server sides of the hive application.

@section{Network read and write}

@(defmodule hive/common/read-write #:packages ("hive-common"))

This module provides read and write for network streams. The connection is unreliable. I know, that TCP is considered
reliable, but in fact, when ISP drops part of packets, it may recover too slow. So, hive uses keepalive packets
and tools to control read timeout in this module.

@defproc[(write/flush [data any/c] [out output-port?]) void?]{
Sends @racket[data] to @racket[out] and flushes @racket[out].}

@defproc[(read/timeout [in input-port? (current-input-port)]
                       [timeout (or/c #f (and/c real? (not/c negative?)) (-> any)) 30]) void?]{
Reads from @racket[in] like @racket[read]. If @racket[read] is not completed in @racket[timeout],
returns @racket[eof]. @racket[timeout] is treated like in @racket[sync/timeout]}

@section{Objects and references}

@(defmodule hive/common/ref #:packages ("hive-common"))

@defstruct[object ([id exact-nonnegative-integer?])]{
  A strucure type for objects, that may be stored in server storage.
Object types are expected to inherit this structure.}

@defstruct[ref ([typename symbol?] [id exact-nonnegative-integer?])]{
  A strucure type for references to objects.}

@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require hive/common/ref)]
@defproc[(find-by-ref [id exact-nonnegative-integer?]
                      [objects (listof object?)]) (or/c #f object?)]{
Returns an object with id equals to @racket[id] from @racket[objects]. If there is no such object, then returns #f.

@examples[#:eval helper-eval
          (find-by-ref 2 (list (object 5) (object 6)))
          (find-by-ref 5 (list (object 5) (object 6)))]}