#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :237)
  (export define-record-type
          define-record-name
	  fields
	  mutable
	  immutable
	  parent
	  protocol
	  sealed
	  opaque
	  nongenerative
          generative
	  parent-rtd
	  record-type-descriptor
	  record-constructor-descriptor
	  make-record-type-descriptor
	  record-type-descriptor?
	  make-record-descriptor
	  make-record-constructor-descriptor
	  record-descriptor-rtd
          record-descriptor-parent
	  record-descriptor?
	  record-constructor-descriptor?
	  record-constructor
	  record-predicate
	  record-accessor
	  record-mutator
	  record?
	  record-rtd
	  record-type-name
	  record-type-parent
	  record-type-uid
	  record-type-generative?
	  record-type-sealed?
	  record-type-opaque?
	  record-type-field-names
	  record-field-mutable?
          record-uid->rtd
          port-write-rtd port-read-rtd)
  (import (srfi :237 records)))

;; Local Variables:
;; mode: scheme
;; End:
