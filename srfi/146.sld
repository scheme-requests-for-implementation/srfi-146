;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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

(define-library (srfi 146)
  (export make-map map-unfold
	  map? map-contains? map-empty? map-disjoint?
	  map-ref map-ref/default map-key-comparator
	  map-set map-set! 
	  map-replace map-replace!
	  map-delete map-delete! map-delete-all map-delete-all!
	  map-intern map-intern!
	  map-update map-update! map-update/default map-update!/default
	  map-search map-search!
	  map-size map-find map-count map-any? map-every?
	  map-keys map-values map-entries
	  map-map map-map->list map-for-each map-fold
	  map-filter map-filter!
	  map-remove map-remove!
	  map-partition map-partition!
	  map-copy map->alist alist->map alist->map!
	  map=? map<? map>? map<=? map>=?
	  map-union map-intersection map-difference map-xor
	  map-union! map-intersection! map-difference! map-xor!
	  make-map-comparator
	  map-comparator)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 1)
	  (srfi 8)
      	  (srfi 128)
	  (srfi 145)
	  (nieper rbtree))
  (include "146.scm"))
