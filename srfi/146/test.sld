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

(define-library (srfi 146 test)
  (export run-tests)
  (import (scheme base)
	  (srfi 64)
	  (srfi 128)
	  (srfi 146))
  (begin
    (define (run-tests)
      (test-begin "SRFI 146")

      (test-group "Predicates"
	(define map0 (make-map comparator))
	(define map1 (make-map comparator 'a 1 'b 2 'c 3))
	(define map2 (make-map comparator 'c 1 'd 2 'e 3))
	(define map3 (make-map comparator 'd 1 'e 2 'f 3))
	
	(test-assert "map?: a map"
	  (map? (make-map comparator)))

	(test-assert "map?: not a map"
	  (not (map? (list 1 2 3))))
	
	(test-assert "map-empty?: empty map"
	  (map-empty? map0))
	
	(test-assert "map-empty?: non-empty map"
	  (not (map-empty? map1)))

	(test-assert "map-contains?: containing"
	  (map-contains? map1 'b))   

	(test-assert "map-contains?: not containing"
	  (not (map-contains? map1 '2)))

	(test-assert "map-disjoint?: disjoint"
	  (map-disjoint? map1 map3))

	(test-assert "map-disjoint?: not disjoint"
	  (not (map-disjoint? map1 map2))))

      (test-group "Accessors"
	(define map1 (make-map comparator 'a 1 'b 2 'c 3))

	(test-equal "map-ref: key found"
	  2
	  (map-ref map1 'b))

	(test-equal "map-ref: key not found/with failure"
	  42
	  (map-ref map1 'd (lambda () 42)))

	(test-error "map-ref: key not found/without failure"
	  (map-ref map1 'd))

	(test-equal "map-ref: with success procedure"
	  (* 2 2)
	  (map-ref map1 'b (lambda () #f) (lambda (x) (* x x))))

	(test-equal "map-ref/default: key found"
	  3
	  (map-ref/default map1 'c 42))

	(test-equal "map-ref/default: key not found"
	  42
	  (map-ref/default map1 'd 42))

	(test-equal "map-key-comparator"
	  comparator
	  (map-key-comparator map1)))

      (test-group "Updaters"
	(define map1 (make-map comparator 'a 1 'b 2 'c 3))
	(define map2 (map-set map1 'c 4 'd 4 'd 5))
	
	(test-equal "map-set: key already in map"
	  3
	  (map-ref map2 'c))

	(test-equal "map-set: key set earlier"
	  4
	  (map-ref map2 'd))

	(test-equal "map-replace: key not in map"
	  #f
	  (map-ref/default (map-replace map1 'd 4) 'd #f))

	(test-equal "map-replace: key in map"
	  6
	  (map-ref (map-replace map1 'c 6) 'c))

	(test-equal "map-delete"
	  42
	  (map-ref/default (map-delete map1 'b) 'b 42))

	(test-equal "map-delete-all"
	  42
	  (map-ref/default (map-delete-all map1 '(a b)) 'b 42))
       
	
	)
      
      
      (test-end "SRFI 146"))

    (define comparator (make-default-comparator))))
