(define-library (srfi 128)
  (import (scheme base))
  (import (scheme case-lambda)
	  (scheme complex)
	  (scheme char)
	  (scheme inexact))
  (export comparator? comparator-ordered? comparator-hashable?)
  (export make-comparator
          make-pair-comparator make-list-comparator make-vector-comparator
          make-eq-comparator make-eqv-comparator make-equal-comparator)
  (export boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash symbol-hash number-hash)
  (export make-default-comparator default-hash comparator-register-default!)
  (export comparator-type-test-predicate comparator-equality-predicate
        comparator-ordering-predicate comparator-hash-function)
  (export comparator-test-type comparator-check-type comparator-hash)
  (export hash-bound hash-salt)
  (export =? <? >? <=? >=?)
  (export comparator-if<=>)
  (include "128/comparators-impl.scm")
  (include "128/default.scm")
)
