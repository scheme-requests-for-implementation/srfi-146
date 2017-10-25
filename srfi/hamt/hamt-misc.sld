(define-library (hamt-misc)
  (import (scheme base)
	  (only (srfi 113) set)
	  (only (srfi 125) make-hash-table string-hash)
	  (only (srfi 128) make-comparator))
  (export assert do-list
	  make-string-hash-table
	  make-string-set
	  with-output-to-string)
  (include "hamt-misc.scm"))