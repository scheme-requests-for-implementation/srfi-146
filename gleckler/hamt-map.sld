;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (gleckler hamt-map)
  (import (scheme base)
	  (scheme case-lambda)
	  (only (srfi 1) fold)
	  (srfi 16)
	  (gleckler hamt)
	  (gleckler hamt-misc))
  (export make-phm phm?
	  phm->alist
	  phm/add-alist phm/add-alist!
	  phm/contains?
	  phm/count
	  phm/empty?
	  phm/for-each
	  phm/get
	  phm/immutable
	  phm/keys
	  phm/mutable phm/mutable?
	  phm/put
	  phm/put!
	  phm/remove phm/remove!
	  phm/replace phm/replace!

	  ;; This is only needed by tests:
	  phm/data)
  (include "hamt-map.scm"))