;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (gleckler hamt-test)
  (import (scheme base)
	  (chibi test)
	  (only (gleckler hamt) fragment->mask))
  (export run-hamt-core-tests)
  (include "hamt-test.scm"))