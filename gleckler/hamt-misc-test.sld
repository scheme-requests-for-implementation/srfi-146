;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (gleckler hamt-misc-test)
  (import (scheme base) (chibi test) (gleckler hamt-misc))
  (export run-hamt-misc-tests)
  (include "hamt-misc-test.scm"))