;;; SPDX-FileCopyrightText: 2021 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(define-library (gleckler vector-edit-test)
  (import (scheme base) (chibi test) (gleckler vector-edit))
  (export run-vector-edit-tests)
  (include "vector-edit-test.scm"))