;;; SPDX-FileCopyrightText: 2025 Arthur A. Gleckler
;;; SPDX-License-Identifier: MIT

(import (gleckler hamt-test)
	(gleckler hamt-misc-test)
	(gleckler hamt-map-test)
	(gleckler vector-edit-test))

(run-hamt-core-tests)
(run-hamt-misc-tests)
(run-hamt-map-tests)

(run-vector-edit-tests)
