# SPDX-FileCopyrightText: 2025 Arthur A. Gleckler
# SPDX-License-Identifier: MIT

.POSIX:

.SILENT: check check-podman

SCHEME = chibi-scheme $(SCHEME_FLAGS)

check-podman:
	podman build -t gleckler-hamt-test -f test.Dockerfile ..
	podman run --rm -v $(shell cd ../.. && pwd):/srfi:ro gleckler-hamt-test