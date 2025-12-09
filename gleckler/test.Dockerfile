# SPDX-FileCopyrightText: 2025 Arthur A. Gleckler
#
# SPDX-License-Identifier: MIT

# Dockerfile for running gleckler HAMT and vector-edit tests
FROM debian:bookworm-slim
RUN apt-get update && \
    apt-get install -y git make gcc libc6-dev && \
    cd /tmp && \
    git clone --depth 1 https://github.com/ashinn/chibi-scheme.git && \
    cd chibi-scheme && \
    make && \
    make install && \
    cd / && \
    rm -rf /tmp/chibi-scheme && \
    apt-get remove -y git gcc libc6-dev && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
WORKDIR /srfi
COPY . /srfi/srfi-146/

# Chibi Scheme includes all SRFIs used except 16.
RUN cd /srfi && ln -sf ../srfi-16 srfi-16

RUN mkdir -p /test
CMD ["sh", "-c", "cd /test && chibi-scheme -I /srfi/srfi-146 /srfi/srfi-146/gleckler/tests.scm"]