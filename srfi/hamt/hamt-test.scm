;;;; HAMT Core Tests

;;; Copyright MMXV-MMXVII Arthur A. Gleckler.  All rights reserved.

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
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(define-test-group (hash-array-mapped-trie fragment->mask)
  (lambda (input expected)
    (assert (bit-string=? expected (fragment->mask input))))
  '(0 #*00000000000000000000000000000000)
  '(1 #*00000000000000000000000000000001)
  '(2 #*00000000000000000000000000000011)
  '(3 #*00000000000000000000000000000111))

(define-test-group (population-count)
  (lambda (expected bit-string)
    (assert (= expected (population-count bit-string))))
  '(3 #*10101)
  '(5 #*11111)
  '(0 #*00000))