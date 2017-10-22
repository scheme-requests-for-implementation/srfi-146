;;;; Persistent Hash Map

;;; Copyright MMXV-MMXVII Arthur A. Gleckler.  All rights reserved.

(declare (usual-integrations))

;;; Public protocol (API)

;; (phm? datum)

;;   Return true iff `datum' is a persistent hash map.

;; (make-phm hash = [alist])

;;   Return a new immutable persistent hash map that uses the `hash'
;;   procedure to hash its keys and `=' to compare them.  If `alist'
;;   is supplied, include all its keys and data in the result.  Later
;;   occurrences of the same key override earlier ones.

;; (phm/count phm)

;;   Return the number of elements in `phm'.

;; (phm/empty? phm)

;;   Return true iff `phm' is empty.

;; (phm/immutable phm #!optional replace)

;;   Return a PHM equivalent to `phm', but that is immutable.  Even if
;;   `phm' is mutable, no change to it will affect the returned one.
;;   If `replace' is supplied, replace the datum associated with each
;;   key whose value has been modified since the PHM was made mutable
;;   with the result of calling `replace' on that key and datum.  This
;;   is useful for converting PHSs (sets) stored as values in a PHM
;;   back into immutable ones when the containing PHM is made
;;   immutable.

;; (phm/mutable phm)

;;   Return a PHM equivalent to `phm', but that is mutable.  If `phm'
;;   was immutable, no change to the returned PHM will affect `phm'.

;; (phm/mutable? phm)

;;   Return true iff `phm' is mutable.

;; (phm/put phm key datum)

;;   Return a PHM equivalent to `phm' except that `datum' is at `key'.

;; (phm/put! phm key datum)

;;   Return a PHM equivalent to `phm' except that `datum' is at `key'.
;;   Modify `phm', which must be mutable, in the process.

;; (phm/replace phm key replace)

;;   Return a PHM equivalent to `phm' except that whatever value is at
;;   `key' has been replaced by `(replace datum)', or `(replace
;;   hamt-null)' if there was no value there already.  If `replace'
;;   returns `hamt-null', the value is removed.

;; (phm/replace! phm key replace)

;;   Return a PHM equivalent to `phm' except that whatever value is at
;;   `key' has been replaced by `(replace datum)', or `(replace
;;   hamt-null)' if there was no value there already.  If `replace'
;;   returns `hamt-null', the value is removed.  Modify `phm', which
;;   must be mutable, in the process.

;; (phm/get phm key [default])

;;   Return the datum stored at `key' in `phm'.  If none is present,
;;   return `default' if it was supplied, or #f if it was not.

;; (phm/contains? phm key)

;;   Return true iff `phm' has a datum at `key'.

;; (phm/remove phm key)

;;   Return a PHM equivalent to `phm' except that there is no datum at
;;   `key'.

;; (phm/remove! phm key)

;;   Return a PHM equivalent to `phm' except that there is no datum at
;;   `key'.  Modify `phm', which must be mutable, in the process.

;; (phm/add-alist phm alist)

;;   Return a PHM equivalent to `phm' except that, for every pair in
;;   `alist', the datum in its cdr is stored in the new PHM at the key
;;   in its car.  Later occurrences of the same key override earlier
;;   ones.

;; (phm/add-alist! phm alist)

;;   Return a PHM equivalent to `phm' except that, for every pair in
;;   `alist', the datum in its cdr is stored in the new PHM at the key
;;   in its car.  Later occurrences of the same key override earlier
;;   ones.  Modify `phm', which must be mutable, in the process.

;; (phm->alist phm)

;;   Return an alist mapping the keys in `phm' to their values.

;; (phm/keys phm)

;;   Return a list of the keys in `phm'.

;; (phm/for-each procedure phm)

;;   Run `procedure' on each key and datum in `phm'.

;;; Implementation of public protocol (API)

(define (phm? datum)
  (and (hash-array-mapped-trie? datum)
       (hamt/payload? datum)))

(define (make-phm hash = #!optional alist)
  (let ((phm (make-hamt = hash #t)))
    (if (default-object? alist)
	phm
	(let ((phm-1 (phm/mutable phm)))
	  (phm/add-alist! phm-1 alist)
	  (phm/immutable phm-1)))))

(define (phm/count phm)
  (assert (phm? phm))
  (hamt/count phm))

(define (phm/empty? phm)
  (assert (phm? phm))
  (hamt/empty? phm))

(define (phm/immutable phm #!optional replace)
  (assert (phm? phm))
  (hamt/immutable phm replace))

(define (phm/mutable phm)
  (assert (phm? phm))
  (hamt/mutable phm))

(define (phm/mutable? phm)
  (assert (phm? phm))
  (hamt/mutable? phm))

(define (phm/put phm key datum)
  (assert (phm? phm))
  (hamt/put phm key datum))

(define (phm/put! phm key datum)
  (assert (phm? phm))
  (hamt/put! phm key datum))

(define (phm/replace phm key replace)
  (assert (phm? phm))
  (hamt/replace phm key replace))

(define (phm/replace! phm key replace)
  (assert (phm? phm))
  (hamt/replace! phm key replace))

(define (phm/get phm key #!optional default)
  (assert (phm? phm))
  (let* ((default (and (not (default-object? default)) default))
	 (result (hamt-fetch phm key)))
    (if (hamt-null? result)
	default
	result)))

(define (phm/contains? phm key)
  (assert (phm? phm))
  (not (hamt-null? (hamt-fetch phm key))))

(define (phm/remove phm key)
  (assert (phm? phm))
  (phm/put phm key hamt-null))

(define (phm/remove! phm key)
  (assert (phm? phm))
  (assert (hamt/mutable? phm))
  (phm/put! phm key hamt-null))

(define (phm/add-alist phm alist)
  (assert (phm? phm))
  (fold-left (lambda (phm a) (phm/put phm (car a) (cdr a))) phm alist))

(define (phm/add-alist! phm alist)
  (assert (phm? phm))
  (do-list (a alist)
    (phm/put! phm (car a) (cdr a)))
  phm)

(define (phm->alist phm)
  (assert (phm? phm))
  (hamt->list phm cons))

(define (phm/data phm)
  (assert (phm? phm))
  (hamt->list phm (lambda (k d) d)))

(define (phm/keys phm)
  (assert (phm? phm))
  (hamt->list phm (lambda (k d) k)))

(define (phm/for-each procedure phm)
  (assert (phm? phm))
  (hamt/for-each procedure phm))

;;; Tests

(define (assert-phm= phm alist)
  (assert (= (length alist) (phm/count phm)))
  (do-list (a alist)
    (assert (phm/contains? phm (car a)))
    (assert (= (cdr a) (phm/get phm (car a) #f))))
  'passed)

(define-test (persistent-hash-map make-phm alist)
  (let* ((alist '(("a" . 1) ("b" . 2)))
	 (phm (make-phm string-hash string=? alist)))
    (assert (not (hamt/mutable? phm)))
    (assert-phm= phm alist)))

(define-test (persistent-hash-map make-phm phm/count)
  (let ((phm (make-phm string-hash string=? '(("a". 1) ("b" . 2)))))
    (assert (= 2 (phm/count phm)))))

(define-test (persistent-hash-map phm/empty?)
  (assert (phm/empty? (make-phm string-hash string=?)))
  (assert (not (phm/empty? (make-phm string-hash string=? '(("a")))))))

(define (phm-random-test put remove transform)
  (define (sort-alist alist)
    (sort alist (lambda (a1 a2) (string<? (car a1) (car a2)))))
  (let ((contents (make-string-hash-table))
	(deleted-keys (make-string-set))
	(deletion-odds 5)
	(max-key-length 5)
	(operations 100))
    (define (random-key)
      (let ((size (1+ (random max-key-length))))
	(with-output-to-string
	  (lambda ()
	    (do-times (i size)
	      (write-char (make-char (+ 97 (random 26)) 0)))))))
    (define (fill-phm i phm)
      (let ((size (hash-table/count contents)))
	(cond ((zero? i) phm)
	      ((and (not (zero? size))
		    (zero? (random deletion-odds)))
	       (let ((key (nth (random size) (hash-table/key-list contents))))
		 (set/add! deleted-keys key)
		 (hash-table/remove! contents key)
		 (fill-phm (-1+ i)
			   (remove phm key))))
	      (else (let* ((key (random-key))
			   (datum (random 1000)))
		      (set/remove! deleted-keys key)
		      (hash-table/put! contents key datum)
		      (fill-phm (-1+ i)
				(put phm key datum)))))))
    (let ((phm (fill-phm operations
			 (transform (make-phm string-hash string=?)))))
      (assert (= (phm/count phm) (hash-table/count contents)))
      (hash-table/for-each contents
			   (lambda (key datum)
			     (assert (= datum (phm/get phm key -1)))
			     (assert (phm/contains? phm key))))
      (do-set (key deleted-keys)
	(assert (= -1 (phm/get phm key -1)))
	(assert (not (phm/contains? phm key))))
      (let ((ht-alist (hash-table->alist contents))
	    (phm-alist (phm->alist phm)))
	(assert (equal? (sort-alist ht-alist)
			(sort-alist phm-alist)))))))

(define-test (persistent-hash-map random pure)
  (phm-random-test phm/put phm/remove (lambda (m) m)))

(define-test (persistent-hash-map random mutate)
  (phm-random-test phm/put! phm/remove! phm/mutable))

(define-test (persistent-hash-map random mixed)
  (define (flip mutate? phm)
    ((if mutate? phm/mutable phm/immutable) phm))
  (phm-random-test (let ((mutate? #t))
		     (lambda (phm key datum)
		       (set! mutate? (not mutate?))
		       ((if mutate? phm/put! phm/put)
			(flip mutate? phm)
			key
			datum)))
		   (let ((count 0))
		     (lambda (phm key)
		       (set! count (remainder (1+ count) 3))
		       (let ((mutate? (zero? count)))
			 ((if mutate? phm/remove! phm/remove)
			  (flip mutate? phm)
			  key))))
		   (lambda (m) m)))

(define (phm-remove-non-existent-test remove transform)
  (define (terrible-hash string) 0)
  (let ((phm (remove (transform (make-phm string-hash string=?))
		     "not-present")))
    (assert (zero? (phm/count phm)))
    (assert (not (phm/contains? phm "not-present")))
    (assert (not (phm/get phm "not-present" #f))))
  (let ((phm (remove (transform (phm/put (make-phm terrible-hash string=?)
					 "foo"
					 1))
		     "not-present")))
    (assert (= 1 (phm/count phm)))
    (assert (phm/contains? phm "foo"))
    (assert (not (phm/contains? phm "not-present")))))

(define-test (persistent-hash-map remove-non-existent pure)
  (phm-remove-non-existent-test phm/remove (lambda (m) m)))

(define-test (persistent-hash-map remove-non-existent mutate)
  (phm-remove-non-existent-test phm/remove! phm/mutable))

(define-test (persistent-hash-map phm/add-alist)
  (let* ((alist '(("foo" . 1) ("bar" . 2) ("baz" . 3)))
	 (phm (phm/add-alist (make-phm string-hash string=?) alist)))
    (assert-phm= phm alist)))

(define-test (persistent-hash-map phm/add-alist!)
  (let* ((alist '(("foo" . 1) ("bar" . 2) ("baz" . 3)))
	 (phm (phm/mutable (make-phm string-hash string=?))))
    (phm/add-alist! phm alist)
    (assert-phm= phm alist)))

(define (phm-collision-test put remove transform)
  (define (sort-alist alist)
    (sort alist (lambda (a1 a2) (string<? (car a1) (car a2)))))
  (define (terrible-hash string)
    (cond ((string=? string "foo") 0)
	  ((string=? string "bar") 1)
	  (else 2)))
  (let* ((alist '(("foo" . 1) ("bar" . 2) ("baz" . 3) ("bat" . 4) ("quux" . 5)))
	 (phm-1 (fold-left
		 (lambda (phm a) (put phm (car a) (cdr a)))
		 (transform (make-phm terrible-hash string=?))
		 alist))
	 (phm (put phm-1 "baz" 3)))
    (assert-phm= phm alist)
    (let ((phm-alist (phm->alist phm)))
      (assert (equal? (sort-alist alist)
		      (sort-alist phm-alist))))
    (let ((alist-minus-baz (del-assoc "baz" alist))
	  (phm-minus-baz (remove (transform phm) "baz")))
      (assert-phm= phm-minus-baz alist-minus-baz)
      (let ((phm-minus-nonexistent (remove phm-minus-baz "not-present")))
	(assert (= (phm/count phm-minus-nonexistent) (-1+ (length alist))))
	(let ((alist-minus-bat (del-assoc "bat" alist-minus-baz))
	      (phm-minus-bat (remove phm-minus-nonexistent "bat")))
	  (assert-phm= phm-minus-bat alist-minus-bat))))))

(define-test (persistent-hash-map collisions pure)
  (phm-collision-test phm/put phm/remove (lambda (m) m)))

(define-test (persistent-hash-map collisions mutate)
  (phm-collision-test phm/put! phm/remove! phm/mutable))

(define-test (persistent-hash-map big-hash)
  "Test that hashes that differ only above `hamt-hash-size' still work."
  (define big-hash
    (let* ((big-1 (expt 2 hamt-hash-size))
	   (big-2 (* 2 big-1)))
      (lambda (string)
	(cond ((string=? string "foo") big-1)
	      (else big-2)))))
  (let* ((alist '(("foo" . 1) ("bar" . 2) ("baz" . 3) ("bat" . 4) ("quux" . 5)))
	 (phm (phm/add-alist (make-phm big-hash string=?) alist)))
    (assert-phm= phm alist)))

(define-test (persistent-hash-map same-first-fragment)
  (define (same-first-fragment string)
    (* hamt-bucket-size (string-hash string)))
  (let* ((alist '(("foo" . 1) ("bar" . 2) ("baz" . 3) ("bat" . 4) ("quux" . 5)))
	 (phm (phm/add-alist (make-phm same-first-fragment string=?) alist)))
    (assert-phm= phm alist)
    (let ((phm-minus-baz (phm/remove phm "baz")))
      (assert-phm= phm-minus-baz (del-assoc "baz" alist)))
    (let ((phm-minus-nonexistent (phm/remove phm "not-present")))
      (assert (= (phm/count phm-minus-nonexistent) (length alist))))))

(define-test (persistent-hash-map pure-mutate-interference)
  "Test that mutating and pure operations interact with each other
correctly."
  (define (alist-replace alist key datum)
    (cons (cons key datum) (del-assoc key alist)))
  (let* ((m0 (make-phm string-hash string=?))
	 (a1 '(("foo" . 1) ("bar" . 2) ("baz" . 3)))
	 (m1 (phm/add-alist m0 a1))
	 (a4 (alist-replace a1 "foo" 4))
	 (m2 (phm/put m1 "foo" 4))
	 (a5 (alist-replace a1 "foo" 5))
	 (m3 (phm/mutable m2))
	 (m4 (phm/put! m3 "foo" 5))
	 (a6 (alist-replace a1 "foo" 6))
	 (m5 (phm/immutable m4))
	 (m6 (phm/mutable m5))
	 (m7 (phm/put! m6 "foo" 6))
	 (a7 (alist-replace a1 "foo" 7))
	 (a8 (alist-replace a1 "foo" 8))
	 (m8 (phm/put! m6 "foo" 7)))
    (phm/put! m4 "foo" 8)
    (assert-phm= m0 '())
    (assert-phm= m1 a1)
    (assert-phm= m2 a4)
    (assert-phm= m3 a8)
    (assert-phm= m4 a8)
    (assert-phm= m5 a5)
    (assert-phm= m6 a7)
    (assert-phm= m7 a7)
    (assert-phm= m8 a7)
    (let ((a (del-assoc "foo" a1))
	  (m9 (phm/remove! m4 "foo")))
      (assert-phm= m4 a)
      (assert-phm= m9 a))))

(define-test (persistent-hash-map phm/data)
  (let* ((alist '(("a" . 1) ("b" . 2) ("c" . 3)))
	 (data (phm/data (make-phm string-hash string=? alist))))
    (assert (equal? (map cdr alist)
		    (sort data <)))))

(define-test (persistent-hash-map phm/keys)
  (let* ((alist '(("a" . 1) ("b" . 2) ("c" . 3)))
	 (keys (phm/keys (make-phm string-hash string=? alist))))
    (assert (equal? (map car alist)
		    (sort keys string<?)))))

(define-test (persistent-hash-map phm/for-each)
  (define (sort-alist alist)
    (sort alist (lambda (a1 a2) (string<? (car a1) (car a2)))))
  (let* ((alist '(("a" . 1) ("b" . 2) ("c" . 3)))
	 (phm (make-phm string-hash string=? alist))
	 (accumulator '()))
    (phm/for-each (lambda (k d) (set! accumulator
				      (cons (cons k d) accumulator)))
		  phm)
    (assert (equal? alist (sort-alist accumulator)))))

(define (persistent-hash-map replace transform)
  (define (sort-alist alist)
    (sort alist (lambda (a1 a2) (string<? (car a1) (car a2)))))
  (let* ((alist-1 '(("a" . 1) ("b" . 2) ("c" . 3)))
	 (alist-2 '(("a" . 1) ("b" . 4) ("c" . 3)))
	 (alist-3 '(("a" . 1) ("b" . 4)))
	 (phm (replace (transform (make-phm string-hash string=? alist-1))
		       "b"
		       (lambda (x) 4))))
    (assert (equal? alist-2 (sort-alist (phm->alist phm))))
    (assert (equal? alist-3
		    (sort-alist
		     (phm->alist
		      (replace phm "c" (lambda (x) hamt-null))))))))

(define-test (persistent-hash-map phm/replace)
  (persistent-hash-map phm/replace (lambda (m) m)))

(define-test (persistent-hash-map phm/replace!)
  (persistent-hash-map phm/replace! phm/mutable))

(define-test (persistent-hash-map immutable-replace)
  (define (sort-alist alist)
    (sort alist (lambda (a1 a2) (string<? (car a1) (car a2)))))
  (let* ((alist-1 '(("a" . 1) ("b" . 2) ("c" . 3)))
	 (alist-2 '(("a" . 1) ("b" . 5) ("c" . 3)))
	 (phm-1 (phm/mutable (make-phm string-hash string=? alist-1))))
    (phm/put! phm-1 "b" 4)
    (let ((phm-2 (phm/immutable phm-1
				(lambda (k d) (if (string=? k "b") (1+ d) d)))))
      (assert (equal? alist-2 (sort-alist (phm->alist phm-2)))))))

(define-test (persistent-hash-map phm/mutable?)
  (let ((phm (make-phm string-hash string=?)))
    (assert (not (phm/mutable? phm)))
    (assert (phm/mutable? (phm/mutable phm)))
    (assert (not (phm/mutable? (phm/immutable (phm/mutable phm)))))))

(define-test (persistent-hash-map modify-collision add-different-hash)
  (define (terrible-hash string)
    (cond ((string=? string "foo") 0)
	  ((string=? string "bar") 0)
	  (else hamt-bucket-size)))	; same as 0 in bottom 5 bits
  (let* ((alist '(("foo" . 1) ("bar" . 2)))
	 (phm-1 (make-phm terrible-hash string=? alist))
	 (phm-2 (phm/put phm-1 "baz" 3)))
    (assert-phm= phm-2 '(("foo" . 1) ("bar" . 2) ("baz" . 3)))))

(define-test (persistent-hash-map lower-collision)
  (define same-bottom-three-fragments (expt hamt-bucket-size 3))
  (define (terrible-hash string)
    (if (or (string=? string "foo")
	    (string=? string "bar"))
	same-bottom-three-fragments
	(* 2 same-bottom-three-fragments)))
  (let* ((alist '(("foo" . 1) ("bar" . 2)))
	 (phm-1 (make-phm terrible-hash string=? alist))
	 (phm-2 (phm/put phm-1 "baz" 3))
	 (phm-3 (phm/remove phm-2 "foo"))
	 (phm-4 (phm/remove phm-3 "bar"))
	 (phm-5 (phm/remove phm-4 "baz")))
    (assert-phm= phm-2 '(("foo" . 1) ("bar" . 2) ("baz" . 3)))
    (assert-phm= phm-3 '(("bar" . 2) ("baz" . 3)))
    (assert-phm= phm-4 '(("baz" . 3)))
    (assert-phm= phm-5 '())
    (assert (= 5 (hamt-max-depth phm-2)))
    (assert (= 4 (hamt-max-depth phm-3)))
    (assert (= 1 (hamt-max-depth phm-4)))
    (assert (= 1 (hamt-max-depth phm-5)))))