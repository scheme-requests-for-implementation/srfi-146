;;;; Persistent Hash Map

;;; Copyright MMXV-MMXVII Arthur A. Gleckler.  All rights reserved.

(declare (usual-integrations))

;;; Naming conventions:

;;    =: procedure that compares keys
;;    c: bit string representing the non-leaf children present
;;       immediately below a sparse node
;;    d: datum, or `hamt-null' to represent absence or deletion
;;   dp: procedure that takes an existing datum and returns the datum
;;       that should replace it.  Either may be `hamt-null'.  When
;;       there is no payload, `hamt-null' is passed.
;;    h: hash
;;   hp: procedure that computes hash
;;    k: key that maps to a particular datum
;;    l: bit string representing the leaves present below a sparse node
;;    n: node (of type `collision', `narrow', or `wide')

;;; Background

;; See these papers:

;; - Ideal Hash Trees, Phil Bagwell, 2000,
;;   <https://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf>

;; - Optimizing Hash-Array Mapped Tries for Fast and Lean Immutable
;;   JVM Collections, Steinforder & Vinju, 2015,
;;   <http://michael.steindorfer.name/publications/oopsla15.pdf>

;; Also, see Clojure's persistent hash maps, which support both
;; mutable ("transient") and persistent modes.

;;; Design

;; According to Phil Bagwell's paper, "Occasionally an entire 32 bit
;; hash may be consumed and a new one must be computed to
;; differentiate the two keys."  Later, he says "The hash function was
;; tailored to give a 32 bit hash.  The algorithm requires that the
;; hash can be extended to an arbitrary number of bits.  This was
;; accomplished by rehashing the key combined with an integer
;; representing the trie level, zero being the root.  Hence if two
;; keys do give the same initial hash then the rehash has a
;; probability of 1 in 2^32 of a further collision."  However, I
;; implement collision lists instead because they will be rarely used
;; when hash functions are good, but work well when they're not, as in
;; the case of MIT Scheme's `string-hash'.

;; <> Replace bit-string operations with integer operations if you
;; can't get your compiler to do that automatically.

(define hamt-hash-slice-size 5)
(define hamt-hash-size
  (let ((word-size 64))
    (- word-size
       (remainder word-size hamt-hash-slice-size))))
(define hamt-hash-modulus (expt 2 hamt-hash-size))
(define hamt-bucket-size (expt 2 hamt-hash-slice-size))
(define hamt-null (cons 'hamt 'null))

(define-record-type hash-array-mapped-trie
    (%make-hamt = count hash mutable? payload? root)
    hash-array-mapped-trie?
  (=        hamt/=)
  (count    hamt/count set-hamt/count!)
  (hash     hamt/hash)
  (mutable? hamt/mutable?)
  (payload? hamt/payload?)
  (root     hamt/root  set-hamt/root!))

(define (make-hamt = hash payload?)
  (%make-hamt = 0 hash #f payload? (make-empty-narrow)))

(define-record-type collision
    (make-collision entries hash)
    collision?
  (entries collision/entries)
  (hash  collision/hash))

;; <> Once SRFI 136 is implemented, make `narrow' and `wide' share
;; fields since they differ only in type.
(define-record-type narrow
    (make-narrow array children leaves)
    narrow?
  (array    narrow/array)
  (children narrow/children)
  (leaves   narrow/leaves))

(define-record-type wide
    (make-wide array children leaves)
    wide?
  (array    wide/array)
  (children wide/children)
  (leaves   wide/leaves))

(define (hamt/empty? hamt)
  (zero? (hamt/count hamt)))

(define (hamt/immutable hamt #!optional replace)
  "Return a HAMT equivalent to `hamt', but that is immutable.  Even if
`hamt' is mutable, no change to it will affect the returned HAMT.  If
`replace' is provided and `hamt' has payloads, replace each datum in a
wide node with what `replace' returns when passed the key and
corresponding datum.  This is useful for converting HAMT sets stored
as values in a HAMT map back to immutable ones when the containing map
is made immutable.  (Only data in wide nodes will have been modified
since the change to mutable happened.)"
  (if (hamt/mutable? hamt)
      (let ((payload? (hamt/payload? hamt)))
	(%make-hamt (hamt/= hamt)
		    (hamt/count hamt)
		    (hamt/hash hamt)
		    #f
		    payload?
		    (->immutable (hamt/root hamt)
				 payload?
				 (if (default-object? replace)
				     (lambda (k d) d)
				     replace))))
      hamt))

(define (hamt/mutable hamt)
  (if (hamt/mutable? hamt)
      hamt
      (%make-hamt (hamt/= hamt)
		  (hamt/count hamt)
		  (hamt/hash hamt)
		  #t
		  (hamt/payload? hamt)
		  (hamt/root hamt))))

(define (hamt/replace hamt key dp)
  (assert (not (hamt/mutable? hamt)))
  (let*-values (((payload?) (hamt/payload? hamt))
		((root) (hamt/root hamt))
		((==) (hamt/= hamt))
		((hp) (hamt/hash hamt))
		((hash) (hash-bits hp key))
		((change node) (modify-pure hamt root 0 dp hash key)))
    (if (eq? node root)
	hamt
	(let ((count (+ (hamt/count hamt) change)))
	  (%make-hamt == count hp #f payload? node)))))

(define (hamt/put hamt key datum)
  (hamt/replace hamt key (lambda (x) datum)))

(define (hamt/replace! hamt key dp)
  (assert (hamt/mutable? hamt))
  (let*-values (((root) (hamt/root hamt))
		((hp) (hamt/hash hamt))
		((hash) (hash-bits hp key))
		((change node) (mutate hamt root 0 dp hash key)))
    (unless (zero? change)
      (set-hamt/count! hamt (+ (hamt/count hamt) change)))
    (unless (eq? node root)
      (set-hamt/root! hamt node))
    hamt))

(define (hamt/put! hamt key datum)
  (hamt/replace! hamt key (lambda (x) datum)))

(define (make-empty-narrow)
  (make-narrow (vector)
	       (make-bit-string hamt-bucket-size #f)
	       (make-bit-string hamt-bucket-size #f)))

(define (hamt-null? n)
  (eq? n hamt-null))

(define (collision-single-leaf? n)
  (let ((elements (collision/entries n)))
    (and (not (null? elements))
	 (null? (cdr elements)))))

(define (narrow-single-leaf? n)
  (and (bit-string-zero? (narrow/children n))
       (= 1 (population-count (narrow/leaves n)))))

(define (wide-single-leaf? n)
  (and (bit-string-zero? (wide/children n))
       (= 1 (population-count (wide/leaves n)))))

(define (hash-bits hp key)
  (unsigned-integer->bit-string hamt-hash-size
				(remainder (hp key) hamt-hash-modulus)))

(define (bit-string-alter bit-string index boolean)
  (let ((result (bit-string-copy bit-string)))
    (if boolean
	(bit-string-set! result index)
	(bit-string-clear! result index))
    result))

(define (narrow->wide n payload?)
  (let* ((c (bit-string-copy (narrow/children n)))
	 (l (bit-string-copy (narrow/leaves n)))
	 (stride (leaf-stride payload?))
	 (a-in (narrow/array n))
	 (a-out (make-vector (* stride hamt-bucket-size))))
    (let next-leaf ((start 0) (count 0))
      (let ((i (bit-substring-find-next-set-bit l start hamt-bucket-size)))
	(when i
	  (let ((j (* stride i)))
	    (vector-set! a-out j (vector-ref a-in count))
	    (when payload?
	      (vector-set! a-out (1+ j) (vector-ref a-in (1+ count)))))
	  (next-leaf (1+ i) (+ stride count)))))
    (let next-child ((start 0) (offset (* stride (population-count l))))
      (let ((i (bit-substring-find-next-set-bit c start hamt-bucket-size)))
	(when i
	  (vector-set! a-out (* stride i) (vector-ref a-in offset))
	  (next-child (1+ i) (1+ offset)))))
    (make-wide a-out c l)))

(define (->immutable n payload? replace)
  "Convert `n' and its descendants into `collision' or `narrow' nodes.
Stop at the first `collision' node or `narrow' node on each path.  If
`payload?' is true, then expect data, not just keys, and replace each
datum in a wide node with what `replace' returns when passed the key
and corresponding datum."
  (cond ((collision? n) n)
	((narrow? n) n)
	((wide? n)
	 (let* ((c (bit-string-copy (wide/children n)))
		(l (bit-string-copy (wide/leaves n)))
		(stride (leaf-stride payload?))
		(l-count (population-count l))
		(a-in (wide/array n))
		(a-out (make-vector
			(+ (* stride l-count) (population-count c)))))
	   (let next-leaf ((start 0) (count 0))
	     (let ((i (bit-substring-find-next-set-bit l
						       start
						       hamt-bucket-size)))
	       (when i
		 (let* ((j (* stride i))
			(key (vector-ref a-in j)))
		   (vector-set! a-out count key)
		   (when payload?
		     (vector-set! a-out
				  (1+ count)
				  (replace
				   key
				   (vector-ref a-in (1+ j))))))
		 (next-leaf (1+ i) (+ stride count)))))
	   (let next-child ((start 0) (offset (* stride l-count)))
	     (let ((i (bit-substring-find-next-set-bit c
						       start
						       hamt-bucket-size)))
	       (when i
		 (vector-set! a-out
			      offset
			      (->immutable (vector-ref a-in (* stride i))
					   payload?
					   replace))
		 (next-child (1+ i) (1+ offset)))))
	   (make-narrow a-out c l)))
	(else (error "Unexpected type of node."))))

(define (hash-fragment shift hash)
  (bit-string->unsigned-integer
   (bit-substring hash
		  shift
		  (+ shift hamt-hash-slice-size))))

(define (fragment->mask fragment)
  (unsigned-integer->bit-string hamt-bucket-size (-1+ (expt 2 fragment))))

(define-test-group (hash-array-mapped-trie fragment->mask)
  (lambda (input expected)
    (assert (bit-string=? expected (fragment->mask input))))
  '(0 #*00000000000000000000000000000000)
  '(1 #*00000000000000000000000000000001)
  '(2 #*00000000000000000000000000000011)
  '(3 #*00000000000000000000000000000111))

(define (bit-string-zero? bit-string)
  (not (bit-substring-find-next-set-bit
	bit-string
	0
	(bit-string-length bit-string))))

(define (bit-string-one? bit-string)
  (and (bit-string-ref bit-string 0)
       (not (bit-substring-find-next-set-bit
	     bit-string
	     1
	     (bit-string-length bit-string)))))

;; <> Rewrite this to use a population count instruction.
(define (population-count bit-string)
  (let ((size (bit-string-length bit-string)))
    (let next-bit-set ((count 0)
		       (position 0))
      (if (= position size)
	  count
	  (let ((found (bit-substring-find-next-set-bit bit-string
							position
							size)))
	    (if found
		(next-bit-set (1+ count) (1+ found))
		count))))))

(define-test-group (population-count)
  (lambda (expected bit-string)
    (assert (= expected (population-count bit-string))))
  '(3 #*10101)
  '(5 #*11111)
  '(0 #*00000))

(define (mutate hamt n shift dp h k)
  (cond ((collision? n) (modify-collision hamt n shift dp h k))
	((narrow? n)
	 (modify-wide hamt
		      (narrow->wide n (hamt/payload? hamt))
		      shift
		      dp
		      h
		      k))
	((wide? n) (modify-wide hamt n shift dp h k))
	(else (error "Unknown HAMT node type." n))))

(define (modify-wide hamt n shift dp h k)
  (let ((fragment (hash-fragment shift h)))
    (cond ((bit-string-ref (wide/children n) fragment)
	   (modify-wide-child hamt n shift dp h k))
	  ((bit-string-ref (wide/leaves n) fragment)
	   (modify-wide-leaf hamt n shift dp h k))
	  (else
	   (let ((d (dp hamt-null)))
	     (if (hamt-null? d)
		 (values 0 n)
		 (modify-wide-new hamt n shift d h k)))))))

(define (modify-wide-child hamt n shift dp h k)
  (let*-values (((fragment) (hash-fragment shift h))
		((c) (wide/children n))
		((l) (wide/leaves n))
		((array) (wide/array n))
		((payload?) (hamt/payload? hamt))
		((stride) (leaf-stride payload?))
		((i) (* stride fragment))
		((child) (vector-ref array i))
		((change new-child)
		 (mutate hamt
			 child
			 (+ shift hamt-hash-slice-size)
			 dp
			 h
			 k)))
    (define (coalesce key datum)
      (vector-set! array i key)
      (when payload?
	(vector-set! array (1+ i) datum))
      (bit-string-clear! c fragment)
      (bit-string-set! l fragment)
      (values change n))
    (define (replace)
      (vector-set! array i new-child)
      (values change n))
    (cond ((eq? new-child child) (values change n))
	  ((hamt-null? new-child)
	   (error "Child cannot become null." n))
	  ((collision? new-child)
	   (if (collision-single-leaf? new-child)
	       (let ((a (car (collision/entries new-child))))
		 (if payload?
		     (coalesce (car a) (cdr a))
		     (coalesce a #f)))
	       (replace)))
	  ((wide? new-child)
	   (if (wide-single-leaf? new-child)
	       (let ((a (wide/array new-child))
		     (j (* stride (bit-substring-find-next-set-bit
				   (wide/leaves new-child)
				   0
				   hamt-bucket-size))))
		 (coalesce (vector-ref a j)
			   (and payload? (vector-ref a (1+ j)))))
	       (replace)))
	  ((narrow? new-child)
	   (replace))
	  (else (error "Unexpected type of child node.")))))

(define (modify-wide-leaf hamt n shift dp h k)
  (let* ((fragment (hash-fragment shift h))
	 (l (wide/leaves n))
	 (array (wide/array n))
	 (payload? (hamt/payload? hamt))
	 (stride (leaf-stride payload?))
	 (i (* stride fragment))
	 (key (vector-ref array i)))
    (if ((hamt/= hamt) k key)
	(let* ((existing (if payload? (vector-ref array (1+ i)) hamt-null))
	       (d (dp existing)))
	  (cond ((hamt-null? d)
		 (vector-set! array i #f)
		 (when payload? (vector-set! array (1+ i) #f))
		 (bit-string-clear! l fragment)
		 (values -1 n))
		(else
		 (when payload? (vector-set! array (1+ i) d))
		 (values 0 n))))
	(let ((d (dp hamt-null)))
	  (if (hamt-null? d)
	      (values 0 n)
	      (add-wide-leaf-key hamt n shift d h k))))))

(define (add-wide-leaf-key hamt n shift d h k)
  (define payload? (hamt/payload? hamt))
  (define make-entry
    (if payload? cons (lambda (k d) k)))
  (let* ((fragment (hash-fragment shift h))
	 (c (wide/children n))
	 (l (wide/leaves n))
	 (array (wide/array n))
	 (stride (leaf-stride payload?))
	 (i (* stride fragment))
	 (key (vector-ref array i))
	 (hash (hash-bits (hamt/hash hamt) key))
	 (datum (and payload? (vector-ref array (1+ i)))))
    (vector-set! array
		 i
		 (if (bit-string=? h hash)
		     (make-collision (list (make-entry k d)
					   (make-entry key datum))
				     h)
		     (make-narrow-with-two-keys
		      payload?
		      (+ shift hamt-hash-slice-size)
		      h
		      k
		      d
		      hash
		      key
		      datum)))
    (when payload?
      (vector-set! array (1+ i) #f))
    (bit-string-set! c fragment)
    (bit-string-clear! l fragment)
    (values 1 n)))

(define (modify-wide-new hamt n shift d h k)
  (let* ((fragment (hash-fragment shift h))
	 (l (wide/leaves n))
	 (array (wide/array n))
	 (payload? (hamt/payload? hamt))
	 (stride (leaf-stride payload?))
	 (i (* stride fragment)))
    (vector-set! array i k)
    (when payload?
      (vector-set! array (1+ i) d))
    (bit-string-set! l fragment)
    (values 1 n)))

(define (make-narrow-with-two-keys payload? shift h1 k1 d1 h2 k2 d2)
  (define (two-leaves f1 k1 d1 f2 k2 d2)
    (make-narrow
     (if payload?
	 (vector k1 d1 k2 d2)
	 (vector k1 k2))
     (make-bit-string hamt-bucket-size #f)
     (let ((leaves (make-bit-string hamt-bucket-size #f)))
       (bit-string-set! leaves f1)
       (bit-string-set! leaves f2)
       leaves)))
  (assert (not (bit-string=? h1 h2)))
  (let ((f1 (hash-fragment shift h1))
	(f2 (hash-fragment shift h2)))
    (cond ((= f1 f2)
	   (make-narrow
	    (vector (make-narrow-with-two-keys payload?
					       (+ shift hamt-hash-slice-size)
					       h1
					       k1
					       d1
					       h2
					       k2
					       d2))
	    (let ((children (make-bit-string hamt-bucket-size #f)))
	      (bit-string-set! children f1)
	      children)
	    (make-bit-string hamt-bucket-size #f)))
	  ((< f1 f2)
	   (two-leaves f1 k1 d1 f2 k2 d2))
	  (else
	   (two-leaves f2 k2 d2 f1 k1 d1)))))

(define (modify-pure hamt n shift dp h k)
  (cond ((collision? n) (modify-collision hamt n shift dp h k))
	((narrow? n) (modify-narrow hamt n shift dp h k))
	((wide? n) (error "Should have been converted to narrow before here."))
	(else (error "Unknown HAMT node type." n))))

(define (lower-collision hamt n shift dp h k)
  "If we try to add a key to a collision but it has a different hash
than the collision's elements, add it to a narrow above the collision
instead.  Add as many levels of child-only narrows as needed to reach
the point where the hash fragments differ.  This is guaranteed to
happen at some level because we're only called when the full hashes
differ."
  (let ((collision-hash (collision/hash n))
	(d (dp hamt-null)))
    (if (hamt-null? d)
	(values 0 n)
	(values
	 1
	 (let descend ((shift shift))
	   (let ((collision-fragment (hash-fragment shift collision-hash))
		 (leaf-fragment (hash-fragment shift h)))
	     (if (= collision-fragment leaf-fragment)
		 (let ((child (descend (+ shift hamt-hash-slice-size))))
		   (make-narrow
		    (vector child)
		    (let ((children (make-bit-string hamt-bucket-size #f)))
		      (bit-string-set! children collision-fragment)
		      children)
		    (make-bit-string hamt-bucket-size #f)))
		 (make-narrow
		  (if (hamt/payload? hamt)
		      (vector k d n)
		      (vector k n))
		  (let ((children (make-bit-string hamt-bucket-size #f)))
		    (bit-string-set! children collision-fragment)
		    children)
		  (let ((leaves (make-bit-string hamt-bucket-size #f)))
		    (bit-string-set! leaves leaf-fragment)
		    leaves)))))))))

(define (modify-collision hamt n shift dp h k)
  (if (bit-string=? h (collision/hash n))
      (let ((payload? (hamt/payload? hamt)))
	(let next ((entries (collision/entries n))
		   (checked '()))
	  (if (null? entries)
	      (let ((d (dp hamt-null)))
		(if (hamt-null? d)
		    (values 0 n)
		    (values 1
			    (make-collision (if payload?
						(cons (cons k d) checked)
						(cons k checked))
					    h))))
	      (let* ((entry (car entries))
		     (key (if payload? (car entry) entry)))
		(if ((hamt/= hamt) k key)
		    (let* ((existing (if payload? (cdr entry) hamt-null))
			   (d (dp existing))
			   (delete? (hamt-null? d))
			   (others (append checked (cdr entries))))
		      (values
		       (if delete? -1 0)
		       (make-collision (cond (delete? others)
					     (payload? (cons (cons k d) others))
					     (else (cons k others)))
				       h)))
		    (next (cdr entries)
			  (cons (car entries) checked)))))))
      (lower-collision hamt n shift dp h k)))

;; If we're storing "payloads," i.e. a datum to go with each key, we
;; must reserve two spots for each key in each vector.  Otherwise, we
;; need only one.
(define (leaf-stride payload?)
  (if payload? 2 1))

(define (narrow-child-index l c mask payload?)
  (+ (* (leaf-stride payload?) (population-count l))
     (population-count (bit-string-and c mask))))

(define (narrow-leaf-index l mask payload?)
  (* (leaf-stride payload?) (population-count (bit-string-and l mask))))

(define (modify-narrow hamt n shift dp h k)
  (let ((fragment (hash-fragment shift h)))
    (cond ((bit-string-ref (narrow/children n) fragment)
	   (modify-narrow-child hamt n shift dp h k))
	  ((bit-string-ref (narrow/leaves n) fragment)
	   (modify-narrow-leaf hamt n shift dp h k))
	  (else
	   (let ((d (dp hamt-null)))
	     (if (hamt-null? d)
		 (values 0 n)
		 (modify-narrow-new hamt n shift d h k)))))))

(define (modify-narrow-child hamt n shift dp h k)
  (let*-values (((fragment) (hash-fragment shift h))
		((mask) (fragment->mask fragment))
		((c) (narrow/children n))
		((l) (narrow/leaves n))
		((array) (narrow/array n))
		((payload?) (hamt/payload? hamt))
		((child-index)
		 (narrow-child-index l c mask payload?))
		((child) (vector-ref array child-index))
		((change new-child)
		 (modify-pure hamt
			      child
			      (+ shift hamt-hash-slice-size)
			      dp
			      h
			      k)))
    (define (coalesce key datum)
      (let ((leaf-index (narrow-leaf-index l mask payload?)))
	(values change
		(make-narrow (if payload?
				 (vector-edit array
					      (add leaf-index key)
					      (add leaf-index datum)
					      (drop child-index 1))
				 (vector-edit array
					      (add leaf-index key)
					      (drop child-index 1)))
			     (bit-string-alter c fragment #f)
			     (bit-string-alter l fragment #t)))))
    (define (replace)
      (values change
	      (make-narrow (vector-replace-one array child-index new-child)
			   c
			   l)))
    (cond ((eq? new-child child) (values 0 n))
	  ((hamt-null? new-child)
	   (error "Child cannot become null." n))
	  ((collision? new-child)
	   (if (collision-single-leaf? new-child)
	       (let ((a (car (collision/entries new-child))))
		 (if payload?
		     (coalesce (car a) (cdr a))
		     (coalesce a #f)))
	       (replace)))
	  ((narrow? new-child)
	   (if (narrow-single-leaf? new-child)
	       (let ((a (narrow/array new-child)))
		 (coalesce (vector-ref a 0)
			   (and payload? (vector-ref a 1))))
	       (replace)))
	  ((wide? new-child)
	   (error "New child should be collision or narrow."))
	  (else (error "Unexpected type of child node.")))))

(define (modify-narrow-leaf hamt n shift dp h k)
  (let* ((fragment (hash-fragment shift h))
	 (mask (fragment->mask fragment))
	 (c (narrow/children n))
	 (l (narrow/leaves n))
	 (array (narrow/array n))
	 (payload? (hamt/payload? hamt))
	 (stride (leaf-stride payload?))
	 (leaf-index (narrow-leaf-index l mask payload?))
	 (key (vector-ref array leaf-index)))
    (if ((hamt/= hamt) k key)
	(let* ((existing (if payload?
			     (vector-ref array (1+ leaf-index))
			     hamt-null))
	       (d (dp existing)))
	  (cond ((hamt-null? d)
		 (values -1
			 (make-narrow (vector-without array
						      leaf-index
						      (+ leaf-index stride))
				      c
				      (bit-string-alter l fragment #f))))
		(payload?
		 (values
		  0
		  (make-narrow (vector-replace-one array (1+ leaf-index) d)
			       c
			       l)))
		(else (values 0 n))))
	(let ((d (dp hamt-null)))
	  (if (hamt-null? d)
	      (values 0 n)
	      (add-narrow-leaf-key hamt n shift d h k))))))

(define (add-narrow-leaf-key hamt n shift d h k)
  (define payload? (hamt/payload? hamt))
  (define make-entry
    (if payload? cons (lambda (k d) k)))
  (let* ((fragment (hash-fragment shift h))
	 (mask (fragment->mask fragment))
	 (c (narrow/children n))
	 (l (narrow/leaves n))
	 (array (narrow/array n))
	 (payload? (hamt/payload? hamt))
	 (stride (leaf-stride payload?))
	 (leaf-index (narrow-leaf-index l mask payload?))
	 (key (vector-ref array leaf-index))
	 (child-index (narrow-child-index l c mask payload?))
	 (hash (hash-bits (hamt/hash hamt) key))
	 (datum (and payload? (vector-ref array (1+ leaf-index)))))
    (values 1
	    (make-narrow (if (bit-string=? h hash)
			     (vector-edit
			      array
			      (drop leaf-index stride)
			      (add child-index
				   (make-collision (list (make-entry k d)
							 (make-entry key datum))
						   h)))
			     (vector-edit
			      array
			      (drop leaf-index stride)
			      (add child-index
				   (make-narrow-with-two-keys
				    payload?
				    (+ shift hamt-hash-slice-size)
				    h
				    k
				    d
				    hash
				    key
				    datum))))
			 (bit-string-alter c fragment #t)
			 (bit-string-alter l fragment #f)))))

(define (modify-narrow-new hamt n shift d h k)
  (let* ((fragment (hash-fragment shift h))
	 (mask (fragment->mask fragment))
	 (c (narrow/children n))
	 (l (narrow/leaves n))
	 (array (narrow/array n))
	 (payload? (hamt/payload? hamt))
	 (leaf-index (narrow-leaf-index l mask payload?))
	 (delete? (hamt-null? d)))
    (values 1
	    (make-narrow (if payload?
			     (vector-edit array
					  (add leaf-index k)
					  (add leaf-index d))
			     (vector-edit array
					  (add leaf-index k)))
			 c
			 (bit-string-alter l fragment #t)))))

(define (hamt-fetch hamt key)
  "Fetch datum from `hamt' at `key'.  Return `hamt-null' if the key is
not present.  If `hamt' stores no payloads, return the symbol
`present' if the key is present."
  (let ((h (hash-bits (hamt/hash hamt) key))
	(payload? (hamt/payload? hamt)))
    (let descend ((n (hamt/root hamt))
		  (shift 0))
      (cond ((collision? n)
	     (let ((entries (collision/entries n))
		   (key= (hamt/= hamt)))
	       (if payload?
		   (let ((assoc= (association-procedure key= car)))
		     (cond ((assoc= key entries) => cdr)
			   (else hamt-null)))
		   (if (find-tail (lambda (e) (key= key e)) entries)
		       'present
		       hamt-null))))
	    ((narrow? n)
	     (let ((array (narrow/array n))
		   (c (narrow/children n))
		   (l (narrow/leaves n))
		   (fragment (hash-fragment shift h)))
	       (cond ((bit-string-ref c fragment)
		      (let* ((mask (fragment->mask fragment))
			     (child-index (narrow-child-index
					   l
					   c
					   mask
					   (hamt/payload? hamt))))
			(descend (vector-ref array child-index)
				 (+ shift hamt-hash-slice-size))))
		     ((bit-string-ref l fragment)
		      (let* ((mask (fragment->mask fragment))
			     (leaf-index
			      (narrow-leaf-index l mask (hamt/payload? hamt)))
			     (k (vector-ref array leaf-index)))
			(if ((hamt/= hamt) k key)
			    (if payload?
				(vector-ref array (1+ leaf-index))
				'present)
			    hamt-null)))
		     (else hamt-null))))
	    ((wide? n)
	     (let ((array (wide/array n))
		   (stride (leaf-stride (hamt/payload? hamt)))
		   (c (wide/children n))
		   (l (wide/leaves n))
		   (i (hash-fragment shift h)))
	       (cond ((bit-string-ref c i)
		      (descend (vector-ref array (* stride i))
			       (+ shift hamt-hash-slice-size)))
		     ((bit-string-ref l i)
		      (let* ((j (* stride i))
			     (k (vector-ref array j)))
			(if ((hamt/= hamt) k key)
			    (if payload?
				(vector-ref array (1+ j))
				'present)
			    hamt-null)))
		     (else hamt-null))))
	    (else (error "Unexpected type of child node."))))))

(define (collision/for-each procedure node payload?)
  (if payload?
      (do-list (e (collision/entries node))
	(procedure (car e) (cdr e)))
      (do-list (e (collision/entries node))
	(procedure e #f))))

(define (narrow/for-each procedure node payload?)
  (let ((array (narrow/array node))
	(stride (leaf-stride payload?))
	(c (narrow/children node))
	(l (narrow/leaves node)))
    (let next-leaf ((count 0)
		    (start 0))
      (let ((i (bit-substring-find-next-set-bit l start hamt-bucket-size)))
	(if i
	    (let* ((j (* stride count))
		   (k (vector-ref array j))
		   (d (and payload? (vector-ref array (1+ j)))))
	      (procedure k d)
	      (next-leaf (1+ count) (1+ i)))
	    (let next-child ((start 0)
			     (offset (* stride count)))
	      (let ((i (bit-substring-find-next-set-bit c
							start
							hamt-bucket-size)))
		(when i
		  (let ((child (vector-ref array offset)))
		    (hamt-node/for-each child payload? procedure)
		    (next-child (1+ i) (1+ offset)))))))))))

(define (wide/for-each procedure node payload?)
  (let ((array (wide/array node))
	(stride (leaf-stride payload?))
	(c (wide/children node))
	(l (wide/leaves node)))
    (do-times (i hamt-bucket-size)
      (let ((j (* stride i)))
	(cond ((bit-string-ref l i)
	       (let ((k (vector-ref array j))
		     (d (and payload? (vector-ref array (1+ j)))))
		 (procedure k d)))
	      ((bit-string-ref c i)
	       (let ((child (vector-ref array j)))
		 (hamt-node/for-each child payload? procedure))))))))

(define (hamt-node/for-each node payload? procedure)
  (cond ((collision? node) (collision/for-each procedure node payload?))
	((narrow? node) (narrow/for-each procedure node payload?))
	((wide? node) (wide/for-each procedure node payload?))
	(else (error "Invalid type of node." node))))

(define (hamt/for-each procedure hamt)
  (hamt-node/for-each (hamt/root hamt)
		      (hamt/payload? hamt)
		      procedure))

(define (hamt->list hamt procedure)
  (let ((accumulator '()))
    (hamt/for-each (lambda (k v)
		     (set! accumulator
			   (cons (procedure k v)
				 accumulator)))
		   hamt)
    accumulator))

(define (hamt-max-depth hamt)
  "Return maximum depth of `hamt'.  For testing."
  (let descend ((n (hamt/root hamt)))
    (cond ((collision? n) 1)
	  ((narrow? n)
	   (let* ((array (narrow/array n))
		  (stride (leaf-stride (hamt/payload? hamt)))
		  (start (* stride (population-count (narrow/leaves n))))
		  (end (vector-length array)))
	     (do ((i start (1+ i))
		  (high 0 (max high (descend (vector-ref array i)))))
		 ((= i end) (1+ high)))))
	  ((wide? n)
	   (let ((array (wide/array n))
		 (c (wide/children n)))
	     (let next-child ((high 0)
			      (i 0))
	       (cond ((bit-substring-find-next-set-bit c i hamt-bucket-size)
		      => (lambda (j)
			   (next-child (max high (descend (vector-ref array j)))
				       (1+ j))))
		     (else (1+ high))))))
	  (else (error "Invalid type of node." n)))))

;;; Debugging

(define (show-hash-fragments hash)
  "Show hash fragments in reverse order."
  (let ((b (unsigned-integer->bit-string hamt-hash-size hash)))
    (do ((shift 0 (+ shift hamt-hash-slice-size)))
	((> shift hamt-hash-size))
      (let ((fragment (bit-substring b
				     shift
				     (min (+ shift hamt-hash-slice-size)
					  hamt-hash-size))))
	(format #t
		"~A: ~A (~A)~%"
		shift
		fragment
		(bit-string->unsigned-integer fragment))))))

(define (assert-collision-valid node hp payload?)
  "Do sanity checks on a collision.  Return the list of all keys
present."
  (let ((entries (collision/entries node))
	(hash (collision/hash node))
	(extract (if payload? car (lambda (x) x))))
    (do-list (a entries)
      (assert (bit-string=? hash (hash-bits hp (extract a)))))
    (if payload?
	(map car entries)
	entries)))

(define (assert-narrow-valid node hp payload? shift)
  "Do sanity checks on a narrow and all its children.  Return the list
of all keys present."
  (let ((array (narrow/array node))
	(stride (leaf-stride payload?))
	(c (narrow/children node))
	(l (narrow/leaves node)))
    (assert (bit-string-zero? (bit-string-and c l)))
    (let next-leaf ((count 0)
		    (i 0)
		    (keys '()))
      (if (< i hamt-bucket-size)
	  (cond ((bit-string-ref l i)
		 (let ((k (vector-ref array (* stride count))))
		   (assert (= i (hash-fragment shift (hash-bits hp k))))
		   (next-leaf (1+ count) (1+ i) (cons k keys))))
		(else (next-leaf count (1+ i) keys)))
	  (let next-child ((i 0)
			   (key-groups (list keys))
			   (offset (* stride count)))
	    (if (= i hamt-bucket-size)
		(apply append key-groups)
		(cond ((bit-string-ref c i)
		       (let* ((child (vector-ref array offset))
			      (child-keys (assert-hamt-node-valid
					   child
					   hp
					   payload?
					   (+ shift hamt-hash-slice-size))))
			 (do-list (k child-keys)
			   (assert (= i
				      (hash-fragment shift (hash-bits hp k)))))
			 (next-child (1+ i)
				     (cons child-keys key-groups)
				     (1+ offset))))
		      (else (next-child (1+ i) key-groups offset)))))))))

(define (assert-wide-valid node hp payload? shift)
  "Do sanity checks on a wide and all its children.  Return the list
of all keys present."
  (let ((array (wide/array node))
	(stride (leaf-stride payload?))
	(c (wide/children node))
	(l (wide/leaves node)))
    (assert (bit-string-zero? (bit-string-and c l)))
    (let next-fragment ((i 0)
			(key-groups '()))
      (if (= i hamt-bucket-size)
	  (apply append key-groups)
	  (let ((j (* stride i)))
	    (cond ((bit-string-ref l i)
		   (let ((k (vector-ref array j)))
		     (assert (= i (hash-fragment shift (hash-bits hp k))))
		     (next-fragment (1+ i) (cons (list k) key-groups))))
		  ((bit-string-ref c i)
		   (let* ((child (vector-ref array j))
			  (child-keys (assert-hamt-node-valid
				       child
				       hp
				       payload?
				       (+ shift hamt-hash-slice-size))))
		     (do-list (k child-keys)
		       (assert (= i
				  (hash-fragment shift (hash-bits hp k)))))
		     (next-fragment (1+ i)
				    (cons child-keys key-groups))))
		  (else
		   (assert (not (vector-ref array j)))
		   (when payload?
		     (assert (not (vector-ref array (1+ j)))))
		   (next-fragment (1+ i) key-groups))))))))

(define (assert-hamt-node-valid node hp payload? shift)
  "Do sanity checks on a HAMT node and all its children.  Return the
list of all keys present."
  (cond ((collision? node) (assert-collision-valid node hp payload?))
	((narrow? node) (assert-narrow-valid node hp payload? shift))
	((wide? node) (assert-wide-valid node hp payload? shift))
	(else (error "Invalid type of node." node))))

(define (assert-hamt-valid hamt)
  "Do sanity checks on `hamt'."
  (let ((hp (hamt/hash hamt)))
    (assert (procedure? (hamt/= hamt)))
    (assert (procedure? hp))
    (assert (memq (hamt/mutable? hamt) '(#t #f)))
    (let* ((payload? (hamt/payload? hamt))
	   (keys (assert-hamt-node-valid (hamt/root hamt) hp payload? 0)))
      (assert (= (hamt/count hamt) (length keys))))))