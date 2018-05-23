;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights
;; Reserved.

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
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-record-type <set>
  (make-set hashmap)
  set?
  (hashmap set-hashmap))

(define-record-type <bag>
  (make-bag hashmap)
  bag?
  (hashmap bag-hashmap))

(define hashmap-fold/reverse hashmap-fold)

(define (make-empty-set comparator)
  (make-set (hashmap comparator)))

(define (make-empty-bag comparator)
  (make-bag (hashmap comparator)))

;; Constructors

(define (set comparator . elements)
  (assume (comparator? comparator))
  (set-unfold null? car cdr elements comparator))

(define (bag comparator . elements)
  (assume (comparator? comparator))
  (bag-unfold null? car cdr elements comparator))

(define (set-unfold stop? mapper successor seed comparator)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (assume (comparator? comparator))
  (make-set (hashmap-unfold stop?
			    (lambda (seed)
			      (values (mapper seed) 1))
			    successor
			    seed
			    comparator)))

(define (bag-unfold stop? mapper successor seed comparator)
  (assume (procedure? stop?))
  (assume (procedure? mapper))
  (assume (procedure? successor))
  (assume (comparator? comparator))
  (let loop ((bag (make-empty-bag comparator))
	     (seed seed))
    (if (stop? seed)
	bag
	(loop (bag-adjoin bag (mapper seed))
	      (successor seed)))))

;; Predicates

(define (set-contains? set element)
  (assume (set? set))
  (hashmap-contains? (set-hashmap set) element))

(define (bag-contains? bag element)
  (assume (bag? bag))
  (hashmap-contains? (bag-hashmap bag) element))

(define (set-empty? set)
  (assume (set? set))
  (hashmap-empty? (set-hashmap set)))

(define (bag-empty? bag)
  (assume (bag? bag))
  (hashmap-empty? (bag-hashmap bag)))

(define (set-disjoint? set1 set2)
  (assume (set? set1))
  (assume (set? set2))
  (hashmap-disjoint? (set-hashmap set1) (set-hashmap set2)))

(define (bag-disjoint? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (hashmap-disjoint? (bag-hashmap bag1) (bag-hashmap bag2)))

;; Accessors

(define (set-member set element default)
  (assume (set? set))
  (call/cc
   (lambda (return)
     (hashmap-search (set-hashmap set)
		     element
		     (lambda (insert ignore)
		       (return default))
		     (lambda (old-element old-count update remove)
		       (return old-element))))))

(define (bag-member bag element default)
  (assume (bag? bag))
  (call/cc
   (lambda (return)
     (hashmap-search (bag-hashmap bag)
		     element
		     (lambda (insert ignore)
		       (return default))
		     (lambda (old-element old-count update remove)
		       (return old-element))))))

(define (set-element-comparator set)
  (assume (set? set))
  (hashmap-key-comparator (set-hashmap set)))

(define (bag-element-comparator bag)
  (assume (bag? bag))
  (hashmap-key-comparator (bag-hashmap bag)))

;; Updaters

(define (set-adjoin set . elements)
  (assume (set? set))
  (make-set (fold (lambda (element hashmap)
		    (receive (hashmap value)
			(hashmap-intern hashmap element (lambda () 1))
		      hashmap))
		  (set-hashmap set) elements)))

(define (bag-adjoin bag . elements)
  (assume (bag? bag))
  (fold (lambda (element bag)
	  (bag-increment bag element 1))
	bag elements))

(define set-adjoin! set-adjoin)
(define bag-adjoin! bag-adjoin)

(define (set-replace set element)
  (assume (set? set))
  (make-set (hashmap-replace (set-hashmap set) element 1)))

(define (bag-replace bag element)
  (assume (bag? bag))
  (receive (hashmap obj)
      (hashmap-search (bag-hashmap bag)
		      element
		      (lambda (insert ignore)
			(ignore #f))
		      (lambda (old-element count update remove)
			(update element count #f)))
    (make-bag hashmap)))

(define set-replace! set-replace)
(define bag-replace! bag-replace)

(define (set-delete set . elements)
  (assume (set? set))
  (set-delete-all set elements))

(define (bag-delete bag . elements)
  (assume (bag? bag))
  (bag-delete-all bag elements))

(define set-delete! set-delete)
(define bag-delete! bag-delete)

(define (set-delete-all set elements)
  (assume (set? set))
  (make-set (hashmap-delete-all (set-hashmap set) elements)))

(define (bag-delete-all bag elements)
  (assume (bag? bag))
  (make-bag (hashmap-delete-all (bag-hashmap bag) elements)))

(define set-delete-all! set-delete-all)
(define bag-delete-all! bag-delete-all)

(define (set-search! set element failure success)
  (assume (set? set))
  (assume (procedure? failure))
  (assume (procedure? success))
  (values set #f)
  (call/cc
   (lambda (return)
     (receive (hashmap obj)
	 (hashmap-search (set-hashmap set)
			 element
			 (lambda (insert ignore)
			   (failure (lambda (obj)
				      (insert 1 obj))
				    ignore))
			 (lambda (old-element count update remove)
			   (remove #f)
			   (success old-element
				    (lambda (new-element obj)
				      (if (=? (set-element-comparator set)
					      old-element
					      new-element)
					  (update new-element count obj)
					  (return (set-adjoin (set-delete set old-element)
							      new-element)
						  obj)))
				    remove)))
       (values (make-set hashmap) obj)))))

(define (bag-search! bag element failure success)
  (assume (bag? bag))
  (assume (procedure? failure))
  (assume (procedure? success))
  (call/cc
   (lambda (return)
     (receive (hashmap obj)
	 (hashmap-search (bag-hashmap bag)
			 element
			 (lambda (insert ignore)
			   (failure (lambda (obj)
				      (insert 1 obj))
				    ignore))
			 (lambda (old-element old-count update remove)
			   (success old-element
				    (lambda (new-element obj)
				      (if (=? (bag-element-comparator bag)
					      old-element
					      new-element)
					  (update new-element old-count obj)
					  (return (bag-adjoin (bag-delete bag old-element)
							      new-element)
						  obj)))
				    (lambda (obj)
				      (let ((new-count (- old-count 1)))
					(if (zero? new-count)
					    (remove obj)
					    (update old-element new-count obj)))))))
       (values (make-bag hashmap) obj)))))

;; The whole set

(define (set-size set)
  (assume (set? set))
  (hashmap-size (set-hashmap set)))

(define (bag-size bag)
  (assume (bag? bag))
  (hashmap-fold (lambda (element count acc)
		  (+ count acc))
		0 (bag-hashmap bag)))

(define (set-find predicate set failure)
  (assume (procedure? predicate))
  (assume (set? set))
  (assume (procedure? failure))
  ((call/cc
    (lambda (return-thunk)
      (receive (element count)
	  (hashmap-find (lambda (element count)
			  (predicate element))
			(set-hashmap set)
			(lambda () (return-thunk failure)))
	(lambda () element))))))

(define (bag-find predicate bag failure)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (assume (procedure? failure))
  ((call/cc
    (lambda (return-thunk)
      (receive (element count)
	  (hashmap-find (lambda (element count)
			  (predicate element))
			(bag-hashmap bag)
			(lambda () (return-thunk failure)))
	(lambda () element))))))

(define (set-count predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (hashmap-count (lambda (element count)
		   (predicate element))
		 (set-hashmap set)))

(define (bag-count predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (hashmap-fold (lambda (element count acc)
		  (if (predicate element)
		      (+ count acc)
		      acc))
		0 (bag-hashmap bag)))

(define (set-any? predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (hashmap-any? (lambda (element count)
		  (predicate element))
		(set-hashmap set)))

(define (bag-any? predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (hashmap-any? (lambda (element count)
		  (predicate element))
		(bag-hashmap bag)))

(define (set-every? predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (hashmap-every? (lambda (element count)
		    (predicate element))
		  (set-hashmap set)))

(define (bag-every? predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (hashmap-every? (lambda (element count)
		    (predicate element))
		  (bag-hashmap bag)))

;; Mapping and folding

(define (set-map proc comparator set)
  (assume (procedure? proc))
  (assume (comparator? comparator))
  (assume (set? set))
  (make-set (hashmap-map (lambda (element count)
			   (values (proc element)
				   count))
			 (set-element-comparator set)
			 (set-hashmap set))))

(define (bag-map proc comparator bag)
  (assume (procedure? proc))
  (assume (comparator? comparator))
  (assume (bag? bag))
  (hashmap-fold (lambda (element count bag)
		  (let loop ((count count) (bag bag))
		    (if (zero? count)
			bag
			(loop (- count 1) (bag-adjoin bag (proc element))))))
		(make-empty-bag comparator) (bag-hashmap bag)))

(define (set-for-each proc set)
  (assume (procedure? proc))
  (assume (set? set))
  (hashmap-for-each (lambda (element count)
		      (proc element))
		    (set-hashmap set)))

(define (bag-for-each proc bag)
  (assume (procedure? proc))
  (assume (bag? bag))
  (hashmap-for-each (lambda (element count)
		      (do ((count count (- count 1)))
			  ((zero? count))
			(proc element)))
		    (bag-hashmap bag)))

(define (set-fold proc nil set)
  (assume (procedure? proc))
  (assume (set? set))
  (hashmap-fold (lambda (element count nil)
		  (proc element nil))
		nil (set-hashmap set)))

(define (bag-fold proc nil bag)
  (assume (procedure? proc))
  (assume (bag? bag))
  (hashmap-fold (lambda (element count acc)
		  (let loop ((count count) (acc acc))
		    (if (zero? count)
			acc
			(loop (- count 1) (proc element acc)))))
		nil (bag-hashmap bag)))

(define (set-filter predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (make-set (hashmap-filter (lambda (element count)
			      (predicate element))
			    (set-hashmap set))))

(define (bag-filter predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (make-bag (hashmap-filter (lambda (element count)
			      (predicate element))
			    (bag-hashmap bag))))

(define set-filter! set-filter)
(define bag-filter! bag-filter)

(define (set-remove predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (make-set (hashmap-remove (lambda (element count)
			      (predicate element))
			    (set-hashmap set))))

(define (bag-remove predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (make-bag (hashmap-remove (lambda (element count)
			      (predicate element))
			    (bag-hashmap bag))))

(define set-remove! set-remove)
(define bag-remove! bag-remove)

(define (set-partition predicate set)
  (assume (procedure? predicate))
  (assume (set? set))
  (receive (hashmap1 hashmap2)
      (hashmap-partition (lambda (element count)
			(predicate element))
			 (set-hashmap set))
    (values (make-set hashmap1) (make-set hashmap2))))

(define (bag-partition predicate bag)
  (assume (procedure? predicate))
  (assume (bag? bag))
  (receive (hashmap1 hashmap2)
      (hashmap-partition (lambda (element count)
			(predicate element))
			 (bag-hashmap bag))
    (values (make-bag hashmap1) (make-bag hashmap2))))

(define set-partition! set-partition)
(define bag-partition! bag-partition)

;; Copying and conversion

(define (set-copy set)
  (assume (set? set))
  set)

(define (bag-copy bag)
  (assume (bag? bag))
  bag)

(define (set->list set)
  (assume (set? set))
  (hashmap-fold/reverse (lambda (element count lst)
			  (cons element lst))
			'() (set-hashmap set)))

(define (bag->list bag)
  (assume (bag? bag))
  (hashmap-fold/reverse (lambda (element count lst)
			  (let loop ((count count) (lst lst))
			    (if (zero? count)
				lst
				(loop (- count 1)
				      (cons element lst)))))
			'() (bag-hashmap bag)))

(define (list->set comparator lst)
  (assume (comparator? comparator))
  (assume (list? lst))
  (apply set comparator lst))

(define (list->bag comparator lst)
  (assume (comparator? comparator))
  (assume (list? lst))
  (apply bag comparator lst))

(define (list->set! set lst)
  (assume (set? set))
  (assume (list? lst))
  (apply set-adjoin set lst))

(define (list->bag! bag lst)
  (assume (bag? bag))
  (assume (list? lst))
  (apply bag-adjoin bag lst))

;; Subsets

(define (set=? set . sets)
  (assume (set? set))
  (apply hashmap=? (make-eqv-comparator) (set-hashmap set) (map set-hashmap sets)))

(define (bag=? bag . bags)
  (assume (bag? bag))
  (apply hashmap=? (make-eqv-comparator) (bag-hashmap bag) (map bag-hashmap bags)))

(define (set<? set . sets)
  (assume (set? set))
  (apply hashmap<? (make-eqv-comparator) (set-hashmap set) (map set-hashmap sets)))

(define (set>? set . sets)
  (assume (set? set))
  (apply hashmap>? (make-eqv-comparator) (set-hashmap set) (map set-hashmap sets)))

(define (set<=? set . sets)
  (assume (set? set))
  (apply hashmap<=? (make-eqv-comparator) (set-hashmap set) (map set-hashmap sets)))

(define (set>=? set . sets)
  (assume (set? set))
  (apply hashmap>=? (make-eqv-comparator) (set-hashmap set) (map set-hashmap sets)))

(define bag<=?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag<=? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag<=? bag1 bag2)
          (apply bag<=? bag2 bags)))))

(define (%bag<=? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (let ((less? (comparator-ordering-predicate (bag-element-comparator bag1)))
	(gen1 (bag-generator bag1))
	(gen2 (bag-generator bag2)))
    (let loop ((item1 (gen1))
	       (item2 (gen2)))
      (cond
       ((eof-object? item1)
	#t)
       ((eof-object? item2)
	#f)
       (else
	(let ((key1 (car item1)) (value1 (cadr item1))
	      (key2 (car item2)) (value2 (cadr item2)))
	  (cond
	   ((less? key1 key2)
	    #f)
	   ((less? key2 key1)
	    (loop item1 (gen2)))
	   ((<= value1 value2)
	    (loop (gen1) (gen2)))
	   (else
	    #f))))))))

(define bag>?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag>? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag>? bag1 bag2)
          (apply bag>? bag2 bags)))))

(define (%bag>? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (not (%bag<=? bag1 bag2)))

(define bag<?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag<? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag<? bag1 bag2)
          (apply bag<? bag2 bags)))))

(define (%bag<? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (%bag>? bag2 bag1))

(define bag>=?
  (case-lambda
    ((bag)
     (assume (bag? bag))
     #t)
    ((bag1 bag2)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (%bag>=? bag1 bag2))
    ((bag1 bag2 . bags)
     (assume (bag? bag1))
     (assume (bag? bag2))
     (and (%bag>=? bag1 bag2)
          (apply bag>=? bag2 bags)))))

(define (%bag>=? bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (not (%bag<? bag1 bag2)))

(define (bag-generator bag)
  (make-coroutine-generator
   (lambda (yield)
     (hashmap-for-each (lambda item (yield item)) (bag-hashmap bag)))))

;; Set theory operations

(define (set-union set . sets)
  (assume (set? set))
  (make-set (apply hashmap-union (set-hashmap set) (map set-hashmap sets))))

(define (set-intersection set . sets)
  (assume (set? set))
  (make-set (apply hashmap-intersection (set-hashmap set) (map set-hashmap sets))))

(define (set-difference set . sets)
  (assume (set? set))
  (make-set (apply hashmap-difference (set-hashmap set) (map set-hashmap sets))))

(define (set-xor set1 set2)
  (assume (set? set1))
  (assume (set? set2))
  (make-set (hashmap-xor (set-hashmap set1) (set-hashmap set2))))

(define set-union! set-union)
(define set-intersection! set-intersection)
(define set-difference! set-difference)
(define set-xor! set-xor)

(define (bag-union bag . bags)
  (assume (bag? bag))
  (fold (lambda (bag2 bag1)
	  (hashmap-fold (lambda (element count bag)
			  (bag-update bag element (lambda (old-count) (max old-count count))))
			bag1 (bag-hashmap bag2)))
	bag bags))

(define (bag-intersection bag . bags)
  (assume (bag? bag))
  (fold (lambda (bag2 bag1)
	  (%bag-intersection bag1 bag2))
	bag bags))

(define (%bag-intersection bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (let ((less? (comparator-ordering-predicate (bag-element-comparator bag1)))
	(gen1 (bag-generator bag1))
	(gen2 (bag-generator bag2)))
    (let loop ((item1 (gen1))
	       (item2 (gen2))
	       (bag (make-empty-bag (bag-element-comparator bag1))))
      (cond
       ((eof-object? item1)
	bag)
       ((eof-object? item2)
	bag)
       (else
	(let ((key1 (car item1)) (count1 (cadr item1))
	      (key2 (car item2)) (count2 (cadr item2)))
	  (cond
	   ((less? key1 key2)
	    (loop (gen1) item2 bag))
	   ((less? key2 key1)
	    (loop item1 (gen2) bag))
	   (else
	    (loop (gen1) (gen2) (bag-increment bag key1 (min count1 count2)))))))))))

(define (bag-difference bag . bags)
  (assume (bag? bag))
  (fold (lambda (bag2 bag1)
	  (hashmap-fold (lambda (element count bag)
			  (bag-update bag element (lambda (old-count) (max 0 (- old-count count)))))
			bag1 (bag-hashmap bag2)))
	bag bags))

(define (bag-xor bag1 bag2)
  (assume (bag? bag1))
  (assume (bag? bag2))
  (hashmap-fold (lambda (element count bag)
		  (bag-update bag element (lambda (old-count) (abs (- old-count count)))))
		bag1 (bag-hashmap bag2)))

(define bag-union! bag-union)
(define bag-intersection! bag-intersection)
(define bag-difference! bag-difference)
(define bag-xor! bag-xor)

(define (bag-update bag element updater)
  (receive (hashmap obj)
      (hashmap-search (bag-hashmap bag)
		      element
		      (lambda (insert ignore)
			(let ((new-count (updater 0)))
			  (if (zero? new-count)
			      (ignore #f)
			      (insert new-count #f))))
		      (lambda (old-element old-count update remove)
			(let ((new-count (updater old-count)))
			  (if (zero? new-count)
			      (remove #f)
			      (update element new-count #f)))))
    (make-bag hashmap)))

;; Additional bag procedures

(define (bag-sum bag . bags)
  (assume (bag? bag))
  (if (null? bags)
      bag
      (hashmap-fold (lambda (element count bag)
		      (bag-update bag element (lambda (old-count) (+ old-count count))))
		    bag (bag-hashmap (car bags)))))

(define bag-sum! bag-sum)

(define (bag-product n bag)
  (assume (not (negative? n)))
  (assume (bag? bag))
  (make-bag (hashmap-map (lambda (element count)
			   (values element (* n count)))
			 (bag-element-comparator bag)
			 (bag-hashmap bag))))

(define bag-product! bag-product)

(define (bag-unique-size bag)
  (assume (bag? bag))
  (hashmap-size (bag-hashmap bag)))

(define (bag-element-count bag element)
  (assume (bag? bag))
  (hashmap-ref/default (bag-hashmap bag) element 0))

(define (bag-for-each-unique proc bag)
  (assume (bag? bag))
  (hashmap-for-each proc (bag-hashmap bag)))

(define (bag-fold-unique proc nil bag)
  (assume (procedure? proc))
  (assume (bag? bag))
  (hashmap-fold proc nil (bag-hashmap bag)))

(define (bag-increment bag element count)
  (assume (exact-integer? count))
  (assume (bag? bag))
  (bag-update bag element (lambda (old-count)
			    (max 0 (+ count old-count)))))

(define bag-increment! bag-increment)

(define (bag-decrement! bag element count)
  (assume (exact-integer? count))
  (assume (bag? bag))
  (bag-update bag element (lambda (old-count)
			    (max 0 (- old-count count)))))

(define (bag->set bag)
  (assume (bag? bag))
  (make-set (hashmap-map (lambda (element count)
			   (values element 1))
			 (bag-element-comparator bag)
			 (bag-hashmap bag))))

(define (set->bag set)
  (assume (set? set))
  (make-bag (set-hashmap set)))

(define (set->bag! bag set)
  (set-fold (lambda (element bag)
	      (bag-adjoin! bag element))
	    bag set))

(define (bag->alist bag)
  (assume (bag? bag))
  (hashmap->alist (bag-hashmap bag)))

(define (alist->bag comparator alist)
  (assume (comparator? comparator))
  (assume (list? alist))
  (make-bag (alist->hashmap comparator alist)))

;; Comparators

(define hashmap-ordering
  (comparator-ordering-predicate (make-hashmap-comparator (make-default-comparator))))

(define (set-ordering set1 set2)
  (hashmap-ordering (set-hashmap set1) (set-hashmap set2)))

(define (bag-ordering bag1 bag2)
  (hashmap-ordering (bag-hashmap bag1) (bag-hashmap bag2)))

(define (hashmap-hash m)
  (let ((hash (comparator-hash-function (hashmap-key-comparator m))))
    (hashmap-fold (lambda (element count accumulator)
		    (+ (hash element) accumulator))
		  0
		  m)))

(define set-comparator
  (make-comparator set?
		   set=?
		   set-ordering
		   (lambda (s) (hashmap-hash (set-hashmap s)))))

(define bag-comparator
  (make-comparator bag?
		   bag=?
		   bag-ordering
		   (lambda (b) (hashmap-hash (bag-hashmap b)))))

(comparator-register-default! set-comparator)
(comparator-register-default! bag-comparator)
