;; Copyright (C) Marc Nieper-Wißkirchen (2016).  All Rights Reserved. 

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

;;; New types

(define-record-type <map>
  (%make-map comparator tree)
  map?
  (comparator map-key-comparator)
  (tree map-tree))

(define (make-empty-map comparator)
  (assume-type comparator? comparator)
  (%make-map comparator (make-tree)))

;;; Exported procedures

;; Constructors

(define (make-map comparator . args)
  (assume-type comparator? comparator)
  (map-unfold null?
	      (lambda (args)
		(values (car args)
			(cadr args)))
	      cddr
	      args
	      comparator))

(define (map-unfold stop? mapper successor seed comparator)
  (assume-type procedure? stop?)
  (assume-type procedure? mapper)
  (assume-type procedure? successor)
  (assume-type comparator? comparator)
  (let loop ((map (make-empty-map comparator))
	     (seed seed))
    (if (stop? seed)
	map
	(receive (key value)
	    (mapper seed)
	  (loop (map-set map key value)
		(successor seed))))))

;; Predicates

(define (map-empty? map)
  (assume-type map? map)
  (not (map-any? (lambda (key value) #t) map)))

(define (map-contains? map key)
  (assume-type map? map)
  (call/cc
   (lambda (return)
     (map-search map
		 key
		 (lambda (insert ignore)
		   (return #f))
		 (lambda (key value update remove)
		   (return #t))))))

(define (map-disjoint? map1 map2)
  (assume-type map? map1)
  (assume-type map? map2)
  (call/cc
   (lambda (return)
     (map-for-each (lambda (key value)
		     (when (map-contains? map2 key)
		       (return #f)))
		   map1)
     #t)))

;; Accessors

(define map-ref
  (case-lambda
    ((map key)
     (assume-type map? map)
     (map-ref map key (lambda ()
			(fatal-error "map-ref: key not in map" key))))
    ((map key failure)
     (assume-type map? map)
     (assume-type procedure? failure)
     (map-ref map key failure (lambda (value)
				value)))
    ((map key failure success)
     (assume-type map? map)
     (assume-type procedure? failure)
     (assume-type procedure? success)
     (call/cc
      (lambda (return)
	(map-search map
		    key
		    (lambda (insert ignore)
		      (return (failure)))
		    (lambda (key value update remove)
		      (return (success value)))))))))

(define (map-ref/default map key default)
  (assume-type map? map)
  (map-ref map key (lambda () default)))

;; Updaters

(define (map-set map . args)
  (assume-type map? map)
  (let loop ((args args)
	     (map map))
    (if (null? args)
	map
	(receive (map)
	    (map-update map (car args) (lambda (value) (cadr args)) (lambda () #f))	
	  (loop (cddr args)
		map)))))

(define map-set! map-set)

(define (map-replace map key value)
  (assume-type map? map)
  (receive (map obj)
      (map-search map
		  key
		  (lambda (insert ignore)
		    (ignore #f))
		  (lambda (old-key old-value update remove)
		    (update key value #f)))
    map))

(define map-replace! map-replace)

(define (map-delete map . keys)
  (assume-type map? map)
  (map-delete-all map keys))

(define map-delete! map-delete)

(define (map-delete-all map keys)
  (assume-type map? map)
  (assume-type list? keys)
  (fold (lambda (key map)
	  (receive (map obj)
	      (map-search map
			  key
			  (lambda (insert ignore)
			    (ignore #f))
			  (lambda (old-key old-value update remove)
			    (remove #f)))
	    map))
	map keys))

(define map-delete-all! map-delete-all)

(define (map-intern map key failure)
  (assume-type map? map)
  (assume-type procedure? failure)
  (call/cc
   (lambda (return)
     (map-search map
		 key
		 (lambda (insert ignore)
		   (receive (value)
		       (failure)
		     (insert key value value)))
		 (lambda (old-key old-value update remove)
		   (return map old-value))))))

(define map-intern! map-intern)

(define map-update 
  (case-lambda
   ((map key updater)
    (map-update map key updater (lambda ()
				  (fatal-error "map-update: key not found in map" key))))
   ((map key updater failure)
    (map-update map key updater failure (lambda (value)
					  value)))
   ((map key updater failure success)
    (assume-type map? map)
    (assume-type procedure? updater)
    (assume-type procedure? failure)
    (assume-type procedure? success)
    (receive (map obj)
	(map-search map
		    key
		    (lambda (insert ignore)
		      (insert key (updater (failure)) #f))
		    (lambda (old-key old-value update remove)
		      (update key (updater (success old-value)) #f)))
      map))))

(define map-update! map-update)

(define (map-update/default map key updater default)
  (map-update map key updater (lambda () default)))

(define map-update!/default map-update/default)

(define (map-search map key failure success)
  (assume-type map? map)
  (assume-type procedure? failure)
  (assume-type procedure? success)
  (call/cc
   (lambda (return)
     (let*-values
	 (((comparator)
	   (map-key-comparator map))
	  ((tree obj)
	   (tree-search comparator
			(map-tree map)
			key
			(lambda (insert ignore)
			  (failure insert
				   (lambda (obj)
				     (return map obj))))
			success)))
       (values (%make-map comparator tree)
	       obj)))))

(define map-search! map-search)

;; The whole map

(define (map-size map)
  (assume-type map? map)
  (map-count (lambda (key value)
	       #t)
	     map))

(define (map-find predicate map failure)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (assume-type procedure? failure)
  (call/cc
   (lambda (return)
     (map-for-each (lambda (key value)
		     (when (predicate key value)
		       (return key value)))
		   map)
     (failure))))

(define (map-count predicate map)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (map-fold (lambda (key value count)
	      (if (predicate key value)
		  (+ 1 count)
		  count))
	    0 map))

(define (map-any? predicate map)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (call/cc
   (lambda (return)
     (map-for-each (lambda (key value)
		     (when (predicate key value)
		       (return #t)))
		   map)
     #f)))

(define (map-every? predicate map)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (not (map-any? (lambda (key value)
		   (not (predicate key value)))
		 map)))

(define (map-keys map)
  (assume-type map? map)
  (map-fold (lambda (key value keys)
	      (cons key keys))
	    '() map))

(define (map-values map)
  (assume-type map? map)
  (map-fold (lambda (key value values)
	      (cons value values))
	    '() map))

(define (map-entries map)
  (assume-type map? map)
  (values (map-keys map)
	  (map-values map)))

;; Mapping and folding

(define (map-map proc comparator map)
  (assume-type procedure? proc)
  (assume-type comparator? comparator)
  (assume-type map? map)
  (map-fold (lambda (key value map)
	      (receive (key value)
		  (proc key value)
		(map-set map key value)))
	    (make-empty-map comparator)
	    map))

(define (map-for-each proc map)
  (assume-type procedure? proc)
  (assume-type map? map)
  (tree-for-each proc (map-tree map)))

(define (map-fold proc acc map)
  (assume-type procedure? proc)
  (assume-type map? map)
  (tree-fold proc acc (map-tree map)))

(define (map-map->list proc map)
  (assume-type procedure? proc)
  (assume-type map? map)
  (map-fold (lambda (key value lst)
	      (cons (proc key value) lst))
	    '()
	    map))

(define (map-filter predicate map)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (map-fold (lambda (key value map)
	      (if (predicate key value)
		  (map-set map key value)
		  map))
	    (make-empty-map (map-key-comparator map))
	    map))

(define map-filter! map-filter)

(define (map-remove predicate map)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (map-filter (lambda (key value)
		(not (predicate key value)))
	      map))

(define map-remove! map-remove)

(define (map-partition predicate map)
  (assume-type procedure? predicate)
  (assume-type map? map)
  (values (map-filter predicate map)
	  (map-remove predicate map)))

(define map-partition! map-partition)

;; Copying and conversion

(define (map-copy map)
  (assume-type map? map)
  map)

(define (map->alist map)
  (assume-type map? map)
  (map-fold (lambda (key value alist)
	      (cons (cons key value) alist))
	    '() map))

(define (map->alist map)
  (assume-type map? map)
  (map-fold (lambda (key value alist)
	      (cons (cons key value) alist))
	    '() map))

(define (alist->map comparator alist)
  (assume-type comparator? comparator)
  (assume-type list? alist)
  (map-unfold null?
	      (lambda (alist)
		(let ((key (caar alist))
		      (value (cdar alist)))
		  (values key value)))
	      cdr
	      alist
	      comparator))

(define (alist->map! map alist)
  (assume-type map? map)
  (assume-type list? alist)
  (fold (lambda (association map)
	  (let ((key (car association))
		(value (cdr association)))
	    (map-set map key value)))
	map
	alist))

;; Submaps

(define map=?
  (case-lambda
    ((comparator map)
     (assume-type map? map)
     #t)
    ((comparator map1 map2) (%map=? comparator map1 map2))
    ((comparator map1 map2 . maps)
     (and (%map=? comparator map1 map2)
          (apply map=? comparator map2 maps)))))
(define (%map=? comparator map1 map2)
  (and (%map<=? comparator map1 map2)
       (%map<=? comparator map2 map1)))

(define map<=?
  (case-lambda
    ((comparator map)
     (assume-type map? map)
     #t)
    ((comparator map1 map2)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map<=? comparator map1 map2))
    ((comparator map1 map2 . maps)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (and (%map<=? comparator map1 map2)
          (apply map<=? comparator map2 maps)))))

(define (%map<=? comparator map1 map2)
  (assume-type comparator? comparator)
  (assume-type map? map1)
  (assume-type map? map2)
  (let ((less? (comparator-ordering-predicate (map-key-comparator map1)))
	(equality-predicate (comparator-equality-predicate comparator))
	(gen1 (tree-generator (map-tree map1)))
	(gen2 (tree-generator (map-tree map2))))    
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
	   ((equality-predicate item1 item2)
	    (loop (gen1) (gen2)))
	   (else
	    #f))))))))

(define map>?
  (case-lambda
    ((comparator map)
     (assume-type map? map)
     #t)
    ((comparator map1 map2)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map>? comparator map1 map2))
    ((comparator map1 map2 . maps)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (and (%map>? comparator  map1 map2)
          (apply map>? comparator map2 maps)))))

(define (%map>? comparator map1 map2)
  (assume-type comparator? comparator)
  (assume-type map? map1)
  (assume-type map? map2)
  (not (%map<=? comparator map1 map2)))

(define map<?
  (case-lambda
    ((comparator map)
     (assume-type map? map)
     #t)
    ((comparator map1 map2)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map<? comparator map1 map2))
    ((comparator map1 map2 . maps)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (and (%map<? comparator  map1 map2)
          (apply map<? comparator map2 maps)))))

(define (%map<? comparator map1 map2)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map>? comparator map2 map1))

(define map>=?
  (case-lambda
    ((comparator map)
     (assume-type map? map)
     #t)
    ((comparator map1 map2)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map>=? comparator map1 map2))
    ((comparator map1 map2 . maps)
     (assume-type comparator? comparator)
     (assume-type map? map1)
     (assume-type map? map2)
     (and (%map>=? comparator map1 map2)
          (apply map>=? comparator map2 maps)))))

(define (%map>=? comparator map1 map2)
  (assume-type comparator? comparator)
  (assume-type map? map1)
  (assume-type map? map2)
  (not (%map<? comparator map1 map2)))

;; Set theory operations

(define (%map-union map1 map2)
  (map-fold (lambda (key2 value2 map)
	      (receive (map obj)
		  (map-search map
			      key2
			      (lambda (insert ignore)
				(insert key2 value2 #f))
			      (lambda (key1 value1 update remove)
				(update key1 value1 #f)))
		map))
	    map1 map2))

(define (%map-intersection map1 map2)
  (map-filter (lambda (key1 value1)
		(map-contains? map2 key1))
	      map1))

(define (%map-difference map1 map2)
  (map-fold (lambda (key2 value2 map)
	      (receive (map obj)
		  (map-search map
			      key2
			      (lambda (insert ignore)
				(ignore #f))
			      (lambda (key1 value1 update remove)
				(remove #f)))
		map))
	    map1 map2))

(define (%map-xor map1 map2)
  (map-fold (lambda (key2 value2 map)
	      (receive (map obj)
		  (map-search map
			      key2
			      (lambda (insert ignore)
				(insert key2 value2 #f))
			      (lambda (key1 value1 update remove)
				(remove #f)))
		map))
	    map1 map2))

(define map-union
  (case-lambda
    ((map)
     (assume-type map? map)
     map)
    ((map1 map2)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map-union map1 map2))
    ((map1 map2 . maps)
     (assume-type map? map1)
     (assume-type map? map2)
     (apply map-union (%map-union map1 map2) maps))))
(define map-union! map-union)

(define map-intersection
  (case-lambda
    ((map)
     (assume-type map? map)
     map)
    ((map1 map2)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map-intersection map1 map2))
    ((map1 map2 . maps)
     (assume-type map? map1)
     (assume-type map? map2)
     (apply map-intersection (%map-intersection map1 map2) maps))))
(define map-intersection! map-intersection)

(define map-difference
  (case-lambda
    ((map)
     (assume-type map? map)
     map)
    ((map1 map2)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map-difference map1 map2))
    ((map1 map2 . maps)
     (assume-type map? map1)
     (assume-type map? map2)
     (apply map-difference (%map-difference map1 map2) maps))))
(define map-difference! map-difference)

(define map-xor
  (case-lambda
    ((map)
     (assume-type map? map)
     map)
    ((map1 map2)
     (assume-type map? map1)
     (assume-type map? map2)
     (%map-xor map1 map2))
    ((map1 map2 . maps)
     (assume-type map? map1)
     (assume-type map? map2)
     (apply map-xor (%map-xor map1 map2) maps))))
(define map-xor! map-xor)

;; Comparators

(define (map-equality comparator)
  (assume-type comparator? comparator)
  (lambda (map1 map2)
    (map=? comparator map1 map2)))

(define (map-ordering comparator)
  (assume-type comparator? comparator)
  (let ((value-equality (comparator-equality-predicate comparator))
	(value-ordering (comparator-ordering-predicate comparator)))
    (lambda (map1 map2)
      (let* ((key-comparator (map-key-comparator map1))
	     (equality (comparator-equality-predicate key-comparator))
	     (ordering (comparator-ordering-predicate key-comparator))
	     (gen1 (tree-generator (map-tree map1)))
	     (gen2 (tree-generator (map-tree map2))))
	(let loop ()
	  (let ((item1 (gen1)) (item2 (gen2)))
	    (cond
	     ((eof-object? item1)
	      (not (eof-object? item2)))
	     ((eof-object? item2)
	      #f)
	     (else
	      (let ((key1 (car item1)) (value1 (cadr item1))
		    (key2 (car item2)) (value2 (cadr item2)))
		(cond
		 ((equality key1 key2)
		  (if (value-equality value1 value2)
		      (loop)
		      (value-ordering value1 value2)))
		 (else
		  (ordering key1 key2))))))))))))

(define (make-map-comparator comparator)
  (make-comparator map? (map-equality comparator) (map-ordering comparator) #f))

(define map-comparator (make-map-comparator (make-default-comparator)))

(comparator-register-default! map-comparator)

