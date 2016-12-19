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
  (declare (comparator? comparator))
  (%make-map comparator (make-tree)))

;;; Exported procedures

;; Constructors

(define (make-map comparator . args)
  (declare (comparator? comparator))
  (map-unfold null?
	      (lambda (args)
		(values (car args)
			(cadr args)))
	      cddr
	      args
	      comparator))

(define (map-unfold stop? mapper successor seed comparator)
  (declare (procedure? stop?))
  (declare (procedure? mapper))
  (declare (procedure? successor))
  (declare (comparator? comparator))
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
  (declare (map? map))
  (not (map-any? (lambda (key value) #t) map)))

(define (map-contains? map key)
  (declare (map? map))
  (call/cc
   (lambda (return)
     (map-search map
		 key
		 (lambda (insert ignore)
		   (return #f))
		 (lambda (key value update remove)
		   (return #t))))))

(define (map-disjoint? map1 map2)
  (declare (map? map1))
  (declare (map? map2))
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
     (map-ref map key (lambda ()
			(fatal-error "map-ref: key not in map" key))))
    ((map key failure)
     (map-ref map key failure (lambda (value)
				value)))
    ((map key failure success)
     (declare (map? map))
     (declare (procedure? failure))
     (declare (procedure? success))
     (call/cc
      (lambda (return)
	(map-search map
		    key
		    (lambda (insert ignore)
		      (return (failure)))
		    (lambda (key value update remove)
		      (return (success value)))))))))

(define (map-ref/default map key default)
  (map-ref map key (lambda () default)))

;; Updaters

(define (map-set map . args)
  (declare (map? map))
  (let loop ((args args)
	     (map map))
    (if (null? args)
	map
	(receive (map value)
	    (map-intern map (car args) (lambda () (cadr args)))	
	  (loop (cddr args)
		map)))))

(define map-set! map-set)

(define (map-replace map key value)
  (declare (map? map))
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
  (declare (map? map))
  (map-delete-all map keys))

(define map-delete! map-delete)

(define (map-delete-all map keys)
  (declare (map? map))
  (declare (list? keys))
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
  (declare (map? map))
  (declare (procedure? failure))
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
    (declare (map? map))
    (declare (procedure? updater))
    (declare (procedure? failure))
    (declare (procedure? success))
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
  (declare (map? map))
  (declare (procedure? failure))
  (declare (procedure? success))
  (let*-values
      (((comparator)
	(map-key-comparator map))
       ((tree obj)
	(tree-search comparator
		     (map-tree map)
		     key
		     failure
		     success)))
    (values (%make-map comparator tree)
	    obj)))

(define map-search! map-search)

;; The whole map

(define (map-size map)
  (declare (map? map))
  (map-count (lambda (key value)
	       #t)
	     map))

(define (map-find predicate map failure)
  (declare (procedure? predicate))
  (declare (map? map))
  (declare (procedure? failure))
  (call/cc
   (lambda (return)
     (map-for-each (lambda (key value)
		     (when (predicate key value)
		       (return key value)))
		   map)
     (failure))))

(define (map-count predicate map)
  (declare (procedure? predicate))
  (declare (map? map))
  (map-fold (lambda (key value count)
	      (if (predicate key value)
		  (+ 1 count)
		  count))
	    0 map))

(define (map-any? predicate map)
  (declare (procedure? predicate))
  (declare (map? map))
  (call/cc
   (lambda (return)
     (map-for-each (lambda (key value)
		     (when (predicate key value)
		       (return #t)))
		   map)
     #f)))

(define (map-every? predicate map)
  (declare (procedure? predicate))
  (declare (map? map))
  (not (map-any? (lambda (key value)
		   (not (predicate key value)))
		 map)))

(define (map-keys map)
  (declare (map? map))
  (map-fold (lambda (key value keys)
	      (cons key keys))
	    '() map))

(define (map-values map)
  (declare (map? map))
  (map-fold (lambda (key value values)
	      (cons value values))
	    '() map))

(define (map-entries map)
  (values (map-keys map)
	  (map-values map)))

;; Mapping and folding

(define (map-fold proc acc map)
  (declare (procedure? proc))
  (declare (map? map))
  (tree-fold proc acc (map-tree map)))

(define (map-for-each proc map)
  (declare (procedure? proc))
  (declare (map? map))
  (tree-for-each proc (map-tree map)))

;; Copying and conversion

(define (map-copy map)
  map)

(define (map->alist map)
  (map-fold (lambda (key value alist)
	      (cons (cons key value) alist))
	    '() map))
