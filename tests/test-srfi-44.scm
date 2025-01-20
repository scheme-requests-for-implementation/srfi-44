;; SRFI 64 tests for SRFI 44: Collections.
;; To run: chibi-scheme -A. tests/test-srfi-44.scm

;; SPDX-License-Identifier: MIT
;; SPDX-FileCopyrightText: 2024 Antero Mejr <mail@antr.me>

(import (except (scheme base) vector? make-vector vector map
                list? make-list list string? make-string string)
        (srfi 8)
        (srfi 44)
        (srfi 64))

(test-begin "SRFI 44")

(test-group "collection"
  (let ((col (collection 1 2 3)))
    (test-assert "predicate" (collection? col))
    (test-assert "name" (eq? 'collection (collection-name col)))
    (test-assert "make-*" (collection? (make-collection)))
    (test-assert "size" (zero? (collection-size (make-collection))))
    (test-assert "size" (= 3 (collection-size col)))
    (test-assert "get-any" (number? (collection-get-any col)))
    (test-assert "empty" (collection-empty? (make-collection)))
    (test-assert "->list" (null? (collection->list (make-collection))))
    (test-assert "clear" (collection-empty? (collection-clear col)))
    (collection-clear! col)
    (test-assert "clear!" (collection-empty? col))
    (test-assert "=" (collection= = (collection 1 2 3) (collection 3 2 1)))
    (test-assert "constructor" (collection? (collection 1 2 3)))
    (let ((col2 (collection 1 2 3)))
      (test-assert "copy" (collection= = (collection-copy col2) col2)))
    (test-assert "fold-left" (= 2 (collection-fold-left
                                   (collection 3 2 1)
                                   (lambda (x acc)
                                     (values #t (- acc x)))
                                   0)))
    (test-assert "fold-right" (= -6 (collection-fold-right
                                     (collection 3 2 1)
                                     (lambda (x acc)
                                       (values #t (- acc x)))
                                     0)))))

(test-group "inheritance"
  (test-assert "collection= list"
               (collection= = (collection-clear (list 1 2 3)) (list)))
  (test-assert "clear sequence"
               (collection-empty? (collection-clear (sequence 1 2 3)))))

(test-group "limited collection"
  (test-assert "vector limited" (limited-collection? (vector 1 2 3)))
  (test-assert "string limited" (limited-collection? (string #\a #\b #\c))))

(test-group "Purely mutable collection"
  (test-assert "vector purely mutable"
               (purely-mutable-collection? (vector 1 2 3)))
  (test-assert "string purely mutable"
               (purely-mutable-collection? (string #\a #\b #\c))))

(test-group "Ordered collection"
  (define list-ordering-func list-ref)
  (let ((ocol (make-ordered-collection list-ordering-func)))
    (sequence-insert-right! ocol 1)
    (sequence-insert-right! ocol 2)
    (sequence-insert-right! ocol 3)
    (test-assert "predicate" (ordered-collection? ocol))
    (test-assert "ofunc accessor"
                 (procedure? (ordered-collection-ordering-function ocol)))
    (test-assert "get-left" (= 1 (ordered-collection-get-left ocol)))
    (test-assert "get-right" (= 3 (ordered-collection-get-right ocol)))
    (receive (rest l) (ordered-collection-delete-left ocol)
      (test-assert "delete-left" (= 1 l))
      (let ((ocol2 (make-ordered-collection list-ordering-func)))
        (sequence-insert-right! ocol2 2)
        (sequence-insert-right! ocol2 3)
        (test-assert "equality" (collection= = ocol ocol2))))
    (let ((my-list (make-ordered-collection list-ordering-func)))
      (sequence-insert-right! my-list 1)
      (receive (_ l) (ordered-collection-delete-left! my-list)
        (test-assert "delete-left!" (collection-empty? my-list))))
    (let ((my-list (make-ordered-collection list-ordering-func)))
      (sequence-insert-right! my-list 1)
      (receive (rest r) (ordered-collection-delete-right my-list)
        (test-assert "delete-right" (= 3 r))
        (test-assert "delete-right" (collection-empty? rest))))
    (let ((ocol2 (make-ordered-collection list-ordering-func)))
      (sequence-insert-right! ocol2 1)
      (receive (_ r) (ordered-collection-delete-right! ocol2)
        (test-assert "delete-right!" (collection-empty? ocol2))))
    (test-assert "make-*"
                 (ordered-collection?
                  (make-ordered-collection
                   (lambda (col x)
                     (list-ref (collection->list col) x)))))))

(test-group "Directional collection"
  (test-assert "predicate" (directional-collection? (sequence 1 2 3)))
  (test-assert "get-left"
               (= 1 (directional-collection-get-left (sequence 1 2 3))))
  (test-assert "get-right"
               (= 3 (directional-collection-get-right (sequence 1 2 3))))
  (test-assert "insert-left"
               (= 1 (directional-collection-get-left
                     (directional-collection-insert-left (sequence 2 3) 1))))
  (test-assert "insert-right"
               (= 1 (directional-collection-get-right
                     (directional-collection-insert-right (sequence 2 3) 1))))
  ;; TODO
  ;; directional-collection-insert!-left directional-collection-insert!-right
  ;; directional-collection-delete-left directional-collection-delete-right
  ;; directional-collection-delete-left directional-collection-delete-right
  ;; directional-collection-delete!-left directional-collection-delete!-right
  )

(test-group "Bag"
  (test-assert "predicate" (bag? (make-bag)))
  (test-assert "constructor" (bag? (bag 1 2 3)))
  (test-assert "equiv-func accessor"
               (procedure? (bag-equivalence-function bag)))
  (test-assert "count" (= 1 (bag-count (bag 3 2 1) 1)))
  (test-assert "contains?" (bag-contains? (bag 1 2 3) 3))
  (test-assert "add" (bag-contains? (bag-add 1 (bag 2 3)) 1))
  (let ((b (bag 1 2 3)))
    (bag-add! b 4)
    (test-assert "add!" (bag-contains? b 4)))
  (test-assert "delete"
               (collection= equal? (bag 2 3) (bag-delete (bag 1 2 3) 1)))
  (let ((b (bag 1 2 3)))
    (bag-delete! b 1)
    (test-assert "delete!" (collection= equal? (bag 2 3) b)))
  (test-assert "delete-all"
               (collection= (bag 2 2) (bag-delete-all (bag 1 2 1 2) 1)))
  (let ((b (bag 1 2 1 2)))
    (bag-delete-all! b 1)
    (test-assert "delete-all!" (collection= equal? (bag 2 2) b)))
  (test-assert "add-from"
               (collection= (bag 1 2 3) (bag-add-from (bag 1) (bag 2 3))))
  (let ((b (bag 1 2 3)))
    (bag-add-from! b (bag 4))
    (test-assert "add-from!" (collection= equal? b (bag 1 2 3 4))))
  (test-assert "delete-from"
               (collection= (bag 1 2) (bag-delete-from (bag 1 2 3) (bag 3))))
  (let ((b (bag 1 2 3)))
    (bag-delete-from! b (bag 2 3))
    (test-assert "delete-from!" (collection= equal? b (bag 1))))
  ;; TODO
  ;;bag-delete-all-from bag-delete-all-from!
  )

(test-group "Set"
  (test-assert "predicate" (set? (make-set)))
  (test-assert "constructor" (set? (set 1 2 2 3)))
  (test-assert "equiv-func accessor"
               (procedure? (set-equivalence-function (set))))
  (test-assert "contains?" (set-contains? (set 1 2 3) 2))
  (test-assert "subset?" (set-subset? (set 1 2 3) (set 1) (set 2) (set 2 3)))
  ;; TODO
  ;; set-add set-add! set-delete set-delete! set-union set-union!
  ;; set-intersection set-intersection! set-difference set-difference!
  ;; set-symmetric-difference set-symmetric-difference!
  ;; set-add-from set-add-from! set-delete-from set-delete-from!
  )

(test-group "map"
  (test-assert "predicate" (map? (make-map)))
  (test-assert "constructor" (map? (map '(1 . 2) '(2 . 3))))
  (test-assert "equiv-func accessor"
               (procedure? (map-equivalence-function (make-map))))
  (test-assert "key-equiv-func accessor"
               (procedure? (map-key-equivalence-function (make-map))))
  ;; TODO
  ;; map-contains-key? map-keys->list map-get map-put map-put!
  ;; map-update map-update! map-delete map-delete!
  ;; map-delete-from map-delete-from! map-add-from map-add-from!
  ;; map-fold-keys-left map-fold-keys-right
  )

(test-group "sequence"
  (test-assert "predicate" (sequence? (make-sequence)))
  (test-assert "constructor"  (sequence? (sequence 1 2 3)))
  (test-assert "ref" (= 1 (sequence-ref (sequence 1 2 3) 0)))
  (test-assert "get-left" (= 1 (sequence-get-left (sequence 1 2 3))))
  (test-assert "get-right"  (= 3 (sequence-get-right (sequence 1 2 3))))
  (test-assert "insert-right"
               (collection= = (sequence-insert-right (sequence 1 2) 3)
                            (sequence 1 2 3)))
  (let ((seq (sequence 1 2)))
    (sequence-insert-right! seq 3)
    (test-assert "insert-right!" (collection= = (sequence 1 2 3) seq)))
  (test-assert "set" (collection= = (sequence 1 2 3)
                                  (sequence-set (sequence 1 1 3) 1 2)))
  (let ((seq (sequence 1 1 3)))
    (sequence-set! seq 1 2)
    (test-assert "set!" (collection= = (sequence 1 2 3) seq)))
  (test-assert "replace-from"
               (collection= = (sequence 1 2 3)
                            (sequence-replace-from (sequence 1 0 0) 1
                                                   (sequence 2 3))))
  (let ((seq (sequence 1 0 0)))
    (sequence-replace-from! seq 1 (sequence 2 3))
    (test-assert "replace-from!" (collection= = (sequence 1 2 3) seq)))
  (test-assert "fold-keys-left"
               (= 1 (sequence-fold-keys-left (sequence 1 2 3)
                                             (lambda (key val acc)
                                               (- acc val))
                                             0)))
  (test-assert "fold-keys-right"
               (= -3 (sequence-fold-keys-left (sequence 1 2 3)
                                              (lambda (key val acc)
                                                (- acc key))
                                              0))))

(test-group "flexible sequence"
  (test-assert "predicate" (flexible-sequence? (make-flexible-sequence)))
  (test-assert "constructor" (flexible-sequence? (flexible-sequence 1 2 3)))
  (test-assert "insert"
               (collection=
                = (flexible-sequence-insert (flexible-sequence 2 3) 0 1)
                (flexible-sequence 1 2 3)))
  (let ((fs (flexible-sequence 2 3)))
    (flexible-sequence-insert! fs 0 1)
    (test-assert "insert!" (collection= = fs (flexible-sequence 1 2 3))))
  (test-assert "delete-at"
               (collection= = (flexible-sequence 1 2 3)
                            (flexible-sequence-delete-at
                             (flexible-sequence 1 2 2 3) 1)))
  (let ((fs (flexible-sequence 1 2 2 3)))
    (flexible-sequence-delete-at! fs 1)
    (test-assert "delete-at!" (collection= = (flexible-sequence 1 2 3) fs))))

(test-group "vector"
  (test-assert "predicate" (vector? (make-vector 10)))
  (test-assert "constructor" (vector? (vector 1 2 3))))

(test-group "list"
  (test-assert "predicate" (list? (make-list)))
  (test-assert "constructor" (list? (list 1 2 3))))

(test-group "string"
  (test-assert "predicate" (string? (make-string 10)))
  (test-assert "constructor" (string? (string #\a #\b #\c))))

(test-group "alist-map"
  (test-assert "predicate" (alist-map? (make-alist-map =)))
  (test-assert "constructor" (alist-map? (alist-map '(1 . 2) '(3 . 4)))))

(test-end)
