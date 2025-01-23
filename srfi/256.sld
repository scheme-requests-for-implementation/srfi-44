;; NOTE: This is a pre-release version of SRFI 256, vendored for compatibility.
;; Please use the finalized version.

;;; SPDX-FileCopyrightText: 2024 Daphne Preston-Kendal
;;; SPDX-License-Identifier: MIT

(define-library (srfi 256)
  (import (rename (scheme base)
                  (define-record-type r7rs:define-record-type)))
  (export define-record-type)

  (begin
    (r7rs:define-record-type RTD
			     (make-rtd parent n-own-fields)
			     rtd?
			     (parent rtd-parent)
			     (n-own-fields rtd-n-own-fields))
    (r7rs:define-record-type Record
			     (make-record type field-values)
			     record?
			     (type record-rtd)
			     (field-values record-field-values))
    (define base-rtd (make-rtd #f 0))
    (define base-record (make-record base-rtd #()))

    (define (rtd-n-all-fields rtd)
      (+ (rtd-n-own-fields rtd)
         (if (rtd-parent rtd)
             (rtd-n-all-fields (rtd-parent rtd))
             0)))

    (define-syntax define-record-type
      (syntax-rules ()
        ((_ (name parent-expr)
            (constructor _ field-tag1 ...)
            predicate
            (field-tag2 accessor . maybe-mutator) ...)
         (begin
           (define parent parent-expr)
           (define rtd
             (if (rtd? parent)
                 (make-rtd parent (length '(field-tag1 ...)))
                 (error "define-record-type: parent is not a record type"
                        parent)))
           (define name rtd)
           (define (predicate obj)
             (and (record? obj)
                  (let loop ((type (record-rtd obj)))
                    (and type
                         (or (eqv? type rtd)
                             (loop (rtd-parent type)))))))
           (define (constructor superinstance field-tag1 ...)
             (if (and (record? superinstance)
                      (eqv? (record-rtd superinstance) parent))
                 (make-record rtd (vector-append
                                   (record-field-values superinstance)
                                   (vector field-tag1 ...)))
                 (error "superinstance not the right type" superinstance rtd)))
           (define-record-type/accessors+mutators
             rtd predicate
             ((field-tag1 field-tag2 accessor . maybe-mutator) ...)
             ())))
        ((_ name (constructor field-tag ...) predicate field-spec ...)
         (begin
           (define-record-type (name base-rtd)
             (fake-constructor superinstance field-tag ...)
             predicate field-spec ...)
           (define (constructor field-tag ...)
             (fake-constructor base-record field-tag ...))))))

    (define-syntax define-record-type/accessors+mutators
      (syntax-rules ()
        ((_ _ _ () _) (begin))
        ((_ rtd pred
            ((field-tag1 field-tag2 accessor) more ...)
            checked)
         (bound-identifier=?
          field-tag1 field-tag2
          (begin
            (define field-id (+ (rtd-n-all-fields (rtd-parent rtd))
                                (length 'checked)))
            (define (accessor inst)
              (if (pred inst)
                  (vector-ref (record-field-values inst) field-id)
                  (error "not an instance of the right type" inst)))
            (define-record-type/accessors+mutators
              rtd pred
              (more ...)
              (n . checked)))
          (syntax-error "\
field tags must be the same in the constructor as in the field specs"
                        'field-tag1 'field-tag2)))
        ((_ rtd pred
            ((field-tag1 field-tag2 accessor mutator) more ...)
            checked)
         (bound-identifier=?
          field-tag1 field-tag2
          (begin
            (define field-id (+ (rtd-n-all-fields (rtd-parent rtd))
                                (length 'checked)))
            (define (accessor inst)
              (if (pred inst)
                  (vector-ref (record-field-values inst) field-id)
                  (error "not an instance of the right type" inst)))
            (define (mutator inst new-val)
              (if (pred inst)
                  (vector-set! (record-field-values inst) field-id new-val)
                  (error "not an instance of the right type" inst)))
            (define-record-type/accessors+mutators
              rtd pred
              (more ...)
              (n . checked)))
          (syntax-error "\
field tags must be the same in the constructor as in the field specs"
                        'field-tag1 'field-tag2)))))

    (define-syntax bound-identifier=?
      (syntax-rules ()
        ((bound-identifier=? id v kt kf)
         (begin
           (define-syntax m
             (syntax-rules :::1 ()
               ((m %kt %kf)
                (begin
                  (define-syntax id
                    (syntax-rules :::2 ()
                      ((id %%kt %%kf) %%kf)))
                  (define-syntax ok
                    (syntax-rules ()
                      ((ok %%kt %%kf) %%kt)))
                  (define-syntax test
                    (syntax-rules :::2 ()
                      ((test v %%kt %%kf) (id %%kt %%kf))
                      ((test _ %%kt %%kf) (id %%kt %%kf))))
                  (test ok %kt %kf)))))
           (m kt kf)))))))
