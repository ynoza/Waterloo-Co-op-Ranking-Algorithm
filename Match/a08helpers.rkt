;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08helpers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(require "provide.rkt")
(provide random-epsilon)

;;-----------------------------------------------------------------------
;; Provided code
;;-----------------------------------------------------------------------

  
;; (random-epsilon eid sid) produces a "random" number in the range [0, 1)
;; based on the two Ids provided.  It will produce the same number
;; given the same inputs.
;; random-epsilon: Str Str -> Num
(check-within (random-epsilon "Google" "Anna") 0.26230023 0.0000001)
(check-within (random-epsilon "zzzzzzzz" "zzzzzzzz") 0.99999999 0.0000001)

(define (random-epsilon eid sid)
  (local [;; A constant determining the precision of the random number.
          (define len 8)
          
          ;; (pad s len) Pad s with spaces to the length len.
          ;; pad: Str Nat -> Str
          (define (pad s)
            (substring (string-append s (replicate len " "))  0 len))

          ;; (insterleave lst1 lst2) interleaves (alternates) the elements
          ;; of lst1 and lst2, the first element of lst1 being first.
          ;; interleave: (listof Any) (listof Any) -> (listof Any)
          ;; requires: lst1 and lst2 must be the same length
          (define (interleave lst1 lst2)
            (foldr (lambda (e1 e2 r) (cons e1 (cons e2 r))) empty lst1 lst2))


          ;; (str->lon s) converts a string to a list of numbers.
          ;; str->lon: Str -> (listof Nat)
          (define (str->lon s) (map char->integer (string->list s)))

          ;; Combine the employer and student ids; convert to list of nats.
          (define ids (interleave (str->lon (pad eid)) (str->lon (pad sid))))

          ;; The "largest" possible combined id.
          (define maxid (str->lon (replicate (* 2 len) "z")))

          ;; Indicies for combining
          (define indices (build-list (* 2 len) (lambda (x) (expt 10 x))))

          ;; (combine x i r) computes x^i+r
          ;; combine: Nat Nat Nat -> Nat
          (define (combine x i r) (+ (* x i) r))
          ]
    (/ (foldr combine 0 ids indices)
       (add1 (foldr combine 0 maxid indices)))
    ))

