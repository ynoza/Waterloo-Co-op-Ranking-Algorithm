;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 08, Problem 3
;; ***************************************************
;;

(require "provide.rkt")
(provide partition)

;; (partition pred? lst) consumes a predicate (pred?) and a list (lst).
;;    It produces a two element list (list X Y) where X is a list of items
;;    in the consumes list that satisfy the predicate and Y is a list of
;;    those that don't satisfy the predicate.
;; partition: (X -> Bool) (listof (anyof X Y)) -> (list (listof X) (listof Y))
;; Examples:
(check-expect (partition odd? '(1 2 3 4 5 6)) '((1 3 5) (2 4 6)))
(check-expect (partition even? empty) '(() ()))

(define (partition pred? lst)
  (local
    [(define (partition/acc orig-lst pred?-lst not-pred?-lst)
       (cond
         [(empty? orig-lst) (list (reverse pred?-lst) (reverse not-pred?-lst))]
         [(pred? (first orig-lst))
          (partition/acc (rest orig-lst)
                         (cons (first orig-lst) pred?-lst)
                         not-pred?-lst)]
         [else
          (partition/acc (rest orig-lst)
                         pred?-lst
                         (cons (first orig-lst) not-pred?-lst))]))]
    (partition/acc lst '() '())))


;;Tests:
(check-expect (partition even? '(1 2 3 4 5)) '((2 4) (1 3 5)))
(check-expect (partition positive? '(1 2 3 4 5)) '((1 2 3 4 5) ()))
(check-expect (partition negative? '(5 4 2 1 3)) '(() (5 4 2 1 3)))
(check-expect (partition symbol? '(a b c)) '((a b c) ()))
(check-expect (partition (lambda (y) (= (modulo y 5) 0))
                         '(3 4 5 6 7 8 9 10))
              '((5 10) (3 4 6 7 8 9)))
