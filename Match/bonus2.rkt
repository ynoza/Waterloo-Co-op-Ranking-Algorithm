;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 08, Problem 7-Bonus
;; ***************************************************
;;

;; ==== Question 7a ========================

;; (my-compose f g) takes in two functions (f and g) and produces
;;    a function that when applied to an argument x, gives the same
;;    result as if g is applied to x, and then f is applied to the result
;; my-compose: (Y -> Z) (X -> Y) -> (X -> Z) 
        
(define (my-compose f g)
    (lambda (x) (f (g x))))


;; ==== Question 7b ========================

;; (curry f) takes in a 2 arguement function f and g, in that order, and
;;    produces a one-arguemnt function that when applied to an argument x,
;;    produces another function that, if applied to an argument y, gives the
;;    same result as if f has been applied to the 2 arguments x and y
;; curry: (X Y -> Z) -> (X -> (Y -> Z))
               
(define (curry f)
  (lambda(x) (lambda(y) (f x y))))


;; ==== Question 7c ========================

;; (uncurry f) takes in a 2 arguement function, and does the opposite of
;;    curry to produce a functionally equaivalent f
;; uncurry: (X -> (Y -> Z)) -> (X Y -> Z)

(define (uncurry f)
  (lambda (x y) ((f x) y)))


;; ==== Question 7d ========================

;; (eat-apples lst) consumes a list of symbols (lst) and removes all the 
;;    elements of the list that are 'apple
;; eat-apples: (listof Sym) -> (listof Sym)
;; Examples:
(check-expect (eat-apples '()) '())
(check-expect (eat-apples (list 'apple 'bananna 'apple)) '(bananna))

(define (eat-apples lst)
  (filter (my-compose not ((curry symbol=?) 'apple)) lst))

;;Tests:
(check-expect (eat-apples (list 'apple 'bananna)) '(bananna))
(check-expect (eat-apples (list 'apple 'bananna 'bananna 'g 'u 'y))
              '(bananna bananna g u y))
(check-expect (eat-apples '(a f g s d f g h)) '(a f g s d f g h))
(check-expect (eat-apples '(apple apple)) '())


;; ==== Question 7e ========================

;; (my-map f lst) consumes a function (f) and a list (lst) and applies
;;    the function f to each element of the list
;; my-map: (X -> Y) (listof X) -> (listof Y)
;; Examples:
(check-expect (my-map add1 '(3 -4.01 2/5)) (list 4 -3.01 1.4))
(check-expect (my-map (lambda (x) (list 'my-list (+ x 1))) '(3 -4.01 2/5))
           (list (list 'my-list 4) (list 'my-list -3.01) (list 'my-list 1.4)))

(define (my-map f lst)    
  (foldr (uncurry (my-compose (curry cons) f))
         empty lst))

;;Tests:
(check-expect (my-map add1 '()) '())
(check-expect (my-map sub1 '(1)) '(0))