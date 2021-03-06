;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 08, Problem 2
;; ***************************************************
;;


;; ==== Question 2a ========================

;; (occurrences lon num) consumes a list of numbers (lon) and a number (num)
;;   and produces the number of times the given number occurs in the list
;;   of numbers
;; occurences: (listof Num) Num -> Nat
;; Examples:
(check-expect (occurrences '(1) 5) 0)
(check-expect (occurrences '() 1) 0)
 
(define (occurrences lon num)
  (length (filter (lambda (fst) (= fst num)) lon)))

;;Tests:
(check-expect (occurrences '(1 1 1 1 1 1) 1) 6)
(check-expect (occurrences '(1 2 3 4 5) 0) 0)
(check-expect (occurrences '(89 1203 2322 111 0 3 2 4 3 3) 3) 3)
(check-expect (occurrences '(1 2 3 4 5) 1) 1)
(check-expect (occurrences '(5 5 5 5 5) 5) 5)
(check-expect (occurrences '(-5.5 -0.23 -5 -6.5 5) -0.23) 1)
(check-expect (occurrences '(5 5 5 5 5) 5) 5)


;; ==== Question 2b ========================

;; (absolutely-odd loi) consumes a list of integers (loi) and produces
;;    the sum of absolute values of the odd integers in the list
;; absolutely-odd: (listof Int) -> Nat
;; Examples:
(check-expect (absolutely-odd '(-5 -6 -7 1 3 -5)) 21)
(check-expect (absolutely-odd '(2 4 6 8 10)) 0)

(define (absolutely-odd loi)
  (foldr + 0 (map (lambda (fst) (abs fst)) (filter odd? loi))))

;;Tests:
(check-expect (absolutely-odd '(2)) 0)
(check-expect (absolutely-odd '(-3)) 3)
(check-expect (absolutely-odd '(1 2 3 4 5 6)) 9) 
(check-expect (absolutely-odd '(-8 -4 2 -88 23 3 0)) 26)
(check-expect (absolutely-odd '(0)) 0)
(check-expect (absolutely-odd '()) 0)
(check-expect (absolutely-odd '(1 3 4 5)) 9)


;; ==== Question 2c ========================

;; (zip lst1 lst2) consumes two lists (lst1) and (lst2) and produces a
;;   list of pairs (2 element list) where the ith pair contains the ith 
;;   element of the first list followed by the ith element of the second list
;; zip: (listof Any) (listof Any) -> (listof (list Any Any))
;; requires: lst1 and lst2 have to be of the same length
;; Examples:
(check-expect (zip empty empty) empty)
(check-expect (zip '(1) '(1)) '((1 1)))

(define (zip lst1 lst2)
  (map (lambda (fst-lst1 fst-lst2) (list fst-lst1 fst-lst2)) lst1 lst2))

;;Tests:
(check-expect (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
(check-expect (zip '(2 5 6 9 34) '(723 "h" a 3.4 -5))
              '((2 723) (5 "h") (6 a) (9 3.4) (34 -5)))

;; ==== Question 2d ========================

;; (unzip lop) consumes a list of pairs (lop) and produces a list of 2 lists.
;;   The first contains the first element from each pair, and the second
;;   list contains the second element from each pair, in the original order
;; unzip: (listof (list Any Any)) -> (list (listof Any) (listof Any))
;; Examples:
(check-expect (unzip empty) (list empty empty))

(define (unzip lop)
  (list (map (lambda (fst) (first fst)) lop)
        (map (lambda (fst) (second fst)) lop)))

;;Tests:
(check-expect (unzip '((1 4) (2 5) (3 6))) '((1 2 3) (4 5 6)))
(check-expect (unzip '((1 4))) '((1) (4)))
(check-expect (unzip '(("a" a))) '(("a") (a)))
(check-expect (unzip '(("a" -2) (b 3.4) (h -6.5))) '(("a" b h) (-2 3.4 -6.5)))


;; ==== Question 2e ========================

;; (dedup lon) consumes a list of numbers (lon) and produces a new list
;;   with only the first occurence of each element of the original list
;; dedup: (listof Num) -> (listof Num)
;; Examples:
(check-expect (dedup '(1 2 1 3 3 2 4 5)) '(1 2 3 4 5))

(define (dedup lon)
  (foldl (lambda (fst rr) (cons fst rr)) empty
         (foldl (lambda (fst rr)
             (cond [(empty? (filter (lambda (sub-fst) (= fst sub-fst)) rr))
                    (cons fst rr)]
                   [else rr]))
                empty lon)))

;;Tests:
(check-expect (dedup '(1 1 1 1 3 3 4 5 6)) '(1 3 4 5 6))
(check-expect (dedup '(6 4 5 3 1 2 3)) '(6 4 5 3 1 2))
(check-expect (dedup '()) '())
(check-expect (dedup '(3 2 1 -3.4 8 -9)) '(3 2 1 -3.4 8 -9))
(check-expect (dedup '(3 1 2 3 4 2 1 -3.4 8 8 -9)) '(3 1 2 4 -3.4 8 -9))
(check-expect (dedup '(2 2 2 2 2 2 2 2)) '(2))


;; ==== Question 2f ========================

;; (subsequence lst from to) consumes a list (lst) and 2 natural numbers
;;    (from) and (to), and produces the subsequence from lst that
;;    begins at index from and ends just before index to
;; subsequence: (listof Any) Nat Nat -> (listof Any)
;; requires: indexing for from and to starts at 0
;; Examples:
(check-expect (subsequence '(a b c d e f g j) 1 4) '(b c d))
(check-expect (subsequence '(a b c d e f g j) 1 1) '())
(check-expect (subsequence '(a b c f d) 0 400) '(a b c f d))

(define (subsequence lst from to)
  (foldl (lambda (fst rr)
           (cond
             [(<= (- to from) (length rr)) rr]
             [else (cons fst rr)])) empty
                                  (foldl (lambda (sub-fst sub-rr)
                                           (cond
                                             [(<= to from) empty]
                                             [(<= to (length sub-rr)) sub-rr]
                                             [else (cons sub-fst sub-rr)]))
                                         empty lst)))

;;Tests:
(check-expect (subsequence '(1 2 3 4) 1 3) '(2 3))
(check-expect (subsequence '() 1 1) '())
(check-expect (subsequence '(1 4 6 3 5 2) 3 5) '(3 5))
(check-expect (subsequence '(1 4 6 3 5 2) 5 5) '())
(check-expect (subsequence '(1 4 6 3 5 2) 5 3) '())
(check-expect (subsequence '(1 4 6 9 5 2) 3 5) '(9 5))
(check-expect (subsequence '() 6 3) '())
(check-expect (subsequence '(1 2 3) 0 0) '())