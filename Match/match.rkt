;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 08, Problem 5
;; ***************************************************
;;

(require "provide.rkt")
(require "a08helpers.rkt")
(require "ranking.rkt")
(provide condensed-lst valid-key?)

(define the-employers
  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 2) ("Hannah" 2)))
    ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 2)))
    ("Apple" (("Rafelia" 1) ("Zihan" 2) ("Feisal" 3) ("Jack" 4)))))

(define the-students
  '(("Anna" (("Google" 1) ("Manulife" 2)))
    ("Feisal" (("Google" 1) ("Apple" 1) ("Manulife" 1)))
    ("Rafelia" (("Apple" 1) ("Google" 2)))
    ("Zihan" (("Apple" 1) ("Manulife" 1)))
    ("Hannah" (("Manulife" 1)))
    ("Jack" 1)))


;; (my-append lst1 lst2) consumes 2 lists (lst1) and (lst2) and joins them
;;    together
;; my-append (listof Any) (listof Any) -> (listof Any)
;; Examples:
(check-expect (my-append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (my-append '() '()) '())

(define (my-append lst1 lst2)
  (foldr (lambda (x acc) (cons x acc)) lst2 lst1))

;;Tests:
(check-expect (my-append '(1 3) '(2 4 5)) '(1 3 2 4 5))
(check-expect (my-append '() '(1 2)) '(1 2))
(check-expect (my-append '(1 2) '()) '(1 2))


;; (condensed-lst employers students) consumes a list of EmpRanking (employers)
;;    and a list of StdRanking (students) and produces a list of lists with the
;;    sum of the Prefs for employers, students, and a random number between 0 
;;    and 1 (to break ties called random-eplison)
;; condensed-lst: (listof EmpRanking) (listof StdRanking)
;;                                         -> (listof (listof EmpId StdId Num)))
;; Examples:
(check-within (condensed-lst
               '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 2) ("Hannah" 2)))
                 ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 2)))
                 ("Apple" (("Rafelia" 1) ("Zihan" 2) ("Feisal" 3) ("Jack" 4))))
               '(("Anna" (("Google" 1) ("Manulife" 2)))
                 ("Feisal" (("Google" 1) ("Apple" 1) ("Manulife" 1)))
                 ("Rafelia" (("Apple" 1) ("Google" 2)))
                 ("Zihan" (("Apple" 1) ("Manulife" 1)))
                 ("Hannah" (("Manulife" 1)))
                 ("Jack" (("Apple" 1)))))
              (list  (list "Manulife" "Anna" 3.31)
                     (list "Manulife" "Feisal" 3.31)
                     (list "Manulife" "Zihan" 3.31)
                     (list "Manulife" "Hannah" 3.31)
                     (list "Google" "Anna" 2.26)
                     (list "Google" "Feisal" 3.26)
                     (list "Google" "Rafelia" 4.26)
                     (list "Apple" "Rafelia" 2.26)
                     (list "Apple" "Zihan" 3.26)
                     (list "Apple" "Feisal" 4.26)
                     (list "Apple" "Jack" 5.26)) 0.01)
(check-expect (condensed-lst empty empty) empty)

(define (condensed-lst employers students)
  (foldr (lambda (fst rr)
           (my-append
            (map (lambda (f)
                   (cons (first fst)
                         (cons (first f)
                               (cons
                                (+ (random-epsilon (first fst) (first f))
                                   (second f)
                                   (find-pref students (first f) (first fst)))
                                     empty))))
                 (second fst)) rr))
         empty employers))


;; (valid-key? key lst-of-lists) is a predicate that consumes a StdID (key)
;;    and a list of lists (lst-of-lists), and produces false if the StdID given
;;    exists in a list of FlatRanking
;; valid-key?: StdID FlatRanking -> Bool
;; Examples:
(check-expect (valid-key? "Anna" '(("Manulife" "Anna" 1))) false)
(check-expect (valid-key? "James" '(("Manulife" "Anna" 1))) true)

(define (valid-key? key lst-of-lists)
     (empty?
      (foldr (lambda (fst rr)
               (cond
                 [(empty? fst) rr]
                 [(string=? (second fst) key) (cons fst rr)]
                 [else rr])) empty lst-of-lists)))


;; (match employers students) consumes a list of EmpRanking (employers) and list
;;    of StdRanking (students) and produces the final matches by taking the min
;;    of the ranking that the employers and students gave to each other
;; match: (listof EmpRanking) (listof StdRanking) -> (listof (list EmpId StdId))
;; requires: every student ranked in employers is present in students...
;;           with that student tanking he same employer
;;Examples:
(check-expect (match
                  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Hannah" 2)))
                    ("Google" (("Anna" 1) ("Rafelia" 2)))
                    ("Apple" (("Rafelia" 1) ("Feisal" 3))))
                '(("Anna" (("Google" 1) ("Manulife" 2)))
                  ("Feisal" (("Apple" 1) ("Manulife" 1)))
                  ("Rafelia" (("Apple" 1) ("Google" 2)))
                  ("Hannah" (("Manulife" 1)))))
'(("Manulife" "Hannah") ("Apple" "Rafelia") ("Google" "Anna")))

(define (match employers students)
    (map (lambda (x) (list (first x) (second x)))
           (foldl
            (lambda (fst rr)
              (cond
                [(empty? rr) (cons fst rr)]
                [(and (equal? (find (first fst) rr) false)
                      (valid-key? (second fst) rr))
                 (cons fst rr)]
                [else rr]))
            empty
            (quicksort (condensed-lst employers students)
                       (lambda (x y) (< (third x) (third y)))))))


;;Tests:
(check-expect (match empty empty) empty)
(check-expect (match
                  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Hannah" 2)))
                    ("Google" (("Anna" 1) ("Rafelia" 2)))
                    ("Apple" (("Rafelia" 1) ("Feisal" 3))))
                '(("Anna" (("Google" 1) ("Manulife" 2)))
                  ("Feisal" (("Apple" 1) ("Manulife" 1)))
                  ("Rafelia" (("Apple" 1) ("Google" 2)))
                  ("Hannah" (("Manulife" 1)))))
'(("Manulife" "Hannah") ("Apple" "Rafelia") ("Google" "Anna")))
(check-expect (match
                  '(("Manulife" (("Hannah" 1) ("Anna" 2) ("Feisal" 2)))
                    ("Google" (("Anna" 1) ("Rafelia" 2)))
                    ("Apple" (("Feisal" 1))))
                '(("Anna" (("Google" 1) ("Manulife" 2)))
                  ("Feisal" (("Apple" 1) ("Manulife" 1)))
                  ("Rafelia" (("Google" 2)))
                  ("Hannah" (("Manulife" 1)))))
'(("Manulife" "Hannah") ("Apple" "Feisal") ("Google" "Anna")))
(check-expect (match
                  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 3)
                                            ("Hannah" 4)))
                    ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 3)))
                    ("Apple" (("Anna" 1) ("Zihan" 2) ("Feisal" 3))))
                '(("Anna" (("Google" 1) ("Apple" 2) ("Manulife" 3)))
                  ("Feisal" (("Google" 1) ("Apple" 2) ("Manulife" 3)))
                  ("Rafelia" (("Google" 1)))
                  ("Zihan" (("Apple" 1) ("Manulife" 2)))
                  ("Hannah" (("Manulife" 1)))))
              '(("Manulife" "Hannah") ("Apple" "Zihan") ("Google" "Anna")))
