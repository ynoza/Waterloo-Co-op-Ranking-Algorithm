;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ranking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 08, Problem 4
;; ***************************************************
;;

(require "provide.rkt")
(provide find-pref find unfold expunge find)

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
    ("Jack" (("Apple" 1)))))

(define flat-the-employers '(("Manulife" "Anna" 1)
                         ("Manulife" "Feisal" 2)
                         ("Manulife" "Zihan" 2)
                         ("Manulife" "Hannah" 2)
                         ("Google" "Anna" 1)
                         ("Google" "Feisal" 2)
                         ("Google" "Rafelia" 2)
                         ("Apple" "Rafelia" 1)
                         ("Apple" "Zihan" 2)
                         ("Apple" "Feisal" 3)
                         ("Apple" "Jack" 4)))

(define flat-the-students '(("Anna" "Google" 1)
                       ("Anna" "Manulife" 2)
                       ("Feisal" "Google" 1)
                       ("Feisal" "Apple" 1)
                       ("Feisal" "Manulife" 1)
                       ("Rafelia" "Apple" 1)
                       ("Rafelia" "Google" 2)
                       ("Zihan" "Apple" 1)
                       ("Zihan" "Manulife" 1)
                       ("Hannah" "Manulife" 1)
                       ("Jack" "Apple" 1)))


;; ==== Question 4a ========================

;; (expunge flat-ranking-lst ranking-id ranked-id) consumes a list of FlatRanking
;;    (flat-ranking-lst), a ranking ID (ranking-id), and a ranked ID (ranked-id),
;;    and produces the same list of FlatRanking, but without the elements containing
;;    the provided ranked ID
;; expunge: (listof FlatRanking) Id Id -> (listof FlatRanking)
;; requires: StdIds and EmpIds are unique
;; Examples:
(check-expect (expunge flat-the-employers "Manulife" "Rafelia")
              '(("Google" "Anna" 1) ("Google" "Feisal" 2)
                                    ("Apple" "Zihan" 2) ("Apple" "Feisal" 3)
                                    ("Apple" "Jack" 4)))

(define (expunge flat-ranking-lst ranking-id ranked-id)
  (filter (lambda (fst)
            (cond
              [(empty? flat-ranking-lst) false]
              [(or (string=? ranking-id (first fst))
                   (string=? ranked-id (second fst))) false]
              [else true])) flat-ranking-lst))

;;Tests:
(check-expect (expunge empty "Manulife" "Rafelia")
              '())
(check-expect (expunge flat-the-employers "Lia" "Rafelia")
              '(("Manulife" "Anna" 1)
                ("Manulife" "Feisal" 2)
                ("Manulife" "Zihan" 2)
                ("Manulife" "Hannah" 2)
                ("Google" "Anna" 1)
                ("Google" "Feisal" 2)
                ("Apple" "Zihan" 2)
                ("Apple" "Feisal" 3)
                ("Apple" "Jack" 4)))
(check-expect (expunge flat-the-employers "Lia" "James")
              '(("Manulife" "Anna" 1)
                ("Manulife" "Feisal" 2)
                ("Manulife" "Zihan" 2)
                ("Manulife" "Hannah" 2)
                ("Google" "Anna" 1)
                ("Google" "Feisal" 2)
                ("Google" "Rafelia" 2)
                ("Apple" "Rafelia" 1)
                ("Apple" "Zihan" 2)
                ("Apple" "Feisal" 3)
                ("Apple" "Jack" 4)))
(check-expect (expunge flat-the-employers "Google" "Rafelia")
              '(("Manulife" "Anna" 1)
                ("Manulife" "Feisal" 2)
                ("Manulife" "Zihan" 2)
                ("Manulife" "Hannah" 2)
                ("Apple" "Zihan" 2)
                ("Apple" "Feisal" 3)
                ("Apple" "Jack" 4)))
(check-expect (expunge flat-the-students "Lia" "James")
              '(("Anna" "Google" 1)
                ("Anna" "Manulife" 2)
                ("Feisal" "Google" 1)
                ("Feisal" "Apple" 1)
                ("Feisal" "Manulife" 1)
                ("Rafelia" "Apple" 1)
                ("Rafelia" "Google" 2)
                ("Zihan" "Apple" 1)
                ("Zihan" "Manulife" 1)
                ("Hannah" "Manulife" 1)
                ("Jack" "Apple" 1)))
(check-expect (expunge flat-the-students "Anna" "James")
              '(("Feisal" "Google" 1)
                ("Feisal" "Apple" 1)
                ("Feisal" "Manulife" 1)
                ("Rafelia" "Apple" 1)
                ("Rafelia" "Google" 2)
                ("Zihan" "Apple" 1)
                ("Zihan" "Manulife" 1)
                ("Hannah" "Manulife" 1)
                ("Jack" "Apple" 1)))
(check-expect (expunge flat-the-students "Lia" "Google")
              '(("Anna" "Manulife" 2)
                ("Feisal" "Apple" 1)
                ("Feisal" "Manulife" 1)
                ("Rafelia" "Apple" 1)
                ("Zihan" "Apple" 1)
                ("Zihan" "Manulife" 1)
                ("Hannah" "Manulife" 1)
                ("Jack" "Apple" 1)))
(check-expect (expunge flat-the-students "Zihan" "Apple")
              '(("Anna" "Google" 1)
                ("Anna" "Manulife" 2)
                ("Feisal" "Google" 1)
                ("Feisal" "Manulife" 1)
                ("Rafelia" "Google" 2)
                ("Hannah" "Manulife" 1)))
(check-expect (expunge flat-the-students "Anna" "")
              '(("Feisal" "Google" 1)
                ("Feisal" "Apple" 1)
                ("Feisal" "Manulife" 1)
                ("Rafelia" "Apple" 1)
                ("Rafelia" "Google" 2)
                ("Zihan" "Apple" 1)
                ("Zihan" "Manulife" 1)
                ("Hannah" "Manulife" 1)
                ("Jack" "Apple" 1)))

;; ==== Question 4b ========================

;; (ranking-sort flat-ranking-lst) consumes a list of FlatRanking (flat-ranking-lst)
;;     and produces the same data but reordered so that the ranking IDs are in
;;     alphabetical order (if the first element int he list of FlatRanking are equal,
;;     then it is ordered alphabetically by the second element
;; ranking-sort: (listof FlatRanking) -> (listof FlatRanking)
;; Examples:
(check-expect (ranking-sort
               '(("A" "c" 123) ("B" "a" 456) ("A" "a" 789)))
              '(("A" "a" 789) ("A" "c" 123) ("B" "a" 456)))

(define (ranking-sort lst)
(foldr (lambda (fst rr)
         (cond  [(empty? rr) (cons fst rr)]
                [(string=? (first (first rr)) (first fst))
                 (foldr
                  (lambda  (a b)
                    (cond
                      [(empty? b)
                       (cons a (filter (lambda (v) (not (string=? (first fst) (first v)))) rr))]
                      [else (cons a b)]))
                  empty
                  (quicksort (filter (lambda (q) (string=? (first fst) (first q))) (cons fst rr))
                             (lambda (x y) (string<? (second x) (second y)))))]
                [else (cons fst rr)]))
       empty (quicksort lst (lambda (x y) (string<? (first x) (first y))))))

;;Test:
(check-expect (ranking-sort '()) '())
(check-expect (ranking-sort
               '(("A" "c" 123) ("B" "a" 456) ("A" "a" 789) ("A" "d" 456)))
              '(("A" "a" 789) ("A" "c" 123) ("A" "d" 456) ("B" "a" 456)))
(check-expect (ranking-sort
               '(("B" "c" 123) ("B" "a" 456) ("B" "f" 789) ("A" "d" 456)))
              '(("A" "d" 456) ("B" "a" 456) ("B" "c" 123) ("B" "f" 789) ))
(check-expect (ranking-sort
               '(("B" "c" 123) ("G" "a" 456) ("H" "f" 789) ("A" "d" 456)))
              '(("A" "d" 456) ("B" "c" 123) ("G" "a" 456)  ("H" "f" 789)))
(check-expect (ranking-sort
               '(("X" "c" 123) ("Y" "a" 456)))
               '(("X" "c" 123) ("Y" "a" 456)))

;; ==== Question 4c ========================

;; (unfold ranking-lst) consumes a list of Ranking (ranking-lst) and produces an
;;    equivalent list of FlatRanking
;; unfold: (listof Ranking) -> (listof FlatRanking)
;; Examples:
(check-expect (unfold the-employers) flat-the-employers)

(define (unfold ranking-lst)
  (foldr (lambda (fst rr)
         (append (map (lambda (x) (cons (first fst) x)) (second fst)) rr))
        empty ranking-lst))

;;Tests:
(check-expect (unfold the-students) flat-the-students)
(check-expect (unfold empty) empty)
(check-expect (unfold '(("Manulife" (("Anna" 1) ("Feisal" 2)))))
              '(("Manulife" "Anna" 1) ("Manulife" "Feisal" 2)))
(check-expect (unfold '(("Manulife" (("Feisal" 2)))))
              '(("Manulife" "Feisal" 2)))
(check-expect (unfold '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 2)))
                        ("Google" (("Anna" 1)))))
              '(("Manulife" "Anna" 1)
                ("Manulife" "Feisal" 2)
                ("Manulife" "Zihan" 2)
                ("Google" "Anna" 1)))


;; ==== Question 4d ========================

;; (find key lst-of-lists) consumes a key and a list of lists (lst-of-lists),
;;    and produces the first element within the list that has a first element
;;    equal to the key; false if there is no such element
;; find: Any (listof (listof Any)) -> (anyof (listof Any) false)
;; Examples:
(check-expect (find "A" '(("B" 1 2 3) ("A" a) ("C"))) '("A" a))
(check-expect (find "D" '(("B" 1 2 3) ("A" X) ("C"))) false)
(check-expect (find 3 '((1 ’a ’b ’c) (2 Y) (3 "X"))) '(3 "X"))

(define (find key lst-of-lists)
  (cond
    [(empty?
      (foldr (lambda (fst rr)
               (cond
                 [(empty? fst) rr]
                 [(equal? (first fst) key) (cons fst rr)]
                 [else rr])) empty lst-of-lists))
     false]
    [else
     (first (foldr (lambda (fst rr)
                     (cond
                       [(empty? fst) rr]
                       [(equal? (first fst) key) (cons fst rr)]
                       [else rr])) empty lst-of-lists))]))

;;Tests:
(check-expect (find 3 '(())) false)
(check-expect (find "A" '(("B" 1 2 3) () ("C"))) false)
(check-expect (find 'a '(() () () () (a))) '(a))
(check-expect (find 'a '((a 8) (a "cool") (b g) (c f) (a g))) '(a 8))


;; ==== Question 4e ========================

;; (find-pref ranking-lst ranking-id ranked-id) consumes a list of Ranking
;;   (ranking-lst), a ranking ID (ranking-id), and a ranked ID (ranked-id) 
;;   and produces the preference of the ranking ID for the ranked ID
;; find-pref: (listof Ranking) Id Id -> Pref
;; requires: the preference must exists within ranking-lst
;; Examples:
(check-expect (find-pref the-students "Rafelia" "Google") 2)
(check-expect (find-pref the-employers "Apple" "Zihan") 2)

(define (find-pref ranking-lst ranking-id ranked-id)
  (second (first (filter (lambda (fst)
                           (string=? (first fst) ranked-id))
                         (second (first (filter (lambda (x)
                                                  (string=? (first x) ranking-id))
                                                ranking-lst)))))))


;;Tests:
(check-expect (find-pref the-employers "Apple" "Feisal") 3)
(check-expect (find-pref the-employers "Google" "Anna") 1)
