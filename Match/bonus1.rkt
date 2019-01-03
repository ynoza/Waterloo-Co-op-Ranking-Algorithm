;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Yash Oza (20770452)
;; CS 135 Fall 2018
;; Assignment 08, Problem 6-Bonus
;; ***************************************************
;;

(require "ranking.rkt")
(require "partition.rkt")

;; ==== Question 6a ========================

(define (match/acc emp-unmatched emp-matched students)
  (local
    [(define check-end
            (length (foldr (lambda (fst rr)
                     (cond
                       [(empty? rr) (cons (first fst) rr)]
                       [(cons? (filter (lambda (x) (string=? (first fst) x))  rr)) rr]
                       [else (cons (first fst) rr)]))
                   empty emp-unmatched)))
    (define (partition original-emp emp-unmatched emp-matched students)
       (local
         [(define (flat-find-pref ranking-lst ranking-id ranked-id)
            (third (first (filter (lambda (x)
                                    (and (string=? (first x) ranking-id)
                                         (string=? (second x) ranked-id)))
                                  ranking-lst))))
          (define (pref-stud list-type)
            (flat-find-pref students (second (first list-type))
                            (first (first list-type))))
          (define (remove-emp list-type)
            (expunge emp-matched ""
                     (second (first list-type))))
          (define (expunge-orig list-type)
            (filter (lambda (fst)
                      (not (and (string=? (first (first list-type)) (first fst))
                                (equal? (second (first list-type)) (second fst)))))
                    original-emp))]
     
         (cond
           [(empty? emp-matched)
            (partition (expunge-orig emp-unmatched) (rest emp-unmatched)
                            (cons (first emp-unmatched) emp-matched) students)]
           [(= check-end (length emp-matched))
            (foldr (lambda (fst rr) (cons (list (first fst) (second fst)) rr))
                   empty emp-matched)]
           [(empty? emp-unmatched)
            (partition original-emp original-emp emp-matched students)]
           [(and (empty? (filter (lambda (fst) (string=? (first (first emp-unmatched)) (first fst)))
                                 emp-matched))
                 (cons? (filter (lambda (fst) (equal? (second (first emp-unmatched)) (second fst)))
                                emp-matched)))
            (cond
              [(< (pref-stud emp-unmatched) (pref-stud emp-matched))
               (partition (expunge-orig emp-unmatched) (rest emp-unmatched)
                               (cons (first emp-unmatched) (remove-emp emp-unmatched))
                               students)]
              [else 
               (partition original-emp (rest emp-unmatched) emp-matched students)])]
           [(and (cons? (filter (lambda (fst) (string=? (first (first emp-unmatched)) (first fst)))
                                emp-matched))
                 (empty? (filter (lambda (fst) (equal? (second (first emp-unmatched)) (second fst)))
                                 emp-matched)))
            (partition original-emp (rest emp-unmatched) emp-matched students)]
           [(and (empty? (filter (lambda (fst) (string=? (first (first emp-unmatched)) (first fst)))
                                 emp-matched))
                 (empty? (filter (lambda (fst) (equal? (second (first emp-unmatched)) (second fst)))
                                 emp-matched)))
            (partition (expunge-orig emp-unmatched) (rest emp-unmatched)
                            (cons (first emp-unmatched) emp-matched) students)]
           [else
            (partition original-emp (rest emp-unmatched)
                            emp-matched students)])))]

    (partition emp-unmatched emp-unmatched emp-matched students)))

;; ==== Question 6b ========================

(define (match employers students)
    (match/acc (unfold employers) empty (unfold students)))

;;Tests:
(check-expect (match
                  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Hannah" 2)))
                    ("Google" (("Anna" 1) ("Rafelia" 2)))
                    ("Apple" (("Rafelia" 1) ("Feisal" 2))))
                '(("Anna" (("Google" 1) ("Manulife" 2)))
                  ("Feisal" (("Apple" 1) ("Manulife" 2)))
                  ("Rafelia" (("Apple" 1) ("Google" 2)))
                  ("Hannah" (("Manulife" 1)))))
'(("Manulife" "Feisal") ("Apple" "Rafelia") ("Google" "Anna")))
(check-expect (match
                  '(("Manulife" (("Hannah" 1) ("Anna" 2) ("Feisal" 2)))
                    ("Google" (("Anna" 1) ("Rafelia" 2)))
                    ("Apple" (("Feisal" 1))))
                '(("Anna" (("Google" 1) ("Manulife" 2)))
                  ("Feisal" (("Apple" 1) ("Manulife" 1)))
                  ("Rafelia" (("Google" 2)))
                  ("Hannah" (("Manulife" 1)))))
'(("Apple" "Feisal") ("Google" "Anna") ("Manulife" "Hannah")))
(check-expect (match
                  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 3) ("Hannah" 4)))
                    ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 3)))
                    ("Apple" (("Anna" 1) ("Zihan" 2) ("Feisal" 3))))
                '(("Anna" (("Google" 1) ("Apple" 2) ("Manulife" 3)))
                  ("Feisal" (("Google" 1) ("Apple" 2) ("Manulife" 3)))
                  ("Rafelia" (("Google" 1)))
                  ("Zihan" (("Apple" 1) ("Manulife" 2)))
                  ("Hannah" (("Manulife" 1)))))
              '(("Manulife" "Feisal") ("Apple" "Zihan") ("Google" "Anna"))) 
(check-expect (match
                  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 2) ("Hannah" 2)))
                    ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 2)))
                    ("Apple" (("Rafelia" 1) ("Zihan" 2) ("Feisal" 3))))
                '(("Anna" (("Google" 1) ("Manulife" 2)))
                  ("Feisal" (("Google" 1) ("Apple" 1) ("Manulife" 1)))
                  ("Rafelia" (("Apple" 1) ("Google" 2)))
                  ("Zihan" (("Apple" 1) ("Manulife" 1)))
                  ("Hannah" (("Manulife" 1)))))
              '(("Manulife" "Feisal") ("Apple" "Rafelia") ("Google" "Anna")))