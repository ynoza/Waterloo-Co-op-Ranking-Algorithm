;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;5

(require "a08helpers.rkt")

(define employers
  '(("Manulife" (("Anna" 1) ("Feisal" 2) ("Zihan" 2) ("Hannah" 2)))
    ("Google" (("Anna" 1) ("Feisal" 2) ("Rafelia" 2)))
    ("Apple" (("Rafelia" 1) ("Zihan" 2) ("Feisal" 3)))
    ))

(define students
  '(("Anna" (("Google" 1) ("Manulife" 2)))
    ("Feisal" (("Google" 1) ("Apple" 1) ("Manulife" 1)))
    ("Rafelia" (("Apple" 1) ("Google" 2)))
    ("Zihan" (("Apple" 1) ("Manulife" 1)))
    ("Hannah" (("Manulife" 1)))
    ))


(define (match employers students)
  (local
    [(define (find-pref ranking-lst ID1 ID2)
       (second (first
                (filter (lambda (fst)
                          (string=? (first fst) ID2))
                        (second (first
                                 (filter (lambda (fst)
                                           (string=? (first fst) ID1))
                                         ranking-lst)))))))]
  (map (lambda (fst)
       (map (lambda (f) (cons (first fst) (cons (first f)
               (+ (random-eplison (first fst) (first f))
                  (second f1) (find-pref students (first fst) (first f))))))
            (second fst)))
       employers)))

  (check-expect (match employers students)
'(("Manulife" "Zihan") ("Apple" "Rafelia") ("Google" "Anna")))
