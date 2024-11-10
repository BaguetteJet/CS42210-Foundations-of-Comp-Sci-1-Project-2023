#lang racket
; Define a function to calculate the percentage of students who scored >= 30%
(define (pass-percentage results)
  ; Initialize a counter for the number of students who scored >= 30%
  (define passed-students (for/sum ((score results)) (if (>= score 30) 1 0)))

  ; Calculate the total number of students
  (define total-students (length results))

  ; Calculate the percentage of students who scored >= 30%
  (if (= total-students 0)
      0 ; Handle the case where the list is empty to avoid division by zero
      (/ (* 100 passed-students) total-students)))

; Example usage:
(displayln (pass-percentage '(40 25 35 50 20))) ; Output: 60
