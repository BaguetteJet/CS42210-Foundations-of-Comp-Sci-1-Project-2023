; link here: [REDACTED]
; Student ID: 23358459
; Code finds the mean, range, minimum, maximum values of student results from
; a list of results and the percentage of students that passed the exam
#lang racket
; Function to analyze results
(define (analysis results)
  ; Filter numbers from 0 to 100
  (set! results (filter (lambda (x) (and (number? x) (>= x 0) (<= x 100))) results))
  
  ; Calculate amount of students
  (define total-students (length results))
  
  ; Calculate mean
  (define mean (/ (apply + results) total-students))

  ; Calculate minimum and maximum
  (define minimum (apply min results))
  (define maximum (apply max results))

  ; Calculate range
  (define range (- maximum minimum))

  ; Initialize a counter for the number of students who scored >= 30%
  (define passed-students (for/sum ((score results)) (if (>= score 30) 1 0)))
  
  (define pass-percentage
    ; Calculate the percentage of students who scored >= 30%
    (if (= total-students 0)
        0 ; Handle the case where the list is empty to avoid division by zero
        (/ (* 100 passed-students) total-students)))

  ; Display Mean
  (display "Mean: ")
  (display (exact->inexact mean))
  (displayln "%")

  ; Display Range
  (display "Range: ")
  (display (exact->inexact range))
  (displayln "%")

  ; Display Minimum
  (display "Minimum: ")
  (display (exact->inexact minimum))
  (displayln "%")

  ; Display Maximum 
  (display "Maximum: ")
  (display (exact->inexact maximum))
  (displayln "%")

  ; Display Students Passed
  (display "Students Passed: ")
  (display (exact->inexact passed-students))
  (display "/")
  (displayln (exact->inexact total-students))

  ; Display Pass Percentage
  (display "Pass Percentage: ")
  (display (exact->inexact pass-percentage))
  (displayln "%"))

; Example usage
(define exam-results '(10 20 30 40 50 60 70 80 90 "six" -88 123 3.14)) ; malformed data
(analysis exam-results)
