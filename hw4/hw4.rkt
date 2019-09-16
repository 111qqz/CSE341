
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
    (if(<= low high)
       (cons low (sequence (+ low stride) high stride))
       null))

(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix))
       xs))
  


