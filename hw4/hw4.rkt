
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

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number") ]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs)))) ]))

(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (let ([pr (stream)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))
  
;(define (stream-maker fn arg)
;  (letrec ([f (lambda (x)
;(cons x (lambda () (f (fn x arg)))))])
;(lambda () (f arg))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (cond [(= (remainder x 5) 0)  (- 0 x)]
                                      [#t x])
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;(define dan-then-dog
;  (letrec ([f (lambda (x) (cons (cond [(= (remainder x 2) 1)  "dan.jpg"]
;                                      [#t "dog.jpg"])
;                                (lambda () (f (+ x 1)))))])
;    (lambda () (f 1))))

; a better way
(define dan-then-dog
  (letrec ([f (lambda () (cons "dan.jpg" (lambda() (g))))]
           [g (lambda () (cons "dog.jpg" (lambda() (f))))])
    (lambda() (f))))

(define (stream-add-zero stream)
  (let ([pr (stream)])
    (lambda() (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))


  



  


