
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

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n) )
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([helper (lambda (n) (cond [(= n (vector-length vec)) #f]
                                     [(and (pair? (vector-ref vec n)) (equal? v (car (vector-ref vec n)))  (vector-ref vec n) )]
                                     [#t (helper (+ n 1))]))])
    (helper 0)))



(define (cached-assoc xs n)
  (letrec( [memo (make-vector n (cons #f #f))]
           [nth 0]
           [f (lambda (v)
                (let ([ans (assoc v (vector->list memo))])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              ;(println memo)
                              (vector-set! memo (remainder nth n) new-ans)
                              (set! nth (+ nth 1))
                              ;(println "cache miss")
                              ;(println  ans)
                              ;(println new-ans)
                              ;(println memo)
                              ;(println nth)
                              new-ans)
                            new-ans)))))])
    f))


  
