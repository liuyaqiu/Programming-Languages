#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;;Problem 1
(define (sequence low high stride)
                   (if (<= low high)
                       (cons low (sequence (+ low stride) high stride))
                       null))

;;Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;Problem 3
(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (let ([i (remainder n (length xs))])
          (car (list-tail xs i)))]))

;;Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([cur (s)])
        (cons (car cur) (stream-for-n-steps (cdr cur) (- n 1))))))

;;Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;;Problem 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

;;Problem 7
(define (stream-add-zero s)
  (lambda ()
    (let ([cur (s)])
      (cons (cons 0 (car cur)) (stream-add-zero (cdr cur))))))

;;Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (i) (cons (cons (list-nth-mod xs i) (list-nth-mod ys i))
                (lambda () (f (+ i 1)))))])
    (lambda () (f 0))))

;;Problem 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (i) (cond [(>= i len) #f]
                                [#t (let ([cur (vector-ref vec i)])
                                   (if (and (pair? cur) (equal? v (car cur)))
                                       cur
                                       (f (+ i 1))))]))])
    (f 0)))

;;Problem 10
(define (cached-assoc xs n)
  (letrec([cache (make-vector n #f)]
          [pos 0]
          [f (lambda (x)
               (let ([ans (vector-assoc x cache)])
                 (if ans
                     ans
                     (let ([new-ans (assoc x xs)])
                       (if new-ans
                           (begin (vector-set! cache pos new-ans)
                                  (set! pos (remainder (+ pos 1) n))
                                  new-ans)
                           #f)))))])
    f))

      
