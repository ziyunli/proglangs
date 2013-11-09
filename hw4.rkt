#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride) 
  (if (> low high) 
      null
      (cons low (sequence (+ low stride) high stride))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (modulo x 5)) 
                              (cons (- 0 x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))
  
; 6
(define dan-then-dog 
  (let ([f (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog))))])
    (lambda () (f))))

; 7
(define (stream-add-zero s)
  (let* ([r (s)]
         [f (lambda () (cons (cons 0 (car r)) (stream-add-zero (cdr r))))])
    (lambda () (f))))
  
; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                              (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))
  
; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos) (cond [(not (< pos (vector-length vec))) #f]
                                  [(not (pair? (vector-ref vec pos))) (f (+ pos 1))]
                                  [(equal? v (car (vector-ref vec pos))) (vector-ref vec pos)]
                                  [#t (f (+ pos 1))]))])
    (f 0)))
  
; 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [curr 0]
           [f (lambda (v) 
                (let ([cached-ans (vector-assoc v cache)])
                  (if cached-ans
                      cached-ans
                      (let ([ans (assoc v xs)])
                        (begin (vector-set! cache curr ans)
                               (set! curr (remainder (+ curr 1) n))
                               ans)))))])
    f))
      
    

; 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([c1 e1]
              [c2 (lambda () e2)]
              [f (lambda (th) (if (> c1 (th)) (f th) #t))])
       (f c2))]))
           
       
       