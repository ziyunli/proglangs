#lang racket

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (mystery s)
  (lambda ()
     (let ([pr (s)])
        (if (car pr)
            (cons (car pr) (mystery (cdr pr)))
            ((mystery (cdr pr)))))))

(define longer-strings
  (lambda ()
    (letrec ([f (lambda(s)
                  (cons s (f (string-append "A" s))))])
      (f "A"))))