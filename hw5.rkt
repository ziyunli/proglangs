;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]))

(define (mupllist->racketlist pairs)
  (cond [(aunit? pairs) null]
        [#t (cons (apair-e1 pairs) (mupllist->racketlist (apair-e2 pairs)))]))
        

;; CHANGE (put your solutions here)

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  ;; CHANGE add more cases here
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) (if (number? (int-num e)) 
                      e
                      (error "MUPL int applied to non-number"))]
        [(aunit? e) e]
        [(closure? e) e] 
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error (format "MUPL ifgreater applied to non-number: ~v; ~v" v1 v2))))]              
        [(mlet? e)
         (let ([name (mlet-var e)]
               [v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons name v) env)))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([cl (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cl)
               (let* ([fn (closure-fun cl)]
                      [bodyenv (cons (cons (fun-formal fn) arg)
                                     (closure-env cl))]
                      [bodyenv (if (fun-nameopt fn)
                                   (cons (cons (fun-nameopt fn) cl)
                                         bodyenv)
                                   bodyenv)])
                 (eval-under-env (fun-body fn) bodyenv))
             (error (format "MUPL call applied to non-closure: ~v" (call-funexp e)))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error (format "MUPL fst applied to non-pair: ~v; ~v" v))))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error (format "MUPL fst applied to non-pair: ~v; ~v" v))))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst) 
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))
               
(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1 
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4 e3)))))
                 
;; Problem 4

(define mupl-map
  (fun #f
       "__f"
       (fun "__internal_rec"
            "__xs"
            (ifaunit (var "__xs")
                     (aunit)
                     (apair (call (var "__f") (fst (var "__xs")))
                            (call (var "__internal_rec") (snd (var "__xs"))))))))
                      
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f
             "__i"
             (call (var "map") (fun #f "__j" 
                                    (add (var "__i") (var "__j"))))))) 

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([create-func (lambda (f vars)
                          (let* ([expand-body (compute-free-vars (fun-body f))]
                                 [large-vars (if (fun-challenge? expand-body)
                                                 (set-union vars
                                                            (fun-challenge-freevars expand-body))
                                                 vars)])
                            (fun-challenge (fun-nameopt f)
                                           (fun-formal f)
                                           expand-body
                                           (set-subtract large-vars 
                                                         (set (fun-formal f) (fun-nameopt f))))))]
           [find-vars (lambda (e acc) 
                        (cond [(var? e) (set-add acc (var-string e))]
                              [(add? e) (set-union (find-vars (add-e1 e) acc)
                                                   (find-vars (add-e2 e) acc))]
                              [(apair? e) (set-union (find-vars (apair-e1 e) acc)
                                                     (find-vars (apair-e2 e) acc))]
                              [(ifgreater? e) (set-union 
                                               (find-vars (ifgreater-e1 e) acc)
                                               (find-vars (ifgreater-e2 e) acc)
                                               (find-vars (ifgreater-e3 e) acc)
                                               (find-vars (ifgreater-e4 e) acc))]
                              [(mlet? e) (set-union 
                                          (find-vars (mlet-e e) acc)
                                          (let ([body-vars (find-vars (mlet-body e) acc)])
                                            (if (set-member? body-vars (mlet-var e))
                                                (set-remove body-vars (mlet-var e))
                                                body-vars)))]
                              [(call? e) (set-union
                                          (find-vars (call-funexp e) acc)
                                          (find-vars (call-actual e) acc))]
                              [(fst? e) (find-vars (fst-e e) acc)]
                              [(snd? e) (find-vars (snd-e e) acc)]
                              [(isaunit? e) (find-vars (isaunit-e e) acc)]
                              [(fun? e) (set-subtract (find-vars (fun-body e) acc)
                                                      (set (fun-formal e) (fun-nameopt e)))]
                              [#t acc]))])
    (cond [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e))
                             (compute-free-vars (apair-e2 e)))]
          [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                     (compute-free-vars (ifgreater-e2 e))
                                     (compute-free-vars (ifgreater-e3 e))
                                     (compute-free-vars (ifgreater-e4 e)))]
          [(mlet? e) (mlet (mlet-var e)
                           (compute-free-vars (mlet-e e))
                           (compute-free-vars (mlet-body e)))]
          [(call? e) (call (compute-free-vars (call-funexp e))
                           (compute-free-vars (call-actual e)))]
          [(fst? e) (fst (compute-free-vars (fst-e e)))]
          [(snd? e) (snd (compute-free-vars (snd-e e)))]
          [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
          [(fun? e) (create-func e (find-vars (fun-body e) (set)))]
          [#t e])))

   
;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) (if (number? (int-num e)) 
                      e
                      (error "MUPL int applied to non-number"))]
        [(aunit? e) e]
        [(closure? e) e] 
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error (format "MUPL ifgreater applied to non-number: ~v; ~v" v1 v2))))]              
        [(mlet? e)
         (let ([name (mlet-var e)]
               [v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons name v) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error (format "MUPL fst applied to non-pair: ~v; ~v" v))))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error (format "MUPL fst applied to non-pair: ~v; ~v" v))))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(fun-challenge? e)
         (let ([free-vars (set->list (fun-challenge-freevars e))])
           (closure (map (lambda (var-name)
                           (cons var-name (envlookup env var-name)))
                         free-vars)
                    e))]
        [(call? e)
         (if (closure? (eval-under-env-c (call-funexp e) env))
             (let* ([f-closure (eval-under-env-c (call-funexp e) env)]
                    [f-env (closure-env f-closure)]
                    [func (closure-fun f-closure)]
                    [f-name (fun-challenge-nameopt func)])
               (if f-name
                   (eval-under-env-c
                    (fun-challenge-body func) 
                    (cons (cons f-name f-closure) 
                          (cons (cons (fun-challenge-formal func) 
                                      (eval-under-env-c (call-actual e) env))
                                f-env)))
                   (eval-under-env-c
                    (fun-challenge-body func)
                    (cons (cons (fun-challenge-formal func)
                                (eval-under-env-c (call-actual e) env))
                          f-env))))
             (error (format "MUPL call applied to non-closure: ~v" (call-funexp e))))]
        [#t (error (format "bad MUPL expression: ~v" e))]))
  

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
