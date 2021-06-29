; aziza mankenova
; 2018400387
; compiling: yes
; complete: yes

#lang racket
(provide (all-defined-out))

;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))
;; given
(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
;; given
(define mse (lambda (x y) (mul (sub x y) (sub x y))))


(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;3.1
(define (get-value num-list)
                  (if (num? num-list) (num-value (eval num-list ns) )
                   (map (lambda (x) (num-value (eval x ns) )) num-list)))
;3.2
(define (get-grad num-list)
                  (if (num? num-list) (num-grad (eval num-list ns) )
                   (map (lambda (x) (num-grad (eval x ns) )) num-list)))
;4.1
(define (add . list)
    (num (apply + (get-value list)) (apply + (get-grad list)) ))
;4.2
(define (sub . list)
    (num (apply - (get-value list)) (apply - (get-grad list)) ))
;4.3
(define (mul . list)
    (num (apply * (get-value list))
         (apply + (map (lambda (x) (/  (* (get-grad x) (apply * (get-value list)) ) (get-value x)) ) list))) )
;5.1
(define (create-hash names values var)
    (make-hash (map (lambda (a b) (if (eq? a var) (cons a (num b 1.0)) (cons a (num b 0.0)) ))  names values)) )
;5.2
(define (parse hash expr )
     (cond
       [(null? expr) '()]
       [(list? expr) (cons (parse hash (car expr)) (parse hash (cdr expr)))]
       [(eqv? '+ expr) 'add]
       [(eqv? '* expr) 'mul]
       [(eqv? '- expr) 'sub]
       [(eqv? 'mse expr) 'mse]
       [(eqv? 'relu expr) 'relu]
       [(number? expr) (num expr 0.0)]
       [else (hash-ref hash expr)]
       ))
;5.3
(define (grad names values var expr )
       (get-grad (eval (parse (create-hash names values var) expr) ns)  ))
;5.4
(define (partial-grad names values vars expr )
       (map (lambda (x) (if (member x vars ) (grad names values x expr) 0.0)) names))
;5.5
(define (gradient-descent names values vars lr expr )
       (define partialgrad (partial-grad names values vars expr))
       (map (lambda (x y) (- x y)) values (map (lambda (x) (* lr x)) partialgrad ))  )
;5.6
(define (optimize names values vars lr k expr)
        (define gradientdescent (gradient-descent names values vars lr expr ))
        (if (> k 1)  (optimize names gradientdescent vars lr (- k 1) expr) (gradient-descent names values vars lr expr )))
