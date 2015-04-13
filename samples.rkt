#lang racket
;
; These are some sample functions I've written for CS550 - Programming Languages @ Drexel
;
; Date - 4/13/2015

; Added these boolean globals to provide switches for various items

(provide (all-defined-out))

;;
;; Problem 1
;;
; Took 5 minutes
;

; Computes the sum from m to n

(define (sigma m n)
   ; Handle the scenerio where m>n, in which case the function call is invalid.
  (cond 
    ((> m n) ((raise-user-error "Malformed function call, m must be less than n"))))
  (cond 
    ((eq? m n) n)
      (else (+ (sigma (+ m 1) n) m))))

;;
;; Problem 2
;;

; I had to define some global state for my implementation of log, this is it.
(define l 0)

(define (log m n)
  (cond
    ((> (expt m (+ 1 l)) n) 
     (define z l)             ; Grab the global state before resetting it
     (begin (set! l 0))       ; If we found the solution, we need to reset the global state for the next time log is called
     z)
    (else (begin (set! l (+ l 1)))
          (log m n))))

;;
;; Problem 3
;;

(define (choose n k)
  (cond 
    [(= 1 k) n]
    [(= n k) 1]
    (else (+ (choose (- n 1) (- k 1)) (choose (- n 1) k)))))
        

;;
;; Problem 4
;;

(define (binary n)
  (define binaryNumber 0)
  (define factor 1)
  (define (helper_function n factor binaryNumber)
    (cond 
      [(= 0 n) binaryNumber]
      [(>= n 1) (begin (set! binaryNumber (+ binaryNumber (* (modulo n 2) factor))))
                (begin (set! factor (* factor 10)))
                (helper_function (floor (/ n 2)) factor binaryNumber)]))
  (helper_function n factor binaryNumber))
  
  

;;
;; Problem 5
;;

(define (scan f z l)
  (define new-l (cons z l))
  (define (scan-helper f z new-l)
    (if (null? new-l)
        '()
        (cons (f z (car new-l)) (scan-helper f (f z (car new-l)) (cdr new-l)))))
  (scan-helper f z new-l))

;;
;; Problem 6
;;

(define (stream-scan f z s)
  (define new-stream (stream-cons z s))
  (define (scan-helper f z new-stream)
    (if (eq? null new-stream)
        empty-stream
        (stream-cons (f z (stream-first new-stream)) (scan-helper f (f z (stream-first new-stream)) (stream-rest new-stream)))))
  (scan-helper f z new-stream))


;;
;; Problem 7
;;

(define (stream-take-n n s)
  (if (or (eq? '() s) (= n 0))
      '()
      (cons (stream-first s)
            (stream-take-n (- n 1) (stream-rest s)))))

;;
;; Problem 8
;;

(define (stream-pair-with f s)
  (if (eq? '() s)
      empty-stream
      (stream-cons (cons (stream-first s) (f (stream-first s))) (stream-pair-with f (stream-rest s)))))


;;
;; Problem 9
;;
                   

(define (cycle-lists xs ys)
  (define (helper lst1 lst2)
    (cond ((null? lst1) (helper xs lst2))
          ((null? lst2) (helper lst1 ys))
          (else
           (stream-cons (cons (car lst1) (car lst2)) (helper (cdr lst1) (cdr lst2))))))
    (helper xs ys))
    

;;
;; Problem 10
;;
;; Struggled a bit with seen, probably spent a few hours total on this.

(define seen
  (let ((xs null))
    (lambda (x)
      (define (search-list x xs)
      ;(display "In search list, called with x=") (display x) (display ", xs=") (display xs) (display (newline))
      (if (null? xs)
          #f
          (if (eq? (car xs) x)
              #t
              (search-list x (cdr xs)))))
      (define result (search-list x xs))
      (cond ((eq? result #f) (set! xs (cons x xs))))
      result))
    
  )

; My test cases
;
; (sigma m n) test cases

(sigma 2 2)
(sigma 2 8)
(sigma 0 1)
(sigma 0 5)
(sigma 5 1)

; End of sigma tests

; (log m n)  test cases
(log 2 4)
(log 2 16)
(log 2 40)

(binary 2)
(binary 4)
(binary 6)

(scan + 0 null)
(scan + 0 '(1 2 3 4 5 6))
(scan * 1 '(1 2 3 4 5 6))

(stream-take-n 7 (stream-scan + 0 '(1 2 3 4 5 6)))
(stream-take-n 7 (stream-scan * 1 '(1 2 3 4 5 6)))
(stream-take-n 2 (stream 1 2 3 4 5 6))
(stream-take-n 3 (stream 1 2 3 4 5 6))

(stream-take-n 6 (stream-pair-with (lambda (x) (+ x 1)) (stream 1 2 3 4 5 6)))
(stream-take-n 6 (stream-pair-with (lambda (x) (* x 5)) (stream 1 2 3 4 5 6)))

(choose 12 12)
(choose 12 2)
(choose 121 3)
(choose 0 0)

(stream-take-n 30 (cycle-lists (list 1 2 3 4) (list 7 8 9)))
(stream-take-n 8 (cycle-lists '(1 2 3) '("a" "b")))

(begin (seen 5) (seen 5))
(begin (seen 5) (seen 10))

(seen 4)
(seen 1)
(seen 5)
(seen 4)
(seen 5)
(seen 1)
(stream->list (stream-pair-with (lambda (x) (+ x 1)) '(1 2 3 4)))
