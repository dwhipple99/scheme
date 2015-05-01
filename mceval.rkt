#lang racket
(require racket/mpair)
(require readline/readline)

;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm with a fair bit of added functionality

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;SECTION 4.1.1

(define (mceval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ;;
        ;;  Added these two special forms for and/or
        ;;
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ;; Added let special-form, which is basically syntactic sugar for a lambda
        ((let? exp) (mceval (let->combination exp) env))
        ;;
        ;;  Added delay, simply syntactic sugar for a lambda expression
        ;;
        ;;  This is my call by need
        ((delay? exp) (mceval (my-memoize-lambda exp) env))       
        ;;  This is my call by name
        ;((delay? exp) (mceval (my-lambda exp) env))
        ;;    
        ;; stream-cons has to be a special form
        ;;
        ((stream-cons? exp) (make-stream-cons exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mceval (cond->if exp) env))
        ((application? exp)
         (mcapply (mceval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (mcapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mceval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mceval (if-predicate exp) env))
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mceval (first-exp exps) env))
        (else (mceval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mceval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

;; Added eval-and
;;
;;  This evaluates via a special-form, and procedures
;;
(define (eval-and exp env)
  ;; Sit in a loop
  (define (eval-and-operands exps)
    (if (no-operands? exps)                     ;; If empty, just return true
        true
        (let ((first (mceval (first-operand exps) env))  ;; If only one expression, return value of first operand
              (rest (rest-operands exps)))      
          (if (false? first)
              false
              (if (no-operands? rest)
                  first
                  (eval-and-operands rest))))))
  (eval-and-operands (operands exp)))              ;; The loop
  
;;

;;  This is how I determine if I am seeing an "and"
;;
(define (and? exp) 
  (tagged-list? exp 'and))
  
;;  This is how I determine if I am seeing an "or"
;;
(define (or? exp) 
  (tagged-list? exp 'or))

;;
;;  This is how I determine if I am seeing a "stream-cons" 
;;
(define (stream-cons? exp) 
  (tagged-list? exp 'stream-cons))

;; These are two helper functions for evaluating first part and second part of expression
(define (stream-cons-a exp) (cadr exp))
(define (stream-cons-b exp env) (mceval (list 'delay (cddr exp)) env))

;; This is my stream-cons implementation
;;
(define (make-stream-cons exp env)
  (cons (stream-cons-a exp) (stream-cons-b exp env)))
  
;; Added eval-or
;;
;;  This evaluates via a special-form, "or" procedures
;;
(define (eval-or exp env)
  (define (eval-or-operands exps)
    (if (no-operands? exps)
        false
        (let ((first (mceval (first-operand exps) env))
              (rest (rest-operands exps)))
          (if (true? first)
              first
              (eval-or-operands rest)))))
  (eval-or-operands (operands exp)))


;;  Adding LET functionality as a special form
;;
(define (let? exp) (tagged-list? exp 'let))

(define (let-clauses exp) (cdr exp))
(define (let-parameters exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
(cons
(cons 'lambda
(cons (map car (let-parameters exp))
(let-body exp)))
(map cadr (let-parameters exp))))

(define (memo-proc proc)
   (let ((already-run? false) (result '()))
      (lambda ()
        (if (not already-run?)
             (begin (set! result (proc))
                    (set! already-run? true)
                     result)
              result))))
 

; Streams
; Stream cons must be special form
(define (stream-first? exp) (tagged-list? exp 'stream-first))
(define (stream-rest? exp) (tagged-list? exp 'stream-rest))
(define (stream-empty? exp) (tagged-list? exp 'stream-empty?))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

;;
;; This is my call by need or memoize as discussed in SICP (call by need)
;;
(define (my-memoize-lambda exp)              ; This is my call by need
      (list 'memo-proc (make-lambda null (cdr exp))))

;;
;; This is my call by need or memoize as discussed in SICP (call by name)
;;
(define (my-lambda exp)               ; This is my call by name
      (make-lambda null (cdr exp)))

;; This is how to identify delay
;;
(define (delay? exp)
  (tagged-list? exp 'delay))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    ;;
    ;; Implemented force by simply calling procedure produced by delay in mceval
    ;;
    (eval-definition '(define (force x) (x)) initial-env)
    ;;
    ;;  Added memo-proc to pre-evaluate procedures
    ;;  memo-proc is a procedure that takes a procedure, and returns a defined value and caches it.
    ;;  (if it has already called proc, it remembers it
    ;;
    (eval-definition '(define (memo-proc proc)
                        (let ((already-run? false) (result '()))
                          (lambda ()
                            (if (not already-run?)
                                (begin (set! result (proc))
                                       (set! already-run? true)
                                       result)
                                result)))) initial-env)
    ;;
    ;; Added empty-stream, stream-first, stream-rest, and stream-empty
    ;;
    (eval-definition '(define (empty-stream '()) initial-env))
    (eval-definition '(define (stream-first x) (car x)) initial-env)
    (eval-definition '(define (stream-rest x) (force (cdr x))) initial-env)
    (eval-definition '(define (stream-empty? x) (if (eq? x empty-stream)
                                                    true
                                                    false)) initial-env)
    (eval-definition '(define (not x) (if x false true)) initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


;;  These are the primitives
;;
;;  Their are 3 ways to add functionality to the interpretor 
;;   
;;    1.  Add a primitive (below)
;;    2.  Add a in setup-environment (above)
;;    3.  Add a special-form in mceval (above)
;;
;;
;;  Adding a primitive
;;    -- first add a primitive, then a racket procedure
;;
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'null? null?)
        ;(list 'exit (exit #t))
        ; I added list, just made it easier to test with
        (list 'list list)
        ;(list 'empty-stream '())
        (list '* *)
        (list '/ /)
        (list '+ +)
        (list '- -)
        (list '< <)
        (list '<= <=)
        (list '= =)
        (list '>= >=)
        (list '> >)
        (list 'eq? eq?)
        (list 'error (lambda () (error "Metacircular Interpreter Aborted")))
;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (top-mceval exp)
  (let ((val (mceval exp (setup-environment))))
    (user-print val)))

(define the-global-environment (setup-environment))

(define input-prompt "> ")

(define (driver-loop)
  (let ((input-string (readline input-prompt)))
    (if (eof-object? input-string)
        (newline)
        (begin
          (let ((input (read (open-input-string input-string))))
            (with-handlers
                ([exn:fail? (lambda (exn)
                              (display "Error: ")
                              (display (exn-message exn))
                              (newline))])
              (add-history input-string)
              (let ((output (mceval input the-global-environment)))
                (user-print output)
                (newline)))
            (driver-loop))))))

(define (main . argv)
  (driver-loop))

(provide mceval
         setup-environment
         main)
