(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) nil (cons (proc (car items)) (map proc (cdr items))))
  )

(define (cons-all first rests)
    (cond
    ((null? rests) nil)
    (else (cons
      (append (cons first nil) (car rests))
      (cons-all first (cdr rests)))))
  )


(define (zip pairs)
  (list (map car pairs) (map cadr pairs))
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define counter (length s))
  (define (helper s)
    (cond 
      ((null? s) 
          nil)
      (else 
        (cons
          (cons 
            (- counter (length s)) 
            (cons (car s) nil)) 
          (helper (cdr s)))
        )
      )
    )
  (helper s)
  )



  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((null? denoms)
      cons nil)
    ((= total 0) (cons nil nil))
    ((< total (car denoms)) (list-change total (cdr denoms)))
    ;; Removes first denoms as soon as is becomes greater than total
    (else 
      (append
        (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
        ;; Add the first term of denom to a list.
        ;; Then, call list-change, using total - denom as total, since denom in list already.
        ;; Once denom becomes bigger than total, it will be removed. 
        ;; Repeat process until total is zero. 
        (list-change total (cdr denoms))
        )
      )
    )
  )

  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr))) 
           ; BEGIN PROBLEM 19
           (cons form (cons params (map let-to-lambda body)))
           ;; Creates list with (form, params, x...)
           ;; x... being let-to-lambda mapped over each section of body
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons(cons 'lambda (cons(car (zip values)) (map let-to-lambda body))) (map let-to-lambda (cadr (zip values)))

            ;; (car (zip values)) is (a b) (1 2)
            
           ; END PROBLEM 19
           )))
        (else
          ; BEGIN PROBLEM 19
          (cons (car expr) (map let-to-lambda (cdr expr)))
          ;; Create list with first element of expr...
          ;; with let-to-lambda mapped over rest of expression
          )

          ; END PROBLEM 19
         )))
