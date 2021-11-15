;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;***** Environment Stuff *****
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? (caar env) 'global) (resolve-scope varName (cdar env)))
      (else (let ((answer (resolve-scope varName (car env))))
              (if (eq? answer #f)
                  (resolve varName (cdr env))
                  answer))))))
    
(define resolve-scope
  (lambda (varName scope)
    (cond
      ((null? scope) #f)
      ((eq? varName (caar scope)) (cadar scope))
      (else (resolve-scope varName (cdr scope))))))

(define empty-scope '())

;((a (lit-exp 2)) (b (var-exp a)))
(define construct-scoped-env
  (lambda (list-of-2-lists env)
    (if (null? list-of-2-lists)
        env
        (let* ((curr-2-list (car list-of-2-lists))
               (curr-var (car curr-2-list))
               (curr-exp-val (run-parsed-code (cadr curr-2-list) env))
               (curr-env-modified-for-scope (if (= 1 (length env)) (extend-env empty-scope env) env)) ;ensures that our env has a scope to update as its first element
               (new-env (update-env-with-scope-in-progress curr-env-modified-for-scope (list curr-var) (list curr-exp-val))))
          (construct-scoped-env (cdr list-of-2-lists) new-env)))))

(define update-env-with-scope-in-progress
  (lambda (env list-of-vars list-of-vals)
    (let* ((curr-scope (car env))
          (old-env (cdr env))
          (new-scope (extend-scope list-of-vars list-of-vals curr-scope)))
      (extend-env new-scope old-env))))

(define extend-scope
  (lambda (lo-vars lo-vals scope)
    (cond
      ((null? lo-vars) scope)
      (else (extend-scope (cdr lo-vars)
                          (cdr lo-vals)
                          (cons (list (car lo-vars) (car lo-vals)) scope))))))

(define extend-env
  (lambda (scope env)
    (cons scope env)))

(define pop-to-global
  (lambda (env)
    (if (eq? (caar env) 'global)
        env
        (pop-to-global (cdr env)))))
                          
(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

; ***** PARSERS *****
(define block-expression-parser
  (lambda (block-expression)
    (list 'block-exp (map no-parser (cdr block-expression)))))


(define remember-expression-parser
  (lambda (remember-expression)
    (cond
      ((eq? (car remember-expression) 'remember)
       (list 'remember-exp (cadr remember-expression) (no-parser (caddr remember-expression))))
      (else "Not a valid remember expression"))))

(define boolean-expression-parser
  (lambda (boolean-expression)
    (cond
      ((eq? (car boolean-expression) '<)
       (list 'less-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '<=)
       (list 'less-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>)
       (list 'greater-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>=)
       (list 'greater-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '==)
       (list 'equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '!=)
       (list 'not-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      (else "Not a valid boolean expression"))))
 
(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))

(define no-code-let-env-parser
  (lambda (list-of-2-lists)
    (map (lambda (2-list) (list (car 2-list) (no-parser (cadr 2-list)))) list-of-2-lists)))  

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'block)
       (block-expression-parser no-code))
      ((eq? (car no-code) 'remember)
       (remember-expression-parser no-code))
      ((eq? (car no-code) 'spit-out)
       (list 'spit-out-exp (no-parser (cadr no-code))))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'local-vars)
       (list 'let-exp
             (no-code-let-env-parser (cadr no-code))
             (no-parser (caddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****

(define run-parsed-block-expression
  (lambda (parsed-block-expression env)
    (cond
      ((null? parsed-block-expression) '())
      ((eq? (car parsed-block-expression) 'block-exp)
       (run-parsed-block-expression (cadr parsed-block-expression) env))
      (else
       (let* ((curr-block-exp (car parsed-block-expression))
              (curr-env env)
              (curr-result (run-parsed-code curr-block-exp curr-env))
              (new-env curr-result))
         (cons (if (eq? (car curr-block-exp) 'spit-out-exp) curr-result 0)
               (run-parsed-block-expression (cdr parsed-block-expression)
                                            (if (eq? (car curr-block-exp) 'remember-exp) new-env curr-env))))))))

(define clean-block-exp
  (lambda (block-results)
    (cond
      ((null? block-results) '())
      ((eq? (car block-results) 0)
       (clean-block-exp (cdr block-results)))
      (else
       (cons (car block-results) (clean-block-exp (cdr block-results)))))))
      

(define run-parsed-remember-expression
  (lambda (parsed-remember-expression env)
    (cond
      ((eq? (car parsed-remember-expression) 'remember-exp)
       (let* ((var-name (cadr parsed-remember-expression))
             (var-value (run-parsed-code (caddr parsed-remember-expression) env))
             (new-env (update-env-with-scope-in-progress env (list var-name) (list var-value))))
         new-env)))))

(define run-parsed-boolean-code
  (lambda (parsed-boolean-code env)
    (cond
      ((eq? (car parsed-boolean-code) 'less-then)
       (<
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'less-then-or-equal)
       (<=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then)
       (>
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then-or-equal)
       (>=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'equal)
       (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'not-equal)
       (not (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env))))
      (else "Not a legal boolean expression!"))))
       
        
(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (let ((body-of-function (cadr (caddr parsed-no-code-function))))
      (run-parsed-code body-of-function env))))

           
(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'block-exp)
   ;    (display parsed-no-code))
       (clean-block-exp (run-parsed-block-expression parsed-no-code env)))
      ((eq? (car parsed-no-code) 'remember-exp)
       (run-parsed-remember-expression parsed-no-code env))
      ((eq? (car parsed-no-code) 'spit-out-exp)
       (list '*screen* (run-parsed-code (cadr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'let-exp)
       (let* ((list-of-2-lists (cadr parsed-no-code))
              (new-env (construct-scoped-env list-of-2-lists env))
              (body (caddr parsed-no-code)))
         (run-parsed-code body new-env))) 
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env) 
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'call-exp) ;this is call lambda code stuff lol woot WHO??
         (let ((function (cadr parsed-no-code)))
            (run-parsed-function-code
             function
             (extend-env
         (let ((list-of-var-names (cdr (cadr (cadr parsed-no-code))))
               (list-of-var-values (map (lambda (packet) (run-parsed-code (car packet) (cadr packet)))
              (map (lambda (x) (list x env)) (caddr parsed-no-code)))))
         (extend-scope
          list-of-var-names
          list-of-var-values
          empty-scope))
         (pop-to-global env))))))))

(define env '((global (age 21) (a 7) (b 5) (c 23))))

(define sample-no-code '(call (function (a) (call (function (a) a) age)) 5))
(define sample-define-code '(block (remember a 15) (remember b 17) (spit-out a) (spit-out b) (remember c (do-mathy-stuff + a b)) (spit-out c)))
(define parsed-no-code (no-parser sample-define-code))
;parsed-no-code
(run-parsed-code parsed-no-code env)