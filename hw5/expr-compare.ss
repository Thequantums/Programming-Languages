#lang racket

;Problem 1: write expr-compare for comparing two scheme expressions

;two expressions are values, so no need to check list
(define (value_checker expx expy)
        ( if (equal? expx expy) expy
            (if (boolean? expx) (if expx '% '(not %))
                ( if (and (list? expx) (list? expy)) (list_checker expx expy)
                     (list 'if '% expx expy)
                 )
         )  
        )
)
(define (check_lambda expre_list1 expre_list2 lornot)
    (define (check_l_wrapper list1 list2 accum)
  (cond
    [(or (empty? list1) (empty? list2)) (reverse accum)]
    [(equal? (car list1) (car list2)) (check_l_wrapper (cdr list1) (cdr list2) (cons (car list1) accum))]
    [(and (list? (car list1)) (list? (car list2)) (lamb (caar list2)) (lamb (caar list1))) (check_l_wrapper (cdr list1) (cdr list2) (cons (check_lambda (car list1) (car list2) lornot) accum))]
    [(and (list? (car list1)) (list? (car list2)) (not (and (lamb (caar list2)) (lamb (caar list1))))) (check_l_wrapper (cdr list1) (cdr list2) (cons (check_l_wrapper (car list1) (car list2) '()) accum)) ]
    [else
     (check_l_wrapper (cdr list1) (cdr list2) (cons (value_checker (car list1) (car list2)) accum))])
)
  (let ([dict (dict-set #hash() '() '())]) 
      (cons lornot (cons (check_l_wrapper (change_bound (cadr expre_list1) (fill_dict (cadr expre_list1) (cadr expre_list2) dict 1) 1) (change_bound (cadr expre_list2) (fill_dict (cadr expre_list1) (cadr expre_list2) dict 2) 2) '())
              (check_l_wrapper (change_bound (cddr expre_list1) (fill_dict (cadr expre_list1) (cadr expre_list2) dict 1) 1) (change_bound (cddr expre_list2) (fill_dict (cadr expre_list1) (cadr expre_list2) dict 2) 2) '()))))
  )

;fill the dictionary 
(define (fill_dict expre_list1 expre_list2 dict num)
  (cond
    [(or (equal? expre_list1 empty) (equal? expre_list2 empty)) dict]
    [(and (symbol? expre_list1) (symbol? expre_list2) (equal? expre_list1 expre_list2)) dict]
    [(and (symbol? expre_list1) (symbol? expre_list2) (not (equal? expre_list1 expre_list2))) (if (= num 1) (dict-set dict expre_list1 expre_list2) (dict-set dict expre_list2 expre_list1))]
    [(equal? (car expre_list1) (car expre_list2)) (fill_dict (cdr expre_list1) (cdr expre_list2) dict num)]
    [else
     (if (= num 1) (fill_dict (cdr expre_list1) (cdr expre_list2) (dict-set dict (car expre_list1)(car expre_list2)) num)
         (fill_dict (cdr expre_list1) (cdr expre_list2) (dict-set dict (car expre_list2) (car expre_list1)) num) )]
    )
)
;write summary for the two list (calling other functions here)
(define (list_checker expx expy)
   (define (list_checker_wrapper expx expy accum) (define (combinable item)
  (if (or (equal? item 'quote) (equal? item 'list)) #f #t)
)
  (cond
    [(or (empty? expx) (empty? expy)) accum] ;return when expx and expy empty
    [(not (equal? (length expy) (length expx))) (reverse (list 'if '% expx expy) )]
    [(and (equal? (length expx) (length expy)) (> (length expy) 2) (real_lambda (car expy)) (real_lambda ( car expx)) (equal? (length (cadr expx)) (length (cadr expy))))
               (reverse (check_lambda expx expy 'lambda )) ;modify this one argument extra
                ] 
    [(and (equal? (length expx) (length expy)) (> (length expy) 2) (lamb (car expy)) (lamb ( car expx)) (equal? (length (cadr expx)) (length (cadr expy))))
               (reverse (check_lambda expx expy '?)) 
                ]
    [(and (equal? (length expx) (length expy)) (> (length expy) 2) (real_lambda (car expy)) (real_lambda ( car expx)) (not (equal? (length (cadr expx)) (length (cadr expy))))) 
      (reverse (cons 'if (cons '% (cons (cons 'lambda (cdr expx)) (list (cons 'lambda (cdr expy)))))))
     ]
    [(and (equal? (length expx) (length expy)) (> (length expy) 2) (lamb (car expy)) (lamb ( car expx)) (not (equal? (length (cadr expx)) (length (cadr expy))))) 
      (reverse (cons 'if (cons '% (cons (cons '? (cdr expx)) (list (cons '? (cdr expy)))))))
     ]
    [(and (real_lambda (car expx)) (real_lambda (car expy))) (reverse (cons 'if (cons '% (cons (cons 'lambda (cdr expx)) (cons expy '()))))) ]
    [(or (and (lamb (car expy)) (not (lamb (car expx)))) (and (lamb (car expx)) (not (lamb (car expy)))))
      (reverse (cons 'if (cons '% (cons (cons '? (cdr expx)) (cons expy '())))))]  
    [(and (equal? (car expx) (car expy)) (combinable (car expx))) (list_checker_wrapper (cdr expx) (cdr expy) (cons (value_checker (car expx) (car expy)) accum )) ]
    [(and (equal? (car expx) (car expy)) (not (combinable (car expx)))) (reverse (list 'if '% expx expy))]
    [(or (equal? (car expy) 'if) (equal? (car expx) 'if)) (reverse (list 'if '% expx expy))]
    [else (list_checker_wrapper (cdr expx) (cdr expy) (cons (value_checker (car expx) (car expy)) accum))] 
    )
)
  (reverse (list_checker_wrapper expx expy '()))
 )


;change the 2 variables to bound
(define (look_bound value1 value2)
  (string->symbol
     (constraint (string-append (symbol->string value1) (string-append "!" (symbol->string value2))))))

;change the place in expression where bound are not supposd to be
(define (constraint string) 
  (if (equal? (list-ref (string-split string "!") (- (length (string-split string "!")) 1)) (list-ref (string-split string "!") 0)) (list-ref (string-split string "!") 0)
      string))

(define (rework_exprx expx dict num )
  (cond
    [(equal? expx '()) (if (= num 1) dict '())]
    [(list? (car expx)) ( if (= num 1) (rework_exprx (cdr expx) (rework_exprx (car expx) dict num) num) (cons (rework_exprx (car expx) dict num) (rework_exprx (cdr expx) dict num)))]
    [(and (= num 1)  (list? (car expx)) (= (length (car expx)) 4) (equal? (caar expx) 'if) (dict-has-key? dict (car expx)) (equal? (cadr (car expx)) '%))
       (rework_exprx (cdr expx) (dict-set dict (car expx) (+ (dict-ref dict (car expx)) 1)) num)
     ]
    [(and (= num 1) (list? (car expx)) (= (length (car expx)) 4) (equal? (caar expx) 'if) (not (dict-has-key? dict (car expx))) (equal? (cadr (car expx)) '%))
       (rework_exprx (cdr expx) (dict-set dict (car expx) 1) num)
     ]
    [( and (= num 2) (= (length expx) 4) (equal? (car expx) 'if) (equal? (cadr expx) '%) (dict-has-key? dict expx) (> (dict-ref dict expx) 1))
       (string->symbol (constraint (string-append (symbol->string (caddr expx)) (string-append "!" (symbol->string (cadddr expx))))))
    ]
    [ (and (= num 2) (= (length expx) 4) (equal? (cadr expx) '%) (equal? (car expx) 'if) (not (and (dict-has-key? dict expx) (> (dict-ref dict expx) 1)))) expx]
    [else ( if (= num 1) (rework_exprx (cdr expx) dict num) (cons (car expx) (rework_exprx (cdr expx) dict num)))]
 )
)



;is lambda or '?
(define (lamb value) (if (or (equal? value 'lambda) (equal? value '?)) #t #f)) 

;is the word "lambda"
(define (real_lambda value) ( if(equal? value 'lambda) #t #f))

;change the expresion list for bound variable
(define (change_bound expre_list dict num)
  (cond
    [(equal? empty expre_list)'()]
    [(list? (car expre_list)) (cons (change_bound (car expre_list) dict num) (change_bound (cdr expre_list) dict num))]
    [(dict-has-key? dict (car expre_list)) (if (= num 1) (cons (look_bound (car expre_list) (dict-ref dict (car expre_list))) (change_bound (cdr expre_list) dict num))
                                         (cons (look_bound (dict-ref dict (car expre_list)) (car expre_list)) (change_bound (cdr expre_list) dict num)) )
                                         ]
    [(and (symbol? expre_list) (dict-has-key? dict expre_list)) ((cons (look_bound expre_list (dict-ref dict expre_list)) '())) ]
    [(and (symbol? expre_list) (not (dict-has-key? dict expre_list))) ((cons expre_list '()))]
    [else (cons (car expre_list) (change_bound (cdr expre_list) dict num))]))


; fucntion to comapre the two expression and report summary of differnces
(define (expr-compare expr_x expr_y) (cond
                                       
     [(and (list? expr_x) (list? expr_y)) (rework_exprx (list_checker expr_x expr_y) (rework_exprx (list_checker expr_x expr_y ) (dict-set #hash() '() '()) 1) 2)]
     [(and (list? expr_x) (not (list? expr_y))) (list 'if '% expr_x expr_y)]
     [(and (not (list? expr_x)) (list? expr_y)) (list 'if '% expr_x expr_y)]
     [else (value_checker expr_x expr_y)]
                                       )
  )                    

;Problem 2:
(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))
  )
)
 

;Problem 3:
(define test-expr-x '(+ 3 ((lambda (a b) (list a b)) 1 2)))
(define test-expr-y '(+ 2 ((lambda (a c) (list a c)) 1 2)))
