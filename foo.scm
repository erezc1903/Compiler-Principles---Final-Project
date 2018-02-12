

;(> 1)

;(< 2)

;(< 3 2 1 4)

;(= 1 2)

;(= 8)

;(= 1 1 1 1)

;(= 2 2 2 3)

;23

;"mayer"

;(define x 6)

;(integer? 15)

;(boolean? #t)

;(integer? #t)

;(boolean? 15)

;(null? '())

;(define + 5)

;-4

;2

;-3/6

;(+ x (((lambda (y) (lambda (z) 5))3)10))

;(number? a 1)

;(pair? '(1 2) 1)

;(integer? "-6" 1)

;10

;(boolean? 3 1)

;(number? 3 1)

;(number? '() 1)

;(number? 2/4 1)

;(char? #\a 1)

;(char? a 1)

;(cdr '(1 2) 1)

;(cons 1 2 1)

(((((lambda (x) (lambda (y) (lambda (z) (lambda (w) (if x y z))))) 1) 2) 3) #f)



;(((lambda (x) (lambda (y) (or y x))) 3) 4)
;(define y 3)

;y

;(define foo (lambda (x) (if #t x 3)))

;(foo 10)

;(define p '(3 4))

;(cons (cons (cons (cons 1 2) 3) 4) 5)

;(cdr '(1 . 2))

;(car '(3 4 . 5))

;(cdr '(3 4 . 5))

;;'((5 6) (7 8 9))

;(car '((1 2 3 4)))

;(cdr '((1 2 3 4)))

;(not (if 1 #f 3))

;(vector? '#(7 8 "f"))


;(begin #t 2 (if x (or #f y #t) 3))

;(or #f #f 2 #f)

;(define x 33)

;(char->integer #\3)

;(char? x)

;(if 7 8 9)

;(define y 1)