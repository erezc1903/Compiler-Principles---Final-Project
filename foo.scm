
;23 ;V

;; test 2
;(- 2) ; known problem

; test 3
;-1 ;V

;; test 4
;(/ 35 7) ;V

;; test 5
;(* 5 7) ;V

;; test 6
;(+ 53242653 35463560) ;V

; test 7
;(+ (* 564 5) (- (+ 4 5))) ; known problem

;; test 8
;(- ( - ( - ( - ( - ( - ( - ( - (- 5))))))))) ;known problem

;; test 9
;((lambda (a) ( + a 7)) 5) ;V

;; test 10
;((lambda (a b) (a ( b (a 5 6) 8) 11)) + -) ;V

;; test 11
;((lambda (a b) (if (number? a) (make-string a) b)) #t 6) ;V

;; test 12
; ((lambda (a b c) (if (= a c) (+ a c) (- b c 4))) 1 2 3) ;V


;(define sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1)))))) 
; (sum 60) ;V

;; test 15
;(define rec (lambda (func1 func2 init param num)
;                (if (= 0 num)
;                    init
;                    (func1 (rec func1 func2 (func2 2 init param) param (- num 1))
;                      )
;                    )
;                )
;    )
;(rec - + 5 7 20) ;V

;; test 21
;((lambda (f1 f2 input1 input2 input3 ifval)
;      (if (ifval input2 input3)
;      (f1 (f2 input1 5) 40)
;      (begin
;        (set! f2 f1)
;        (f1 (f2 input1 5) 40)
;        )
;      )
; ) * + 5 7 -8 >) ;V
;; test 24
;(((lambda (x y) (lambda () (+ x y))) 56 65)) ;V

;; test 25
;(((lambda (x y) (lambda () (+ x y))) ((lambda (a) (* a a)) 500) 2)) ;V

;; test 26
;(((lambda (x y) (lambda () (x 89 y))) (lambda (a b) (* a b)) 2)) ;V

;;test 28
;((lambda (f1 f2)
;      (if (eq? f1 f2)
;          'eq!
;          'no!
;          )
;      ) + -) ;V

;; test 29
;(define factorial
;    (lambda(n)
;      (if (= n 0)
;        1
;        (* n (factorial (- n 1))))))
;(factorial 6) ;V

;; test 30
;(define fibonacci
;        (lambda (n)
;          (if (< n 2)
;              1
;              (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
;(fibonacci 11) ;V

;; test 31
;(define (equal? x y)
;    (if (not (pair? x))
;        (eq? x y)
;        (and (equal? (car x) (car y))
;             (equal? (cdr x) (cdr y)))))
;(equal? (cons 1 2) (cons 1 3)) ;V

;; test 32
;(define (variable? x) (symbol? x))
;(variable? #t) ;V

;; test 33
;((lambda (x y)
;      (cond ((= x y) #t)
;            ((> x y) 'x>y)
;            ((and (> (+ x y) 10) (> (* x y) 40)) 'string)
;            )
;      ) 111 11) ;V

;; test 34
;((lambda (a) (if (string? a) (string->symbol a))) "a23") ;V

;; test 35
; (define (=number? exp num)
;  (and (number? exp) (= exp num)))
;(=number? 5 1) ;V

; test 37
;(define (a x set)
;  (cond
;    ((null? set) (list x))
;    ((= x (car set)) set)
;    ((< x (car set)) (cons x set))
;    (else (cons (car set)(a x (cdr set))))))
	
;(a 3 (cons 5 4)) ;V

;; test 38
;(define (expmod a b m) 
;  (cond ((= b 0) 1)
;	((= (remainder b 2) 0) (remainder (expmod (remainder (* a a) m) (/ b 2) m) m))
;	(else (remainder (* a (expmod a (- b 1) m)) m))))
   
;(expmod 5 13 1) ;V

;; test 40
;(define (b set1 set2)
;  (cond
;    ((null? set1) set2)
;    ((null? set2) set1)
;    (else
;     (let ((s1 (car set1))
;           (s2 (car set2)))
;       (cond
;       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
;       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
;       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
;(b '(1 2 3) '(4 5 6)) ;V

;; test 43
;(define a 'hello)
;a ;V

;; test 44
;(define b (string-length "world"))
;b ;V

;; test 45
;(define loop (lambda (num func param)
;                 (if (zero? num)
;                     param
;                     (loop (- num 1) func (func param))
;                     )
;                 )
;    )
;(loop 7 (lambda (x) (+ x x)) 43) ;V

;; test 46
;(define loop2 (lambda (num func param)
;                  (if (zero? num)
;                      param
;                      (func (loop2 (- num 1) func param)
;                        )
;                      )
;                  )
;    )
;(loop2 7 (lambda (x) (+ x x)) 3) ;V

;; test 50
;(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;c ;V
;; test 52
;(define (accumulate op init lst)
;    (if (null? lst)
;        init
;        (op (car lst) (accumulate op init (cdr lst)))))
;(accumulate * 2 '(1 2 3 4 5 6 7 8 9)) ;V

;; test 53
;(define f1 (lambda (x) x))
;(f1 2) ;V

;; test 54
;(define f2 (lambda (o a b) (o a b)))
;(f2 + 5 6) ;V


;; test 59
;(let ((square (lambda (x) (* x x)))) 33) ;V

;; test 60
;(define fun1 (lambda ()
;                 (begin
;                   (+ 2 1)
;                   (+ 3 4)
;                   )
;                 )
;    )
;(fun1) ;V


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test 61
; (define fun2 (lambda (x)
;                 (begin
;                   (set! x (+ 2 1))
;                   (set! x (+ x 3 4))
;                   x
;                   )
;                 )
;    )
;(fun2 45)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 76
;(define foo7 (lambda (x y) (
;                            begin
;                            (set! y x)
;                            (set! x y)
;                            (+ y x))
;                 )
;    )
;(foo7 1 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; test 79
;(define foo10 (lambda (x y) (
;                            begin
;                            (set! y x)
;                            (eq? y x))
;                 )
;    )
;(foo10 12 12) ;V

;; test 90
;(((lambda (x)  
;    (lambda (z)
;      (* x x))) 4) 5) ;V

;; test 91
;((lambda () (+))) ;V

;; test 92
;((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 55) 66) ;V

;; test 93
;((((lambda () (lambda (aa) (lambda (bb) (- aa bb))))) 55) 66) ;V

;; test 94
;((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 30) 4) ;V

;; test 95
;((lambda (a b c d) (a (b (c d)))) + - * 4) ;known problem

;; test 99
;(define rem (lambda (x)(remainder x 10)))
;(rem 443) ;V

;; test 100
;(define f (lambda (b) (/ 200 b)))
;(f 4) ;V

;; test 101
;((lambda (a b) (cons a b)) 5 4) ;V

;; test 103
;(boolean? (procedure? (lambda () (make-string 5)))) ;V

;; test 104
;((lambda (a) (boolean? a)) #t) ;V

;; test 105
;((lambda (a) (if (char? a) (char->integer a) (if (integer? a) (integer->char a) a))) #\x50) ;V

;; test 106
;(pair? (cons 4 6)) ;V

;; test 107
;((lambda (a b) (cons a b)) 55 6) ;V

;; test 108
;(pair? (lambda (a b) (cons a b))) ;V

;; test 109
;((lambda (a b) (pair? (cons a b))) 1234 5678) ;V

;; test 110
;(procedure? (lambda (a b) (cons a b))) ;V

;; test 111
;(zero? 5) ;V

;; test 112
;(not (zero? 5)) ;V

;; test 113
;(define a (lambda (b) (rational? b))) ;V
;(a 56)

;;test 114
;(define a (lambda (b) (not (rational? b))))
;(a 56) ;V

;; test 115
;(denominator (/ 10 2)) ;V

;; test 116
;(numerator 100/50) ;V

;; test 117
;(define a (lambda (x y) (if (not (zero? x)) (denominator (/ y x)) (numerator y))))
;(a 0 5) ;V

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;test 119
;(define x (lambda (a b) (if (> (string-length a) b) (string-ref a b) a)))
;(char->integer (x "hello" 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; test 120
;(define x (lambda (a b c) (if (> (string-length a) b) (string-set! a b c) a)))
;(string->symbol (x "hello" 30 #\r)) ;V

;; test 121
;(string->symbol ((lambda (b) (symbol->string b)) 'a)) ;V

;; test 128
;(define f (lambda (p x) (begin
;                            (set-car! p x)
;                            p)))
;(f (cons 4 5) 444) ;V

;; test 129
;(define f (lambda (p x) (begin
;                            (set-cdr! p x)
;                            p)))
;(f (cons 4 5) 444) ;V

;; test 130
;(apply (lambda (a) (* a a)) '(2)) ;V

;; test 131
;(let ((str 'hello))
;    (set! f1 (lambda () str))
;    (set! f2 (lambda () (string->symbol str)))
;	str
;    ) ;V

;; test 132
;(let ((x 2) (y 3))
;  (let* ((x 7)
;         (z (+ x y)))
;    (* z x))) ;V

;; test 133
;(let* ((x 2) (y 3))
;    (let ((x 7)
;           (z (+ x y)))
;      (* z x))) ;V

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 134
;(letrec ((x 2) (y 3))
;    (let ((x 7)
;           (z (+ x y)))
;      (* z x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; test 135
;((lambda (ls1 ls2) (append ls1 ls2)) '(1 2 3) '(q w e)) ;V


;; test 136
;(define bla (lambda (x y) (append (list x 2) (list 2 y))))
;(bla '(1 2 3) '(q w e)) ;V



;; test 137
;(apply + (list 1 3 2)) ;V

;; test 138
;((lambda (list) (apply (lambda (x . y) (+ x 3)) list)) (list 1 3 2)) ;V

;; test 139
;(map number? '(1 2 3)) ;V

;; test 140
;(map boolean? '(#t #t #f "bla")) ;V

;; test 141
;(map (lambda (x) (if (integer? x) (char->integer (integer->char x)) 0)) '(1 2 3 + #f)) ;V


;; test 142
;(map (lambda (x) (if (string? x) (string->symbol x) 0)) '("a1" "b2" 3 + "cf")) ;V


;; test 143
;((lambda (int) (integer? int))4) ;V

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 144
;(map number? '(1 2 '3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; test 145
;(string? '1) ;V

;; test 147
;((lambda (ch) (if (char? ch) (char->integer ch))) #\x666) ;V

;; test 148
;((lambda (int) (if (boolean? (char? (integer->char int))) 'ok)) 5) ;V

;; test 149
;((lambda (str)
;   (if (string? str)
;       (begin
;	 (string-set! str 1 (integer->char 66))
;	 str))) "ssss") ; V

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 150
;((lambda (sym int)
;   (if (symbol? sym) (begin
;		       (set! a (symbol->string sym))
;		       (string-set! a 2 (integer->char int))
;		       a))) 'abc 33)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; test 151
;((lambda (list) (begin
;		(set-car! (car (cdr list)) (cons 1 2))
;		 list)) (list 1 (cons 22 66) 3 4)) ;V

;; test 152
;((lambda (list) (begin
;		(set-cdr! (cdr list) (cons 1 2))
;		list)) (list 1 2 3 4)) ;V

;; test 153
;(let* ((x 1)
;         (y 2)
;         (z 3))
;    (+ x y z)
;    ) ;V

;; test 154
;((lambda (x y) (
;                 let* ((a x)
;                       (b y)
;                       )
;                 (* a a b))
;    ) 44 55) ;V

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 155
;(letrec ((loop (lambda (i a)
;		 (set! a (+ (* 10 a) i))
;		 (if (< i 10)
;		     (loop (+ i 1) a)
;		     a))))
;  (loop 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; test 156
;(define func (lambda (lst num) (
;                                  letrec ((loop
;                                             (lambda (i a)
;                                               (cond ((null? i)
;                                                      #f)
;                                                 ((eq? (car i) a) #t)
;                                                 (else
;                                                   (loop (cdr i) a)))
;                                               )))
;                                    (loop lst num)))
;                 )
;(func (list 1 2 3) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 157
;(quasiquote (0 1 2)) ;V

;; test 158
;(quasiquote (0 (unquote (+ 1 2)) 4)) ;V


;; test 159
;(quote (1 a 4)) ;V


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 160
;(define q (quote (bla (((s ) s )sd ))))
;q ; known problem with nested lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 161
;(quasiquote (1 2 (unquote (+ 3 4)))) ;V


;; test 162
;(quasiquote ( a 3 4 (unquote (* 4 3 2 1)))) ;V



;; test 164
;`(unquote (quote (3 4 5))) ; V

;; test 166
;(let* ((a 1) (b 1) (c (* a b)))
;   c) ;V

;; test 167
;(define (lst . x) x)
;(lst 1 2 3 4 5 6) ;V

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 168
;(define (func . numbers)
;    (if (null? numbers)
;        0
;        (+ (car numbers) (apply func (cdr numbers)))))
;(func 9 8 7 6 5 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; test 169
;(define (f . x) (apply + x)) 
;(f 5 4 8 6) ;V

;; test 172
;5 ;V

;; test 174
;(define (plusminus . l)
;    (if (null? l) 0
;        (if (null? (cdr l)) (car l)
;        (+ (- (car l) (car (cdr l))) (apply plusminus (cdr (cdr l)))))))
;(plusminus 5 4 8 6 7 2 3 0 5 4 8 9 0) ;V

;; test 175
;(define (less-than  . l)
;     (cond
;       ((null? l) #t)
;       ((null? (cdr l)) #t)
;       ((< (car l) (car (cdr l))) (apply less-than  (cdr l)))
;       (else #f)))
	   
;(less-than 5 4 8 9 6 2 5 4 4 44) ;V

;; test 176
;(procedure? (lambda () (make-string 5))) ;V


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test 177
;(((((lambda (a)
;      (lambda (b)
;        (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y)))))
;	  ((lambda (n)
;	     ((n (lambda (x) (lambda (x) (lambda (y) y))))
;	      (lambda (x) (lambda (y) x))))
;	   (((lambda (a)
;	       (lambda (b)
;		 ((b (lambda (n)
;		       ((lambda (p) (p (lambda (a) (lambda (b) b))))
;			((n (lambda (p)
;			      (((lambda (a)
;				  (lambda (b) (lambda (c) ((c a) b))))
;				((lambda (n)
;				   (lambda (s)
;				     (lambda (z) (s ((n s) z)))))
;				 ((lambda (p)
;				    (p (lambda (a) (lambda (b) a))))
;				  p)))
;			       ((lambda (p)
;				  (p (lambda (a) (lambda (b) a))))
;				p))))
;			 (((lambda (a)
;			     (lambda (b) (lambda (c) ((c a) b))))
;			   (lambda (x) (lambda (y) y)))
;			  (lambda (x) (lambda (y) y)))))))
;		  a)))
;	     a)
;	    b)))
;	 ((lambda (n)
;	    ((n (lambda (x) (lambda (x) (lambda (y) y))))
;	     (lambda (x) (lambda (y) x))))
;	  (((lambda (a)
;	      (lambda (b)
;		((b (lambda (n)
;		      ((lambda (p) (p (lambda (a) (lambda (b) b))))
;		       ((n (lambda (p)
;			     (((lambda (a)
;				 (lambda (b) (lambda (c) ((c a) b))))
;			       ((lambda (n)
;				  (lambda (s)
;				    (lambda (z) (s ((n s) z)))))
;				((lambda (p)
;				   (p (lambda (a) (lambda (b) a))))
;				 p)))
;			      ((lambda (p)
;				 (p (lambda (a) (lambda (b) a))))
;			       p))))
;			(((lambda (a)
;			    (lambda (b) (lambda (c) ((c a) b))))
;			  (lambda (x) (lambda (y) y)))
;			 (lambda (x) (lambda (y) y)))))))
;		 a)))
;	    b)
;	   a)))))
;    ((lambda (n)
;       ((lambda (p) (p (lambda (a) (lambda (b) b))))
;	((n (lambda (p)
;	      (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
;		((lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))
;		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)))
;	       (((lambda (a)
;		   (lambda (b)
;		     ((b (a (lambda (a)
;			      (lambda (b)
;				((a (lambda (n)
;				      (lambda (s)
;					(lambda (z) (s ((n s) z))))))
;				 b)))))
;		      (lambda (x) (lambda (y) y)))))
;		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))
;		((lambda (p) (p (lambda (a) (lambda (b) b)))) p)))))
;	 (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
;	   (lambda (x) x))
;	  (lambda (x) x)))))
;     (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
;   (((lambda (a)
;       (lambda (b)
;	 ((b (a (lambda (a)
;		  (lambda (b)
;		    ((a (lambda (n)
;			  (lambda (s) (lambda (z) (s ((n s) z))))))
;		     b)))))
;	  (lambda (x) (lambda (y) y)))))
;     (((lambda (a)
;	 (lambda (b)
;	   ((b (a (lambda (a)
;		    (lambda (b)
;		      ((a (lambda (n)
;			    (lambda (s) (lambda (z) (s ((n s) z))))))
;		       b)))))
;	    (lambda (x) (lambda (y) y)))))
;       ((lambda (x) (lambda (y) (x (x (x y)))))
;	(lambda (x) (lambda (y) (x (x y))))))
;      (lambda (x) (lambda (y) (x (x (x y)))))))
;    (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
;  #t)
; #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FIX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; test 178
((lambda (x) (x x 10000))
 (lambda (x n)
   (if (zero? n) #t
       (x x (- n 1)))))

; test 179
(define not (lambda (x) (if x #f #t)))

(and
 (boolean? #t)
 (boolean? #f)
 (not (boolean? 1234))
 (not (boolean? 'a))
 (symbol? 'b)
 (procedure? procedure?)
 (eq? (car '(a b c)) 'a)
 (= (car (cons 1 2)) 1)
 (integer? 1234)
 (char? #\a)
 (null? '())
 (string? "abc")
 (symbol? 'lambda)
 (not (string? 1234))
 (pair? '(a . b))
 (not (pair? '()))
 (zero? 0)
 (not (zero? 234))
 (= 97 (char->integer (string-ref "abc" 0)))
 (let ((n 10000))
   (= n (string-length (make-string n))))
 (= 65 (char->integer #\A))
 (= 3 (remainder 7 4))
 (= 6 (* 1 2 3))
 (= 1 (*))
 (= 234 (* 234))
 (= 6 (+ 1 2 3))
 (zero? (+))
 (= 234 (+ 234))
 (= 1 (- 6 3 2))
 (< 1 2 3 4 5)
 (> 5 4 3 2 1)
 )














;;; Constants
;5
;-5
;1/3
;-2/4
;#t
;#f
;#\k
;#\newline
;'()
;"av"
;"at\nme"
;'(1 . 2)
;'(1 . (2 3))
;'#(1 "at" 2 4 "ag")
;'#(1 "at" 2 4 "ag" #(1 2 69) #(1 2 69))
;'(1 2 3 (4 (5 6 7) 8 9))
;'(4 5 6 . #(4 8 16 (#\a #\b #\c)))
;'(4 5 6 . #(4 8 16 (1 2 3)))
;'a ;; to figure out how to handle symbol type cons
;'b
;'+
;(symbol? "a")
;; Constants
;(eq? 'a 'a)
;(eq? (string->symbol (symbol->string (string->symbol "c"))) (string->symbol "c"))


;"+"

;(list 1 2 '(3))

;((lambda () 3))

;(append '(1 2) '(3 4) '(5 6))

;;; primitives tests
;append

;apply ; procedure
;(apply) ; Exception: incorrect argument count in call (apply)
;(apply +) ; Exception: incorrect argument count in call (apply +)
;(apply + 4) ; Exception in apply: 4 is not a proper list
;(apply + '(1 2 3)) ; 6
;(apply + '(4)) ; 4
;(apply 1 '(4)) ; Exception: attempt to apply non-procedure 1
;(apply + 1 '(4)) ; 5
;(apply + 1 2 '(3 4 5)) ; 15
;(apply + 1 2 3 4 '(5 6 7 8 9 10)) ; 55
;((lambda (a b c d e f g) (apply)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; Exception: incorrect argument count in call (apply)
;((lambda (a b c d e f g) (apply a)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; Exception: incorrect argument count in call (apply +)
;((lambda (a b c d e f g) (apply a b)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; Exception in apply: 4 is not a proper list
;((lambda (a b c d e f g) (apply a c)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; 6
;((lambda (a b c d e f g) (apply a d)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; 4
;((lambda (a b c d e f g) (apply e d)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; Exception: attempt to apply non-procedure 1
;((lambda (a b c d e f g) (apply a e d)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; 5
;((lambda (a b c d e f g) (apply a e f g)) + 4 '(1 2 3) '(4) 1 2 '(3 4 5)) ; 15


;<
;=
;>

;+ ; closure
;(+) ; 0
;(+ 1 2) ; 3
;(+ 1 2 3 4) ; 10
;(+ 1 2 "three" 4) ; exception not a number
;((lambda (w x y z) (+)) 1 2 3 4) ; 0
;((lambda (w x y z) (+ w x)) 1 2 3 4) ; 3
;((lambda (w x y z) (+ w x y z)) 1 2 3 4) ; 10
;((lambda (w x y z) (+ w x y z)) 1 2 "three" 4) ; exception not a nnumber

;/
;*
;-

;boolean?
;(boolean? #t #f) ; exception incorrect arg count
;(boolean? #f) ; #t
;(boolean? #f) ; #t
;(boolean? 1/3) ; #f
;(boolean? '(1 . 8)) ; #f
;(boolean? ((lambda () #t))) ; #t
;((lambda (v w x y z) (boolean? v w)) #t #f 1/3 '(1 . 8) ((lambda () #t))) ; exception incorrect arg count
;((lambda (v w x y z) (boolean? w)) #t #f 1/3 '(1 . 8) ((lambda () #t))) ; #t
;((lambda (v w x y z) (boolean? w)) #t #f 1/3 '(1 . 8) ((lambda () #t))) ; #t
;((lambda (v w x y z) (boolean? x)) #t #f 1/3 '(1 . 8) ((lambda () #t))) ; #f
;((lambda (v w x y z) (boolean? y)) #t #f 1/3 '(1 . 8) ((lambda () #t))) ; #f
;((lambda (v w x y z) (boolean? z)) #t #f 1/3 '(1 . 8) ((lambda () #t))) ; #t

;car
;(car '(111 2 3)) ; 111
;(car '(111 2 3)) ; 111
;(car '(111 2 3) 6) ; exception incorrect arg count
;(car 6) ; exception argument is not a pair
;(car "king") ; exception argument is not a pair
;((lambda (x y z) (car x)) '(111 2 3) 6 "king") ; 111
;((lambda (x y z) (car x)) '(111 2 3) 6 "king") ; 111
;((lambda (x y z) (car x y)) '(111 2 3) 6 "king") ; exception incorrect arg count
;((lambda (x y z) (car x) (x y)) '(111 2 3) 6 "king") ; exception attempt to apply non procedure
;((lambda (x y z) (car y)) '(111 2 3) 6 "king") ; exception argument is not a pair
;((lambda (x y z) (car z)) '(111 2 3) 6 "king") ; exception argument is not a pair
;((lambda (x y z) ((lambda (a) (car x)) 5)) '(111 2 3) 6 "king") ; 111
;((lambda (x y z) ((lambda (a) (car x)) 5)) '(111 2 3) 6 "king") ; 111
;((lambda (x y z) ((lambda (a) (car x y)) 5)) '(111 2 3) 6 "king") ; exception incorrect arg count
;((lambda (x y z) ((lambda (a) (car y)) 5)) '(111 2 3) 6 "king") ; exception argument is not a pair
;((lambda (x y z) ((lambda (a) (car z)) 5)) '(111 2 3) 6 "king") ; exception argument is not a pair

;cdr
;(cdr '(111 2 3)) ; (2 3)
;(cdr '(111 2 3)) ; (2 3)
;(cdr '(111 2 3) 6) ; exception incorrect arg count
;(cdr 6) ; exception argument is not a pair
;(cdr "king") ; exception argument is not a pair
;((lambda (x y z) (cdr x)) '(111 2 3) 6 "king") ; (2 3)
;((lambda (x y z) (cdr x)) '(111 2 3) 6 "king") ; (2 3)
;((lambda (x y z) (cdr x y)) '(111 2 3) 6 "king") ; exception incorrect arg count
;((lambda (x y z) (cdr x) (x y)) '(111 2 3) 6 "king") ; exception attempt to apply non procedure 
;((lambda (x y z) (cdr y)) '(111 2 3) 6 "king") ; exception argument is not a pair
;((lambda (x y z) (cdr z)) '(111 2 3) 6 "king") ; exception argument is not a pair
;((lambda (x y z) ((lambda (a) (cdr x)) 5)) '(111 2 3) 6 "king") ; (2 3)
;((lambda (x y z) ((lambda (a) (cdr x)) 5)) '(111 2 3) 6 "king") ; (2 3)
;((lambda (x y z) ((lambda (a) (cdr x y)) 5)) '(111 2 3) 6 "king") ; exception incorrect arg count
;((lambda (x y z) ((lambda (a) (cdr y)) 5)) '(111 2 3) 6 "king") ; exception argument is not a pair
;((lambda (x y z) ((lambda (a) (cdr z)) 5)) '(111 2 3) 6 "king") ; exception argument is not a pair

;char->integer
;(char->integer 7 13) ; exception incorrect arg count
;(char->integer '#\x) ; 120
;(char->integer '#\x) ; 120
;(char->integer 1/3) ; exception 1/3 is not a char
;(char->integer '#\y) ; 121
;(char->integer '#\z) ; 122
;(char->integer '#\w) ; 119
;(char->integer '#\a 105) ; exception incorrect arg count
;((lambda (x y z) (char->integer (cdr z))) 10 1/3 '(1 . #\s)) ; 115
;((lambda (x y z) (char->integer (cdr z))) 7 1/3 '(#\s . 8)) ; exception 8 is not a char
;((lambda (x y z w) (char->integer w)) #\a 1/3 '(1 . 8) #\s) ; 115
;(char->integer '#\newline) ; 10

;char?
;(char? #\d #\f) ; exception incorrect arg count
;(char? #\f) ; #t
;(char? #\f) ; #t
;(char? 1/3) ; #f
;(char? '(1 . 8)) ; #f
;(char? ((lambda () #\z))) ; #t
;((lambda (v w x y z) (char? v w)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; exception incorrect arg count
;((lambda (v w x y z) (char? w)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; #t
;((lambda (v w x y z) (char? w)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; #t
;((lambda (v w x y z) (char? x)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; #f
;((lambda (v w x y z) (char? y)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; #f
;((lambda (v w x y z) (char? z)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; #t

;cons
;(cons #\d #\f) ; (#\d . #\f)
;(cons #\d #\f) ; (#\d . #\f)
;(cons #\f) ; exception incorrecct arg count
;(cons 1/3) ; exception incorrecct arg count
;(cons '(1 . 8)) ; exception incorrecct arg count
;(cons ((lambda () #\z))) ; exception incorrecct arg count
;((lambda (v w x y z) (cons v w)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; (#\d . #\f)
;((lambda (v w x y z) (cons v w)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; (#\d . #\f)
;((lambda (v w x y z) (cons w z)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; (#\d . #\z)
;((lambda (v w x y z) (cons x y)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; (1/3 1 . 8)
;((lambda (v w x y z) (cons y z)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; ((1 . 8) . #\z)
;((lambda (v w x y z) (cons z z)) #\d #\f 1/3 '(1 . 8) ((lambda () #\z))) ; (#\z . #\z)

;denominator
;(denominator 2/3 7) ; ecxeption incorrect arg count
;(denominator 2/3) ; 3
;(denominator 2/3) ; 3
;(denominator 7) ; 1
;(denominator '(1 . 8)) ; exception not a rational number
;((lambda (x y z) (denominator x y)) 1/3 7 '(1 . 8)) ; ecxeption incorrect arg count
;((lambda (x y z) (denominator x)) 1/3 7 '(1 . 8)) ; 3
;((lambda (x y z) (denominator x)) 1/3 7 '(1 . 8)) ; 3
;((lambda (x y z) (denominator y)) 1/3 7 '(1 . 8)) ; 1
;((lambda (x y z) (denominator z)) 1/3 7 '(1 . 8)) ; exception not a rational number

;eq?

;integer?
;(integer? 7 1/3) ; ecxeption incorrect arg count
;(integer? 7) ; #t
;(integer? 7) ; #t
;(integer? 1/3) ; #f
;(integer? '(1 . 8)) ; #f
;((lambda (x y z) (integer? x y)) 7 1/3 '(1 . 8)) ; ecxeption incorrect arg count
;((lambda (x y z) (integer? x)) 7 1/3 '(1 . 8)) ; #t
;((lambda (x y z) (integer? x)) 7 1/3 '(1 . 8)) ; #t
;((lambda (x y z) (integer? y)) 7 1/3 '(1 . 8)) ; #f
;((lambda (x y z) (integer? z)) 7 1/3 '(1 . 8)) ; #f

;integer->char
;(integer->char 7 1/3) ; exception incorrect arg count
;(integer->char 7) ; #\alarm - TODO: to handle this
;(integer->char 1/3) ; exception not a valid unicode scalar value
;(integer->char 80) ; #\P
;(integer->char 80) ; #\P
;(integer->char 95) ; #\_
;(integer->char 100) ; #\d
;(integer->char 102) ; #\f
;(integer->char 105) ; #\i
;(integer->char 1) ; #\x1 - TODO: to handle this
;((lambda (x y z) (integer->char (car z))) 7 1/3 '(1 . 8)) ; #\x1 - TODO: to handle this
;((lambda (x y z) (integer->char x)) 10 1/3 '(1 . 8)) ; #\newline
;((lambda (x y z) (integer->char (cdr z))) 7 1/3 '(1 . 8)) ; #\backspace - TODO: to handle this
;((lambda (x y z w) (integer->char w)) 7 1/3 '(1 . 8) 85) ; #U

;list
;make-string
;(integer->char 1)
;(make-string 4)
;(make-string 4 #\b)
;#\d
;make-vector
;(make-vector 5 4 -7) ; ecxeption incorrect arg count
;(make-vector 5) ; #(0 0 0 0 0)
;(make-vector 5) ; #(0 0 0 0 0)
;(make-vector -7) ; Exception in make-vector: -7 is not a nonnegative fixnum
;(make-vector 5 4) ; #(4 4 4 4 4)
;(make-vector 1/2) ; Exception in make-vector: 1/2 is not a nonnegative fixnum
;((lambda (w x y z) (make-vector w x y)) 5 4 -7 1/2) ; ecxeption incorrect arg count
;((lambda (w x y z) (make-vector w)) 5 4 -7 1/2) ; #(0 0 0 0 0)
;((lambda (w x y z) (make-vector w)) 5 4 -7 1/2) ; #(0 0 0 0 0)
;((lambda (w x y z) (make-vector y)) 5 4 -7 1/2) ; Exception in make-vector: -7 is not a nonnegative fixnum
;((lambda (w x y z) (make-vector w x)) 5 4 -7 1/2) ; #(4 4 4 4 4)
;((lambda (w x y z) (make-vector z)) 5 4 -7 1/2) ; Exception in make-vector: 1/2 is not a nonnegative fixnum

;;map

;(map boolean? '(#t #t #f "bla"))

;;(apply list (cons (car (car '((1 2) (1 2) (1 2)))) (cons (car (car (cdr '((1 2) (1 2) (1 2))))) (cons (car (car (cdr (cdr '((1 2) (1 2) (1 2)))))) '()))))

;((lambda (x y) (list)) 1 '(2))

;(map list '(1 2) '(1 2) '(1 2))

;(map + '(1 2) '(1 2))


; test 139
;(map number? '(1 2 3))

;; test 140
;(map boolean? '(#t #t #f "bla"))

;; test 141
;(map (lambda (x) (if (integer? x) (char->integer (integer->char x)) 0)) '(1 2 3 #f))

;; test 142
;(map (lambda (x) (if (string? x) (string->symbol x) 0)) '("a1" "b2" 3 "cf"))

;(map number? '(1 2))

;(mapList list ('(1 2) '(1 2) '(1 2)))

;(cons (apply list (map1 car '((1 2) (1 2) (1 2)))) (mapList list (map1 cdr '((1 2) (1 2) (1 2)))))

;(cons '(1 1 1) '((2 2 2)))

;(map1 car ((1 2) (1 2) (1 2))) = (cons (car (1 2)) (map1 car ((1 2) (1 2)))) = (cons (car (1 2)) (cons (car (1 2)) (cons (car (1 2)) '())))

;(map1 car ((1 2) (1 2))) = (cons (car (1 2)) (map1 car ((1 2)))) = (cons (car (1 2)) (cons (car (1 2)) '()))

;(map1 car ((1 2))) = (cons (car (1 2)) (map1 car ())) = (cons (car (1 2)) '())

;(map1 car ()) = '()



;(mapList list (map1 cdr ((1 2) (1 2) (1 2))))

;(map1 cdr ((1 2) (1 2) (1 2))) = (cons (cdr (1 2)) (map1 cdr ((1 2) (1 2)))) = 
;(cons (cdr '(1 2)) (cons (cdr '(1 2)) (cons (cdr '(1 2)) '())))

;(map1 cdr ((1 2) (1 2))) = (cons (cdr (1 2)) (map1 cdr ((1 2)))) = 
;(cons (cdr '(1 2)) (cons (cdr '(1 2)) '()))

;(map1 cdr ((1 2))) = (cons (cdr (1 2)) (map1 cdr ())) = 
;(cons (cdr '(1 2)) '())

;(map1 cdr ()) = '()

;(cons (cons (car '(1 2)) (cons (car '(1 2)) (cons (car '(1 2)) '()))) (cons (cdr '(1 2)) (cons (cdr '(1 2)) (cons (cdr '(1 2)) '()))))


;(map list '(1 2 3) '(1 2 3) '(1 2 3))
;(map + '(1 2 3) '(1 2 3) '(1 2 3) '(1 2 3))
;(map remainder '(13 13 -13 -13) '(4 -4 -4 4))
;(map cons '(1 #t #f 3) '(1 2 3 4))
;(map boolean? '(1 #f "a"))

;(cons (+ 1 2) (apply + '(3 3)))
;(cons (+ 1 2) (- 2 5))
;(append '(1 2) '(3 (4)))
;(apply boolean? '(3 #t #\a))
;(car '((1 2) (3 (4))))
;(cdr '((1 2) (3 (4))))
;(cdr '((1 2) (3 ((4)))))


;not ; #f
;(not 5) ; #f
;(not "super") ; #f
;(not "sukka") ; #f
;(not #f) ; #t
;(not ((lambda (x) (not x)) 5)) ; #t

;null?
;(null? 69 "snir") ; ecxeption incorrect arg count
;(null? 69) ; #f
;(null? "snir") ; #f
;(null? '()) ; #t
;((lambda (x y z) (null? x y)) 69 "snir" '()) ; ecxeption incorrect arg count
;((lambda (x y z) (null? x)) 69 "snir" '()) ; #f
;((lambda (x y z) (null? y)) 69 "snir" '()) ; #f
;((lambda (x y z) (null? z)) 69 "snir" '()) ; #t

;(null? '((1 2 3) (1 2 3) (1 2 3)))

;number?
;(number? 48 4/8) ; exception incorrect arg count
;(number? 48) ; #t
;(number? 4/8) ; #t
;(number? "shihrur") ; #f
;((lambda (x y z) (number? x y)) 48 4/8 "shihrur") ; exception incorrect arg count
;((lambda (x y z) (number? x)) 48 4/8 "shihrur") ; #t
;((lambda (x y z) (number? y)) 48 4/8 "shihrur") ; #t
;((lambda (x y z) (number? z)) 48 4/8 "shihrur") ; #f

;numerator
;(numerator 2/3 7) ; ecxeption incorrect arg count
;(numerator 2/3) ; 2
;(numerator 7) ; 7
;(numerator '(1 . 8)) ; exception not a rational number
;((lambda (x y z) (numerator x y)) 2/3 7 '(1 . 8)) ; ecxeption incorrect arg count
;((lambda (x y z) (numerator x)) 2/3 7 '(1 . 8)) ; 2
;((lambda (x y z) (numerator y)) 2/3 7 '(1 . 8)) ; 7
;((lambda (x y z) (numerator z)) 2/3 7 '(1 . 8)) ; exception not a rational number

;pair?
;(pair? '(1 . 2) '(3 4 5)) ; exception incorrect arg count
;(pair? '(1 . 2)) ; #t
;(pair? '(3 4 5)) ; #t
;(pair? number?) ; #f
;((lambda (x y z) (pair? x y)) '(1 . 2) '(3 4 5) number?) ; exception incorrect arg count
;((lambda (x y z) (pair? x)) '(1 . 2) '(3 4 5) number?) ; #t
;((lambda (x y z) (pair? y)) '(1 . 2) '(3 4 5) number?) ; #t
;((lambda (x y z) (pair? z)) '(1 . 2) '(3 4 5) number?) ; #f

;procedure?
;(procedure? '(1 . 2) procedure?) ; exception incorrect arg count
;(procedure? '(1 . 2)) ; #f
;(procedure? procedure?) ; #t
;(procedure? number?) ; #t
;((lambda (x y z) (procedure? x y)) '(1 . 2) procedure? number?) ; exception incorrect arg count
;((lambda (x y z) (procedure? x)) '(1 . 2) procedure? number?) ; #f
;((lambda (x y z) (procedure? y)) '(1 . 2) procedure? number?) ; #t
;((lambda (x y z) (procedure? z)) '(1 . 2) procedure? number?) ; #t

;rational?
;(rational? 2/3 7) ; exception incorrect arg count
;(rational? 2/3) ; #t
;(rational? 7) ; #t
;(rational? number?) ; #f
;((lambda (x y z) (rational? x y)) 2/3 7 number?) ; exception incorrect arg count
;((lambda (x y z) (rational? x)) 2/3 7 number?) ; #t
;((lambda (x y z) (rational? y)) 2/3 7 number?) ; #t
;((lambda (x y z) (rational? z)) 2/3 7 number?) ; #f

;remainder ; closure
;(remainder 13 4 5) ; exception incorrect arg count
;(remainder 13 4) ; 1
;(remainder -13 4) ; -1
;(remainder -13 -4) ; -1
;(remainder 13 -4) ; 1
;(remainder 13 "a") ; exception not an integer
;(remainder "b" 4) ; exception not an integer
;(remainder 13 0) ; exception undefined for 0
;((lambda (x y z) (remainder x y z)) 13 4 5) ; exception incorrect arg count
;((lambda (x y z) (remainder x y)) 13 4 5) ; 1
;((lambda (x y z) (remainder x y)) -13 4 5) ; -1
;((lambda (x y z) (remainder x y)) -13 -4 5) ; -1
;((lambda (x y z) (remainder x y)) 13 -4 5) ; 1
;((lambda (x y z) (remainder x y)) 13 "a" 5) ; exception not an integer
;((lambda (x y z) (remainder x y)) "b" 4 5) ; exception not an integer
;((lambda (x y z) (remainder x y)) 13 0 5)  ; exception undefined for 0

;set-car!
;(define x1 '(1 . 2))
;(define x2 '("sukka" . "blyat"))
;(define x3 '("huyassa" . "gopnik"))
;(set-car! x1 3)
;x1
;(set-car! x2 "nahui")
;(set-car! x1 "blyat")
;x1
;x2
;x3

;set-cdr!
;(define x1 '(1 . 2))
;(define x2 '("sukka" . "blyat"))
;(define x3 '("huyassa" . "gopnik"))
;(set-cdr! x1 3)
;(set-cdr! x2 "nahui")
;(set-cdr! x3 "blyat")
;x1
;x2
;x3

;string-length
;string-ref
;string-set!
;string->symbol

;string?
;(string? '(1 . 2) procedure?) ; exception incorrect arg count
;(string? '(1 . 2)) ; #f
;(string? "seagulls") ; #t
;(string? number?) ; #f
;((lambda (x y z) (string? x y)) '(1 . 2) "seagulls" number?) ; exception incorrect arg count
;((lambda (x y z) (string? x)) '(1 . 2) "seagulls" number?) ; #f
;((lambda (x y z) (string? y)) '(1 . 2) "seagulls" number?) ; #t
;((lambda (x y z) (string? z)) '(1 . 2) "seagulls" number?) ; #f

;symbol?
;symbol->string
;vector

;vector-length
;(vector-length '#(10 20 30 40) 5) ; exception incorrect arg count
;(vector-length 5) ; exception not a vector
;(vector-length '#(10 20 30 40)) ; 4
;((lambda (x y) (vector-length x y)) '#(10 20 30 40) 5) ; exception incorrect arg count
;((lambda (x y) (vector-length y)) '#(10 20 30 40) 5) ; exception not a vector
;((lambda (x y) (vector-length x)) '#(10 20 30 40) 5) ; 4

;vector-ref
;(vector-ref '#(10 20 30 40)) ; exception incorrect arg count
;(vector-ref 3) ; exception incorrect arg count
;(vector-ref '#(10 20 30 40) 3 "f") ; exception incorrect arg count
;(vector-ref 4 3) ; exception not a vector
;(vector-ref '#(10 20 30 40) "f") ; exception not a valid index
;(vector-ref '#(10 20 30 40) 4) ; exception not a valid index
;(vector-ref '#(10 20 30 40) 3) ; 40
;((lambda (w x y z) (vector-ref w)) '#(10 20 30 40) 3 4 "f") ; exception incorrect arg count
;((lambda (w x y z) (vector-ref x)) '#(10 20 30 40) 3 4 "f") ; exception incorrect arg count
;((lambda (w x y z) (vector-ref w x z)) '#(10 20 30 40) 3 4 "f") ; exception incorrect arg count
;((lambda (w x y z) (vector-ref y x)) '#(10 20 30 40) 3 4 "f") ; exception not a vector
;((lambda (w x y z) (vector-ref w z)) '#(10 20 30 40) 3 4 "f") ; exception not a valid index
;((lambda (w x y z) (vector-ref w y)) '#(10 20 30 40) 3 4 "f") ; exception not a valid index
;((lambda (w x y z) (vector-ref w x)) '#(10 20 30 40) 3 4 "f") ; 40
;(define a '#(1 2 3)) ; nothing
;a ; #(1 2 3)
;(vector-ref a 2) ; 3
;a ; #(1 2 3)

;vector-set! ; closure
;(define a '#(1 2 3)) ; nothing
;a ; '#(1 2 3)
;;(vector-set! a 2) ; Exception: incorrect argument count in call
;;(vector-set! a 3 10) ; Exception in vector-ref: 3 is not a valid index for #(1 2 3)
;;(vector-set! a -7 10) ; Exception in vector-ref: -7 is not a valid index for #(1 2 3)
;(vector-set! a 2 10) ; nothing
;a ; '#(1 2 10)
;;(define b '(1 2 3)) ; nothing
;;b ; '(1 2 3)
;;(vector-set! b 2 10) ; Exception in vector-ref: (1 2 3) is not a vector
;(define c '#(1 2 3)) ; nothing
;c ; '#(1 2 3)
;;((lambda (v w x y z) (vector-set! v w)) c 2 3 -7 10) ; Exception: incorrect argument count in call
;;((lambda (v w x y z) (vector-set! v x z)) c 2 3 -7 10) ; Exception in vector-ref: 3 is not a valid index for #(1 2 3)
;;((lambda (v w x y z) (vector-set! v y z)) c 2 3 -7 10) ; Exception in vector-ref: -7 is not a valid index for #(1 2 3)
;((lambda (v w x y z) (vector-set! v w z)) c 2 3 -7 10); nothing
;c ; '#(1 2 10)
;3
;10

;(make-string 4 #\f)
;(make-string 5 #\0)


;((car '(+ 2 1)) 1 2)
;(define s "abcde")
;s
;(string-set! s 3 #\g)
;s
;vector?
;(vector?  '#(1 2 3) '(1 2 3)) ; exception incorrect arg count
;(vector? '#(1 2 3)) ; #t
;(vector? '(1 2 3)) ; #f
;(vector? "seagulls") ; #f
;((lambda (x y z) (vector? x y)) '#(1 2 3) '(1 2 3) "seagulls") ; exception incorrect arg count
;((lambda (x y z) (vector? x)) '#(1 2 3) '(1 2 3) "seagulls") ; #t
;((lambda (x y z) (vector? y)) '#(1 2 3) '(1 2 3) "seagulls") ; #f
;((lambda (x y z) (vector? z)) '#(1 2 3) '(1 2 3) "seagulls") ; #f

;zero?
;(zero? 0 7) ; exception incorrect arg count
;(zero? 0) ; #t
;(zero? 7) ; #f
;(zero? "seagulls") ; exception not a number
;((lambda (x y z) (zero? x y)) 0 7 "seagulls") ; exception incorrect arg count
;((lambda (x y z) (zero? x)) 0 7 "seagulls") ; #t
;((lambda (x y z) (zero? y)) 0 7 "seagulls") ; #f
;((lambda (x y z) (zero? z)) 0 7 "seagulls") ; exception not a number
;;; primitives tests


;;; if, or, sequence
;(if #f 2 4)	;4
;(if 1 2 3)		;2
;(or 1 2 3 4)	;1
;(if 1 (if 2 (if 3 4) 5)) ; 4
;(or #f (or #f 1 2) 3 4) ; 1
;(or #f #f #f)	;#f
;(or)			;#f
;(begin 1 2 3 4 5 6 7 8 9 10) ;10
;(or #f (if #f 2 '("avi" . "king")) 30) ;'( "avi" . "king")
;;; if, or, sequence


;;; define, fvar
;(define a 9) ; nothing
;(define x1 5) ; nothing
;a ; 9
;b ; Exception: attempt to use undefined
;x1 ; 5
;;; define, fvar


;;; lambda-simple, applic
;(lambda (a) (if #t 4 a)) ; a closure
;(lambda (x) (lambda (a) (if #t 4 a))) ; a closure
;(lambda (y) (lambda (x) (lambda (a) (if #t 4 a)))) ; a closure
;(((lambda (x) (lambda (y) (or x y))) 3) 4) ; 3
;9
;('(4 . 6) 1 2 3) ; exception not a closure
;8
;(((((lambda (x) (lambda (y) (lambda (z) (lambda (w) (or w x y z))))) 1) 2) 3) 17) ; 17
;(((((lambda (x) (lambda (y) (lambda (z) (lambda (w a b c) (or w x y z a b c))))) 66) 2) 3) #f 200 300 400) ; 66
;(((((lambda (a) (lambda (b c) (lambda (d e f) (lambda (g h i j) (or g a b d))))) #f) 11 12) 21 22 23) #f 32 33 34) ; 11
;(define a (lambda (z) (lambda (w) (or w x y z)))) ; nothing
;(((((lambda (x) (lambda (y) a)) 1)  2) 3) 17) ; 17
;(define b (lambda (x) x)) ; nothing
;b ; closure
;(b 6) ; 6
;(b 5 7 5 5) ; exception incorrect arg count
;56
;c ; exception attemt to use unbounded
;3
;3
;444
;(r 4 5) ; exception attempt to use unbounded
;444
;;; lambda-simple, applic

;(append '(5 6) '(7 8 (9)))

;(append '(1 2) (list 3 '(4)))

;(+ 1/2 -1/2 -1/2 -1/2 3 -4 1/4 -1/3 1/7)

;(- -1 1/2 -3 -4 -5 -6/5 2/3)

;(* -1/2 -2 -3 -4 1/7 -5)

;(/ 1/4 1/2 -3 2/5 7 -1/21)

;(> 2 1 1/4 1/2 -1 -4)
;(> -1 -3/2 -2)
;(< 0 1 2 5/2 10/3)
;(< -5 -3 -1/4 -1/3)

;(= -1 -1 (- 2 3) (* 1/2 -2) -1)

;;;; lambda-opt
;(lambda (a b . c) (or a b)) ; procedure
;((lambda (a b . c) (or a b)) 49 2 3 4) ; 49
;((lambda (a b . c) (or a b)) 34) ; exception incorrect arg count
;((lambda (a b . c) a) 49 2 3 4) ; 49
;((lambda (a b . c) b) 49 2 3 4) ; 2
;((lambda (a b . c) c) 49 2 3 4) ; (3 4)
;((lambda (a b . c) c) 49 2) ; ()
;((lambda (a b . c) c) 49) ; exception invalid arg count
;((lambda c c) 1 2 3 4) ; (1 2 3 4)
;((lambda (a b . c) ((car c) (car (cdr c)))) 70 71 integer? 69) ; t
;((lambda (a b . c) ((car c) (car (cdr c)))) 70 71 (lambda (x) (+ 10 x)) 39) ; 49
;;;; lambda-opt

;(list 1 2 3)

;(list 1 2 3 '(4 5))

;(+ 1)
;(+ 1 2 3)
;(- 1 2 3)
;(- (+ 1 2 3) 5)
;(> 3 2 1)
;(> 1 2 3)
;(< 3 2 1)
;(< 1 2 3)
;(* 1 2 3)
;(* (+ 4 5) (- 2 6))
;(* 1)
;(= 1 1 1 1)
;(= 1 1 1 2)
;;; tc-applic
;((lambda (x y) ((lambda (a) (remainder a y)) 13)) 6 4)
;;; tc-applic

;;; set, box, box-set, box-get
;(((lambda (x y z) (lambda (a b c) (set! y 7) (or x y a c))) 10 20 30) 100 200 300) ; 10
;;; set, box, box-set, box-get

;((lambda (a) (+ 5 a)) 4)
;(define adder3
;	; this is a comment in the middle of the function definition!
;	(lambda (x) (+ 3 x)))
;(adder3 4)
;##6-8
;##6-##(adder3 8)
;(* #;(/ 5 0) 8 9)
;(- 2)
;-2
;(+ 1 2)