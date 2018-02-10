; test 1
23

; test 2
(- 2)

; test 3
-1

; test 4
(/ 35 7)

; test 5
(* 5 7)

; test 6
(+ 53242653 35463560)

; test 7
(+ (* 564 5) (- (+ 4 5)))

; test 8
(- ( - ( - ( - ( - ( - ( - ( - (- 5)))))))))

; test 9
((lambda (a) ( + a 7)) 5)

; test 10
((lambda (a b) (a ( b (a 5 6) 8) 11)) + -)

; test 11
((lambda (a b) (if (number? a) (make-string a) b)) #t 6)

; test 12
 ((lambda (a b c) (if (= a c) (+ a c) (- b c 4))) 1 2 3)

; test 13
((lambda (a)
        (begin
          (define pi 3)
          (define e 2)
          (if (> a 64)
              (+ pi e)
              (* pi e)
              )
          )
        ) 10)

; test 14
(define sum (lambda (x) (if (= x 0) 0 (+ x (sum (- x 1))))))
 (sum 60)

; test 15
(define rec (lambda (func1 func2 init param num)
                (if (= 0 num)
                    init
                    (func1 (rec func1 func2 (func2 2 init param) param (- num 1))
                      )
                    )
                )
    )
(rec - + 5 7 20)

; test 16
(((lambda (x)
      (begin
        (define func (lambda (y)
                       (x y 5)
                       )
          )
        func)
      ) +) 65)

; test 17
((lambda (x)
      (begin
        (define func1 (lambda (a)
                        (+ a 4)
                        )
          )
        (define func2 (lambda (a)
                        (* a 4)
                        )
          )
        (func1 (func2 (func1 x))))) 11)

; test 18
((lambda (f1 f2 f3 x)
      (begin
        (define pi 3)
        (f1 (f2 (f3 pi x) x) x)
        )
      ) + - * 9)

; test 20
(define odd? (lambda (x)
                 (begin
                   (define even?
            (lambda (x)
              (or (= x 0) (odd? (- x 1)))))
                   (if (even? x) #f #t)
                   )
                 )
    )
(odd? 129)

; test 21
((lambda (f1 f2 input1 input2 input3 ifval)
      (if (ifval input2 input3)
      (f1 (f2 input1 5) 40)
      (begin
        (set! f2 f1)
        (f1 (f2 input1 5) 40)
        )
      )
 ) * + 5 7 -8 >)

; test 22
((lambda (f1 f2 input1 input2 input3)
    (begin
      (define f (lambda () (f1 (f2 input1 input2) input3)))
      (f)
      )
    ) - - 1 2 3)

; test 23
((lambda (f1 f2 input1 input2 input3 ifval)
          (begin
            (define f (lambda () (f1 (f2 input1 5) 40)))
           (if (ifval input2 input3)
               (f)
               (begin
                 (set! f2 f1)
                 (f)
                 )
               )
           )
        ) * * 1 2 3 =)

; test 24
(((lambda (x y) (lambda () (+ x y))) 56 65))

; test 25
(((lambda (x y) (lambda () (+ x y))) ((lambda (a) (* a a)) 500) 2))

; test 26
(((lambda (x y) (lambda () (x 89 y))) (lambda (a b) (* a b)) 2))

; test 27
((lambda (x)
      (begin
        (define f1 (lambda (a) (+ a a)))
        (define f2 (lambda (a) (* a a)))
        (if (eq? (f1 x) (f2 x))
            'eq!'
            'no!
            )
        )
      ) 2)

; test 28
((lambda (f1 f2)
      (if (eq? f1 f2)
          'eq!
          'no!
          )
      ) + -)

; test 29
(define factorial
    (lambda(n)
      (if (= n 0)
        1
        (* n (factorial (- n 1))))))
(factorial 6)

; test 30
(define fibonacci
        (lambda (n)
          (if (< n 2)
              1
              (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
(fibonacci 11)

; test 31
(define (equal? x y)
    (if (not (pair? x))
        (eq? x y)
        (and (equal? (car x) (car y))
             (equal? (cdr x) (cdr y)))))
(equal? (cons 1 2) (cons 1 3))

; test 32
(define (variable? x) (symbol? x))
(variable? #t)

; test 33
((lambda (x y)
      (cond ((= x y) #t)
            ((> x y) 'x>y)
            ((and (> (+ x y) 10) (> (* x y) 40)) 'string)
            )
      ) 111 11)

; test 34
((lambda (a) (if (string? a) (string->symbol a))) "a23")

; test 35
 (define (=number? exp num)
  (and (number? exp) (= exp num)))
(=number? 5 1)

; test 37
(define (a x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set)(a x (cdr set))))))
	
(a 3 (cons 5 4))

; test 38
(define (expmod a b m) 
  (cond ((= b 0) 1)
	((= (remainder b 2) 0) (remainder (expmod (remainder (* a a) m) (/ b 2) m) m))
	(else (remainder (* a (expmod a (- b 1) m)) m))))
   
(expmod 5 13 1)

; test 39
(define (a str)
    (define (b x sum)
      (cond
        ((= (string-length str) x) sum)
        (else (b (+ x 1) (+ (char->integer (string-ref str x)) (* 256 sum))))))
    (b 0 0))
(a "hello")

; test 40
(define (b set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((s1 (car set1))
           (s2 (car set2)))
       (cond
       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
(b '(1 2 3) '(4 5 6))

; test 41
(let ((z 2))
  (define x (lambda (x) (lambda (y z) (y x))))
  (((x (lambda () z)) (lambda (z) z) 3))
)

; test 42
((lambda (z)
     (define x (lambda (xv) (lambda (y z) (y xv))))

     (((x (lambda () z)) (lambda (zv) zv) 3))
     ) 14)

; test 43
(define a 'hello)
a

; test 44
(define b (string-length "world"))
b

; test 45
(define loop (lambda (num func param)
                 (if (zero? num)
                     param
                     (loop (- num 1) func (func param))
                     )
                 )
    )
(loop 7 (lambda (x) (+ x x)) 43)

; test 46
(define loop2 (lambda (num func param)
                  (if (zero? num)
                      param
                      (func (loop2 (- num 1) func param)
                        )
                      )
                  )
    )
(loop2 7 (lambda (x) (+ x x)) 3)

; test 47
(define loop3 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (if (= i num)
                                          param
                                          (begin
                                            (set! i (+ i 1))
                                            (func param)
                                            (subloop)
                                            )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop3 7 (lambda (x) (+ 8 x)) 123)

; test 48
(define loop4 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (if (= i num)
                                          param
                                          (begin
                                            (set! i (+ i 1))
                                            (set! param (func param))
                                            (subloop)
                                            )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop4 7 (lambda (x) (+ 4 x)) 1213)

; test 49
(define loop5 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (cond ((= i num) param)
                                        (else
                                          (begin
                                            (set! i (+ i 1))
                                            (set! param (func param))
                                            (subloop)
                                            )
                                          )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop5 21 (lambda (x) (* 3 x)) 123)

; test 50
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
c

; test 51
(define (c set1 set2)
  (define s1 (car set1))
  (define s2 (car set2))
  (cond
    ((or (null? set1) (null? set2)) (append set1 set2))
    (else
       (cond
       ((= s1 s2) (cons s1 (cons (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (cons set1 (cdr set2))))
       ((< s1 s2) (cons s1 (cons (cdr set1) set2)))))))	   
(c '(1 2 3) '(4 6))

; test 52
(define (accumulate op init lst)
    (if (null? lst)
        init
        (op (car lst) (accumulate op init (cdr lst)))))
(accumulate * 2 '(1 2 3 4 5 6 7 8 9))

; test 53
(define f1 (lambda (x) x))
(f1 2)

; test 54
(define f2 (lambda (o a b) (o a b)))
(f2 + 5 6)

; test 55
(define f3 (lambda () (begin
                         (define foo (lambda (x) (x 5 6)))
                         (define bar (lambda (a b) (+ a b)))
                         (foo bar)
                         )
               )
    )
(f3)

; test 56
(define f4 (lambda (z) (begin
                         (define foo (lambda (x y) (x y 5 6)))
                         (define bar (lambda (op a b) (op a b)))
                         (foo bar z)
                         )
               )
    )
(f4 *)

; test 57
(define f5 (lambda () (begin
                           (define foo (lambda (x y) (x y 5 6)))
                           (define bar (lambda (op a b) (op a b)))
                           (define oop +)
                           (foo bar oop)
                           )
                 )
      )
(f5)

; test 59
(let ((square (lambda (x) (* x x)))) 33)

; test 60
(define fun1 (lambda ()
                 (begin
                   (+ 2 1)
                   (+ 3 4)
                   )
                 )
    )
(fun1)

; test 61
 (define fun2 (lambda (x)
                 (begin
                   (set! x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun2 45)

; test 62
(define fun3 (lambda ()
                 (begin
                   (define x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun3)

; test 63
(define fun4 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define x (+ (f) 3 4))
                   x
                   )
                 )
    )
(fun4)

; test 64
(define fun5 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   g
                   )
                 )
    )
((fun5))

; test 65
(define fun6 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   (g)
                   )
                 )
    )
(fun6)

; test 66
(define fun7 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ 2 1)))
                   (define g (lambda () (f 3 4)))
                   (g)
                   )
                 )
    )
(fun7)

; test 67
(define fun8 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (+ (g f) (g *) (g -) (g +))
                   )
                 )
    )
(fun8)

; test 68
(define fun9 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (define t (lambda (a) (
                                         if (eq? a *)
                                             *
                                             a)))
                   (+ (g f) (g (t *)) (g -) (g (t -)))
                   )
                 )
    )
(fun9)

; test 70
(define fool (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (+ (f) (f) (f)))
                            a)
                 )
    )
(fool 2 3)

; test 71
(define foo2 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (f))
                            (set! a (f))
                            (set! a (f))
                            a)
                 )
    )
(foo2 50 60)

; test 72
(define foo3 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (set! a (f a))
                            (set! a (f a))
                            (set! a (f a))
                            a)
                 )
    )
(foo3 43 3)

; test 73
(define foo4 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            a)
                 )
    )
(foo4 31 3)

; test 74
(define foo5 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            (g)
                            (f x))
                 )
    )
(foo5 11 4)

; test 75
(define foo6 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b a x y)))
                            (define g (lambda () (set! x 5)))
                            (define t (lambda () (set! a y)))
                            (g)
                            (t)
                            (f x))
                 )
    )
(foo6 101 3)

; test 76
(define foo7 (lambda (x y) (
                            begin
                            (set! y x)
                            (set! x y)
                            (+ y x))
                 )
    )
(foo7 1 3)

; test 77
(define foo8 (lambda (x y) (
                            begin
                            (define y x)
                            (+ y x))
                 )
    )
(foo8 2 3)

; test 78
(define foo9 (lambda (x y) (
                            begin
                            (define y x)
                            (eq? y x))
                 )
    )
(foo9 12 8)

; test 79
(define foo10 (lambda (x y) (
                            begin
                            (set! y x)
                            (eq? y x))
                 )
    )
(foo10 12 12)

; test 80
(define bar1 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define num b)
                  (cond ((eq? num 0) a)
                    (else
                      (bar1 (rec1 a) (- b 1)))
                    )
                  )
                )
    )
	
(bar1 4 3)

; test 81
(define bar2 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (set! b (- b 1))
                  (cond ((eq? b 0) a)
                    (else
                      (bar2 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar2 4 5)

; test 82
(define bar3 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (bar3 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar3 6 2)

; test 83
(define bar4 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (rec1 (bar4 a b)))
                    )
                  )
                )
    )
	
(bar4 5 2)

; test 84
(define bar5 (lambda (a b)
                  (begin
                    (define rec1 (lambda (b) (* b b)))
                    (define rec2 (lambda (b) (- b 1)))
                    (set! b (rec2 b))
                    (if (eq? b 0) a
                        (rec1 (bar5 a b)))

                    )
                  )
    )
	
(bar5 5 3)

; test 85
(define bar6 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (rac b)
                    )
                  )
    )
	
(bar6 1 2 3)

; test 86
(define bar7 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar7 (rac a) (- b 1) c))
                    )
                  )
    )
	
(bar7 5 2 6)

; test 87
(define bar8 (lambda (a b c d)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar8 (rac a) (- b 1) c d))
                    )
                  )
    )
	
(bar8 1 5 2 6)

; test 88
(define bar9 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar9 (rac a) (- b 1) c d e))
                    )
                  )
    )
(bar9 2 7 3 3 10)

; test 89
(define bar10 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar10 (rac (rac1 (rac2 a))) (- b 1) c d e))
                    )
                  )
    )
(bar10 1 5 4 6 1)

; test 90
(((lambda (x)  
    (lambda (z)
      (* x x))) 4) 5)

; test 91
((lambda () (+)))

; test 92
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 55) 66)

; test 93
((((lambda () (lambda (aa) (lambda (bb) (- aa bb))))) 55) 66)

; test 94
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 30) 4)

; test 95
((lambda (a b c d) (a (b (c d)))) + - * 4)

; test 96
(define tar1 (lambda (a)
                (begin
                  (define r a)
                  (if (= r 1) 1 (+ 1 (tar1 (- r 1)))))))
				  
(tar1 50)

; test 97
(define tar2 (lambda (a)
                (begin
                  (define r a)
                  (cond ((= r 1) 1)
                   (else (* 2 (tar2 (- r 1))))))))
				  
(tar2 5)

; test 98
(define bin2dec (lambda (x y)
                    (begin
                      (define rem (remainder x 10))
                      (set! y (+ (* y 2) (* y rem)))
                      (if (= x 0)
                          y
                          (bin2dec (remainder x 10) y)
                          )
                      )
                    )
    )
(bin2dec 1000 2)

; test 99
(define rem (lambda (x)(remainder x 10)))
(rem 443)

; test 100
(define f (lambda (b) (/ 200 b)))
(f 4)

; test 101
((lambda (a b) (cons a b)) 5 4)

; test 103
(boolean? (procedure? (lambda () (make-string 5))))

; test 104
((lambda (a) (boolean? a)) #t)

; test 105
((lambda (a) (if (char? a) (char->integer a) (if (integer? a) (integer->char a) a))) #\x50)

; test 106
(pair? (cons 4 6))

; test 107
((lambda (a b) (cons a b)) 55 6)

; test 108
(pair? (lambda (a b) (cons a b)))

; test 109
((lambda (a b) (pair? (cons a b))) 1234 5678)

; test 110
(procedure? (lambda (a b) (cons a b)))

; test 111
(zero? 5)

; test 112
(not (zero? 5))

; test 113
(define a (lambda (b) (rational? b)))
(a 56)

; test 114
(define a (lambda (b) (not (rational? b))))
(a 56)

; test 115
(denominator (/ 10 2))

; test 116
(numerator 100/50)

; test 117
(define a (lambda (x y) (if (not (zero? x)) (denominator (/ y x)) (numerator y))))
(a 0 5)

; test 119
(define x (lambda (a b) (if (> (string-length a) b) (string-ref a b) a)))
(char->integer (x "hello" 3))

; test 120
(define x (lambda (a b c) (if (> (string-length a) b) (string-set! a b c) a)))
(string->symbol (x "hello" 30 #\r))

; test 121
(string->symbol ((lambda (b) (symbol->string b)) 'a))

; test 128
(define f (lambda (p x) (begin
                            (set-car! p x)
                            p)))
(f (cons 4 5) 444)

; test 129
(define f (lambda (p x) (begin
                            (set-cdr! p x)
                            p)))
(f (cons 4 5) 444)

; test 130
(apply (lambda (a) (* a a)) '(2))

; test 131
(let ((str 'hello))
    (set! f1 (lambda () str))
    (set! f2 (lambda () (string->symbol str)))
	str
    )

; test 132
(let ((x 2) (y 3))
  (let* ((x 7)
         (z (+ x y)))
    (* z x)))

; test 133
(let* ((x 2) (y 3))
    (let ((x 7)
           (z (+ x y)))
      (* z x)))

; test 134
(letrec ((x 2) (y 3))
    (let ((x 7)
           (z (+ x y)))
      (* z x)))

; test 135
((lambda (ls1 ls2) (append ls1 ls2)) '(1 2 3) '(q w e))

; test 136
(define bla (lambda (x y) (append (list x 2) (list 2 y))))
(bla '(1 2 3) '(q w e))

; test 137
(apply + (list 1 3 2))

; test 138
((lambda (list) (apply (lambda (x . y) (+ x 3)) list)) (list 1 3 2))

; test 139
(map number? '(1 2 3))

; test 140
(map boolean? '(#t #t #f "bla"))

; test 141
(map (lambda (x) (if (integer? x) (char->integer (integer->char x)) 0)) '(1 2 3 + #f))

; test 142
(map (lambda (x) (if (string? x) (string->symbol x) 0)) '("a1" "b2" 3 + "cf"))

; test 143
((lambda (int) (integer? int))4)

; test 144
(map number? '(1 2 '3))

; test 145
(string? '1)

; test 147
((lambda (ch) (if (char? ch) (char->integer ch))) #\x666)

; test 148
((lambda (int) (if (boolean? (char? (integer->char int))) 'ok)) 5)

; test 149
((lambda (str)
   (if (string? str)
       (begin
	 (string-set! str 1 (integer->char 66))
	 str))) "ssss")

; test 150
((lambda (sym int)
   (if (symbol? sym) (begin
		       (set! a (symbol->string sym))
		       (string-set! a 2 (integer->char int))
		       a))) 'abc 33)

; test 151
((lambda (list) (begin
		(set-car! (car (cdr list)) (cons 1 2))
		 list)) (list 1 (cons 22 66) 3 4))

; test 152
((lambda (list) (begin
		(set-cdr! (cdr list) (cons 1 2))
		list)) (list 1 2 3 4))

; test 153
(let* ((x 1)
         (y 2)
         (z 3))
    (+ x y z)
    )

; test 154
((lambda (x y) (
                 let* ((a x)
                       (b y)
                       )
                 (* a a b))
    ) 44 55)

; test 155
(letrec ((loop (lambda (i a)
		 (set! a (+ (* 10 a) i))
		 (if (< i 10)
		     (loop (+ i 1) a)
		     a))))
  (loop 0 0))

; test 156
(define func (lambda (lst num) (
                                  letrec ((loop
                                             (lambda (i a)
                                               (cond ((null? i)
                                                      #f)
                                                 ((eq? (car i) a) #t)
                                                 (else
                                                   (loop (cdr i) a)))
                                               )))
                                    (loop lst num)))
                 )
(func (list 1 2 3) 5)

; test 157
(quasiquote (0 1 2))

; test 158
(quasiquote (0 (unquote (+ 1 2)) 4))

; test 159
(quote (1 a (* 4)))

; test 160
(define q (quote (bla (((s ) s )sd ))))
q

; test 161
(quasiquote (1 2 (unquote (+ 3 4))))

; test 162
(quasiquote ( a 3 4 (unquote (* 4 3 2 1))))

; test 164
`(unquote (quote (3 4 5)))

; test 166
(let* ((a 1) (b 1) (c (* a b)))
   c)

; test 167
(define (lst . x) x)
(lst 1 2 3 4 5 6)

; test 168
(define (func . numbers)
    (if (null? numbers)
        0
        (+ (car numbers) (apply func (cdr numbers)))))
(func 9 8 7 6 5 4)

; test 169
(define (f . x) (apply + x))
(f 5 4 8 6)

; test 172
5

; test 174
(define (plusminus . l)
    (if (null? l) 0
        (if (null? (cdr l)) (car l)
        (+ (- (car l) (car (cdr l))) (apply plusminus (cdr (cdr l)))))))
(plusminus 5 4 8 6 7 2 3 0 5 4 8 9 0)

; test 175
(define (less-than  . l)
     (cond
       ((null? l) #t)
       ((null? (cdr l)) #t)
       ((< (car l) (car (cdr l))) (apply less-than  (cdr l)))
       (else #f)))
	   
(less-than 5 4 8 9 6 2 5 4 4 44)

; test 176
(procedure? (lambda () (make-string 5)))

; test 177
(((((lambda (a)
      (lambda (b)
        (((lambda (a) (lambda (b) ((a b) (lambda (x) (lambda (y) y)))))
	  ((lambda (n)
	     ((n (lambda (x) (lambda (x) (lambda (y) y))))
	      (lambda (x) (lambda (y) x))))
	   (((lambda (a)
	       (lambda (b)
		 ((b (lambda (n)
		       ((lambda (p) (p (lambda (a) (lambda (b) b))))
			((n (lambda (p)
			      (((lambda (a)
				  (lambda (b) (lambda (c) ((c a) b))))
				((lambda (n)
				   (lambda (s)
				     (lambda (z) (s ((n s) z)))))
				 ((lambda (p)
				    (p (lambda (a) (lambda (b) a))))
				  p)))
			       ((lambda (p)
				  (p (lambda (a) (lambda (b) a))))
				p))))
			 (((lambda (a)
			     (lambda (b) (lambda (c) ((c a) b))))
			   (lambda (x) (lambda (y) y)))
			  (lambda (x) (lambda (y) y)))))))
		  a)))
	     a)
	    b)))
	 ((lambda (n)
	    ((n (lambda (x) (lambda (x) (lambda (y) y))))
	     (lambda (x) (lambda (y) x))))
	  (((lambda (a)
	      (lambda (b)
		((b (lambda (n)
		      ((lambda (p) (p (lambda (a) (lambda (b) b))))
		       ((n (lambda (p)
			     (((lambda (a)
				 (lambda (b) (lambda (c) ((c a) b))))
			       ((lambda (n)
				  (lambda (s)
				    (lambda (z) (s ((n s) z)))))
				((lambda (p)
				   (p (lambda (a) (lambda (b) a))))
				 p)))
			      ((lambda (p)
				 (p (lambda (a) (lambda (b) a))))
			       p))))
			(((lambda (a)
			    (lambda (b) (lambda (c) ((c a) b))))
			  (lambda (x) (lambda (y) y)))
			 (lambda (x) (lambda (y) y)))))))
		 a)))
	    b)
	   a)))))
    ((lambda (n)
       ((lambda (p) (p (lambda (a) (lambda (b) b))))
	((n (lambda (p)
	      (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
		((lambda (n) (lambda (s) (lambda (z) (s ((n s) z)))))
		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p)))
	       (((lambda (a)
		   (lambda (b)
		     ((b (a (lambda (a)
			      (lambda (b)
				((a (lambda (n)
				      (lambda (s)
					(lambda (z) (s ((n s) z))))))
				 b)))))
		      (lambda (x) (lambda (y) y)))))
		 ((lambda (p) (p (lambda (a) (lambda (b) a)))) p))
		((lambda (p) (p (lambda (a) (lambda (b) b)))) p)))))
	 (((lambda (a) (lambda (b) (lambda (c) ((c a) b))))
	   (lambda (x) x))
	  (lambda (x) x)))))
     (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
   (((lambda (a)
       (lambda (b)
	 ((b (a (lambda (a)
		  (lambda (b)
		    ((a (lambda (n)
			  (lambda (s) (lambda (z) (s ((n s) z))))))
		     b)))))
	  (lambda (x) (lambda (y) y)))))
     (((lambda (a)
	 (lambda (b)
	   ((b (a (lambda (a)
		    (lambda (b)
		      ((a (lambda (n)
			    (lambda (s) (lambda (z) (s ((n s) z))))))
		       b)))))
	    (lambda (x) (lambda (y) y)))))
       ((lambda (x) (lambda (y) (x (x (x y)))))
	(lambda (x) (lambda (y) (x (x y))))))
      (lambda (x) (lambda (y) (x (x (x y)))))))
    (lambda (x) (lambda (y) (x (x (x (x (x y)))))))))
  #t)
 #f)

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

; test 180
(define positive? (lambda (n) (> n 0)))
(define even?
  (letrec ((even-1?
	    (lambda (n)
	      (or (zero? n)
		  (odd-2? (- n 1) 'odd-2))))
	   (odd-2?
	    (lambda (n _)
	      (and (positive? n)
		   (even-3? (- n 1) (+ n n) (+ n n n)))))
	   (even-3?
	    (lambda (n _1 _2)
	      (or (zero? n)
		  (odd-5? (- n 1) (+ n n) (* n n) 'odd-5 'odder-5))))
	   (odd-5?
	    (lambda (n _1 _2 _3 _4)
	      (and (positive? n)
		   (even-1? (- n 1))))))
    even-1?))

(even? 100)

; test 181
(let ((a 1))
  (let ((b 2) (c 3))
    (let ((d 4) (e 5) (f 6))
      (= 720 (* a b c d e f)))))

; test 182
(define sum (lambda (n) (/ (* n (+ n 1)) 2)))
(sum 300)

; test 183
(define with (lambda (s f) (apply f s)))
(define fact
  (letrec ((fact-1
	    (lambda (n r)
	      (if (zero? n)
		  r
		  (fact-2 (- n 1)
			  (* n r)
			  'moshe
			  'yosi))))
	   (fact-2
	    (lambda (n r _1 _2)
	      (if (zero? n)
		  r
		  (fact-3 (- n 1)
			  (* n r)
			  'dana
			  'michal
			  'olga
			  'sonia))))
	   (fact-3
	    (lambda (n r _1 _2 _3 _4)
	      (if (zero? n)
		  r
		  (fact-1 (- n 1)
			  (* n r))))))
    (lambda (n)
      (fact-1 n 1))))
(fact 10)

; test 184
(define with (lambda (s f) (apply f s)))
(define list (lambda args args))
(define fact-1
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-1)
	(with (fact-2 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-1 trail)))))))
(define fact-2
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-2)
	(with (fact-3 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-2 trail)))))))
(define fact-3
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-3)
	(with (fact-1 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-3 trail)))))))
(fact-1 10)

; test 185
(+ 1 1/2)

; test 186
(+ 1/2 1)

; test 187
(+ 1/3 2/3)

; test 188
(+)

; test 189
(= (+ (/ 1 3) 5/3 (/ 9 27)) 7/3)

; test 190
(*)

; test 191
(or)

; test 192
(and)

; test 193
(+ 3 4 5/4 (* 1000 2/1000) 4/5 3 2 4 3/200)

; test 194
(define f (lambda (x) (if (zero? x) x (+ 1 (f (- x 1))))))
(eq? 50 (f 50))

; test 195
`(+ 1 ,(car '(1 2)) ,@'(2 3))

; test 196
`((unquote-splicing (quote (3 4 5))))

; test 197
`(+ ,'(+ 1 2 3) ,'(+ 2 3) (+ ,@'( 6 7)))

; test 198
`(+ ,(+ 1 2 3) ,(+ 2 3) (+ ,@'( 6 7)))

; test 199
(quasiquote (+ ,(+ 1 2 3) ,(+ 2 3) (+ (unquote-splicing '( 6 7)))))

; test 200
`(+ ,(cons 2 3) ,@'((cons 2 3)) ,'( 2 3))

