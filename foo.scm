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
;;'a ;; to figure out how to handle symbol type const
;;; Constants


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

;(map cons '(1 2 3) '(1 2 3))
;(map + '(1 2 3) '(1 2 3))
;(map remainder '(13 13 -13 -13) '(4 -4 -4 4))
;(map cons '(1 #t #f 3) '(1 2 3 4))
;(map boolean? '(1 #f "a"))


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

;(append '(1 2) '(3 4))

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