23

-1

(/ 35 7)

(+ (* 564 5) (- (+ 4 5)))

(- ( - ( - ( - ( - ( - ( - ( - (- 5)))))))))

((lambda (a) ( + a 7)) 5)

((lambda (a b) (if (number? a) (make-string a) b)) #t 6)

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

(define rec (lambda (func1 func2 init param num)
                (if (= 0 num)
                    init
                    (func1 (rec func1 func2 (func2 2 init param) param (- num 1))
                      )
                    )
                )
    )
(rec - + 5 7 20)

"abc"

'()

;'(1 2 3 4)

