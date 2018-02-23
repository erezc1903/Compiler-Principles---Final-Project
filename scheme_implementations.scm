(define map
  (lambda (f . s)
    (mapList f s)))


(define mapList
    (lambda (f s)
      (cond ((null? (car s)) '())
            ;((null? (cdr s)) (map1 f (car s)))
            (else (cons (apply f (map1 car s)) (mapList f (map1 cdr s)))))))

(define map1
    (lambda (f s)
        (if (null? s)
          '()
          (cons (f (car s))
                (map1 f (cdr s))))))


;(map list '(1 2) '(1 2) '(1 2))

;(mapList list ('(1 2) '(1 2) '(1 2)))

;(cons (apply list (map1 car ('(1 2) '(1 2) '(1 2)))) (mapList list (map1 cdr ('(1 2) '(1 2) '(1 2)))))

;(map1 car ('(1 2) '(1 2) '(1 2))) = (cons (car '(1 2)) (map1 car ('(1 2) '(1 2)))) = (cons (car '(1 2)) (cons (car '(1 2)) (cons (car '(1 2)) '())))

;(map1 car ('(1 2) '(1 2))) = (cons (car '(1 2)) (map1 car ('(1 2)))) = (cons (car '(1 2)) (cons (car '(1 2)) '()))

;(map1 car ('(1 2))) = (cons (car '(1 2)) (map1 car ())) = (cons (car '(1 2)) '())

;(map1 car ()) = '()



;(mapList list (map1 cdr ('(1 2) '(1 2) '(1 2))))

;(map1 cdr ('(1 2) '(1 2) '(1 2))) = (cons (cdr '(1 2)) (map1 cdr ('(1 2) '(1 2)))) = (cons (cdr '(1 2)) (cons (cdr '(1 2)) (cons (cdr '(1 2)) '())))

;(map1 cdr ('(1 2) '(1 2))) = (cons (cdr '(1 2)) (map1 cdr ('(1 2)))) = (cons (cdr '(1 2)) (cons (cdr '(1 2)) '()))

;(map1 cdr ('(1 2))) = (cons (cdr '(1 2)) (map1 cdr ())) = (cons (cdr '(1 2)) '())

;(map1 cdr ()) = '()

;(cons (cons (car '(1 2)) (cons (car '(1 2)) (cons (car '(1 2)) '()))) (cons (cdr '(1 2)) (cons (cdr '(1 2)) (cons (cdr '(1 2)) '()))))


(define append
    (lambda args
        (append_base_case '() args)))

(define append_base_case
      (lambda (ls args)
          (if (null? args)
              ls
              (append_helper ls args))))

(define append_helper
    (lambda (ls args)
          (if (null? ls)
              (append_base_case (car args) (cdr args))
              (cons (car ls) (append_helper (cdr ls) args)))))


(define list (lambda x x))

;(define list
;    (lambda args
;        (if (null? args)
;            '()
;            (cons (car args) (list (cdr args))))))