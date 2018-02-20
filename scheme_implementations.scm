(define map
  (lambda (func first . rest)
    (if (null? rest)
        (not_var_map func first)
        (var_map func first rest))))


(define not_var_map
    (lambda (func lst)
      (if (null? lst)
          '()
          (cons (func (car lst)) (not_var_map func (cdr lst))))))

(define var_map
    (lambda (func first rest)
        (if (null? first)
            '()
            (cons (apply func (car first) (map car rest))
                  (var_map func (cdr first)
                            (map cdr rest))))))

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

;(define list
;    (lambda args
;        (if (null? args)
;            '()
;            (cons (car args) (list (cdr args))))))