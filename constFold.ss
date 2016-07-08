(define-record ast-node (type rands))

(define build-ast
    (lambda (x)
      (let ([check-RC (lambda (x) (and (not (null? x)) (eq? (car x) 'RC)))])
        (if (null? x) 'parse-error
            (let ([tok1 (car x)] [rest (cdr x)])
              (cond
                [(eq? tok1 'LC)
                 (if (null? rest) 'parse-error
                     (let ([tok2 (car rest)]
                           [rest (cdr rest)])
                       (cond
                         [(eq? tok2 'const)
                          (let-values ([(r1 rest) (build-ast rest)])
                            (if (check-RC rest)
                                (values (make-ast-node 'const (list r1)) (cdr rest))
                                'parse-error))]
                         [(eq? tok2 'add)
                          (let*-values ([(r1 rest) (build-ast rest)]
                                            [(r2 rest) (build-ast rest)])
                            (if (check-RC rest)
                                (values (make-ast-node 'add (list r1 r2)) (cdr rest))
                               'parse-error))]
                        [(eq? tok2 'var)
                         (let-values ([(r1 rest) (build-ast rest)])
                           (if (check-RC rest)
                               (values (make-ast-node 'var (list r1)) (cdr rest))
                               'parse-error))]
                        [else 'parse-error])))]
                [(eq? tok1 'asgn_expr)
                 (let*-values ([(e1 rest) (build-ast rest)]
                                   [(e2 rest) (build-ast rest)])
                  (values (make-ast-node 'asgn (list e1 e2)) rest))]
                [(number? tok1)
                 (values (make-ast-node 'num (list tok1)) rest)]
                [(symbol? tok1)
                 (values (make-ast-node 'id (list tok1)) rest)]
                [else 'parse-error]))))))

(define const-fold
   (lambda (n)
      (let ([type (ast-node-type n)])
         (cond
            [(eq? type 'asgn)  
             (make-ast-node 'asgn (map const-fold (ast-node-rands n)))]
            [(eq? type 'add)
             (let* ([rands (map const-fold (ast-node-rands n))]
                     [r1 (car rands)]
                     [r2 (cadr rands)])
                 (cond
                    [(and (eq? 'const (ast-node-type r1)) (eq? 'const (ast-node-type r2)))
                     (make-ast-node 'const (list (apply-op 'add (car (ast-node-rands r1)) (car (ast-node-rands r2)))))]
                    [else (make-ast-node 'add rands)]))]
            [(or (eq? type 'var) (eq? type 'const)) n]
            [else 'invalid-ast]))))

(define apply-op
   (lambda (rator rand1 rand2)
      (cond
         [(and (eq? 'num (ast-node-type rand1)) 
                 (eq? 'num (ast-node-type rand2)) 
                 (eq? 'add rator))
          (make-ast-node 'num (list (+ (car (ast-node-type rand1)) (car (ast-node-type rand2)))))]
         [else 'invalid-rator-or-rands])))

(define print-ast
    (lambda (x)
      (let ([t (ast-node-type x)]
            [rands (ast-node-rands x)])
        (cond
          [(eq? t 'num) rands]
          [(eq? t 'const) (cons 'LC (cons 'const (reverse (cons 'RC (reverse (pr
int-ast (car rands)))))))]
          [(eq? t 'id) rands]
          [(eq? t 'var) (cons 'LC (cons 'var (reverse (cons 'RC (reverse (print-
ast (car rands)))))))]
          [(eq? t 'add) (cons 'LC (cons 'add (append (print-ast (car rands)) (pr
int-ast (cadr rands)) (list 'RC))))]
          [(eq? t 'asgn) (cons 'asgn_expr (append (print-ast (car rands)) (print
-ast (cadr rands))))]
          [else 'invalid-ast]))))
