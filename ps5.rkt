#lang plai-typed
(require "ps5-ast.rkt")

(define (parse-ty (s : s-expression)) : Type
  (cond
    [(s-exp-symbol? s)
     (case (s-exp->symbol s)
       [(boolT) (boolT)]
       [(voidT) (voidT)]
       [(numT) (numT)])]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(funT) (funT (parse-ty (second l)) (parse-ty (third l)))]
            [(pairT) (pairT (parse-ty (second l)) (parse-ty (third l)))]
            [(boxT) (boxT (parse-ty (second l)))]
            [(listT) (listT (parse-ty (second l)))]
            )]))]))

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(pair) (pairC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(cons) (consC (parse (second l)) (parse (third l)))]
            [(is-empty?) (is-empty?C (parse (second l)))]
            [(empty) (emptyC (parse-ty (second l)))]
            [(first) (firstC (parse (second l)))]
            [(rest) (restC (parse (second l)))]
            [(fst) (fstC (parse (second l)))]
            [(snd) (sndC (parse (second l)))]
            [(box) (boxC (parse (second l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(set-box!) (set-box!C (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse-ty (third l)) (parse (fourth l)))]
            [(rec) (recC (s-exp->symbol (second l)) (s-exp->symbol (third l)) (parse-ty (fourth l))
                         (parse-ty (list-ref l 4)) (parse (list-ref l 5)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
         ))]
    ))



(define-type (Binding 'a)
  [bind (name : symbol) (val : 'a)])

(define-type-alias TyEnv (listof (Binding Type)))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : (listof (Binding 'a)))) : 'a
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))


; TODO: you must implement this.
; It if e has type t under environment env, then
; (tc-env env e) should return t.
; Otherwise, if e is not well-typed (i.e. does not type check), tc-env should raise an exception
; of some form using the 'error' construct in plai-typed.

(define (tc-env (env : TyEnv) (e : Expr)) : Type
  (type-case Expr e
    [numC (n) (numT)]
    [voidC () (voidT)]
    [boolC (b) (boolT)]
    [pairC (e1 e2) (let ([t1 (tc-env env e1)]
                         [t2 (tc-env env e2)]) (pairT t1 t2))]
    [fstC (e) (type-case Type (tc-env env e)
                [pairT (t1 t2) t1]
                [else (error 'tc "fstC expected pairT")])]
    [sndC (e) (type-case Type (tc-env env e)
                [pairT (t1 t2) t2]
                [else (error 'tc "sndC expected pairT")])]
    [plusC (e1 e2) (if (and (equal? (tc-env env e1) (numT)) (equal? (tc-env env e2) (numT)))
                       (numT)
                       (error 'tc "plusC not numbers"))]
    [timesC (e1 e2) (if (and (equal? (tc-env env e1) (numT)) (equal? (tc-env env e2) (numT)))
                        (numT)
                        (error 'tc "timesC not numbers"))]
    [equal?C (e1 e2) (if (equal? (tc-env env e1) (tc-env env e2))
                         (boolT)
                         (error 'tc "equal?C type mismatch"))]
    [letC (x e1 e2) (let ([t1 (tc-env env e1)]) (tc-env (extend-env (bind x t1) env) e2))]
    [lambdaC (x argT e) (funT argT (tc-env (extend-env (bind x argT) env) e))]
    [appC (e1 e2) (type-case Type (tc-env env e1)
                    [funT (ty-arg ty-ret)
                          (if (equal? (tc-env env e2) ty-arg)
                              ty-ret
                              (error 'tc "argument did not match input type"))]
                    [else (error 'tc "application of a non-function")])]
    [idC (x) (lookup x env)]
    [ifC (e0 e1 e2) (if (equal? (tc-env env e0) (boolT))
                        (let ([t1 (tc-env env e1)]
                              [t2 (tc-env env e2)])
                          (if (equal? t1 t2)
                              t1
                              (error 'tc "ifC branch type mismatch")))
                        (error 'tc "ifC gaurd type mismatch"))]
    [emptyC (t) (listT t)]
    [consC (e1 e2) (type-case Type (tc-env env e2)
                     [listT (t)
                            (if (equal? (tc-env env e1) t)
                                (listT t)
                                (error 'tc "consC head/list mismatch"))]
                     [else (error 'tc "consC expected listT")])]
    [firstC (e) (type-case Type (tc-env env e)
                  [listT (t) t]
                  [else (error 'tc "firstC expected listT")])]
    [restC (e) (type-case Type (tc-env env e)
                 [listT (t) (listT t)]
                 [else (error 'tc "restC expected listT")])]
    [is-empty?C (e) (type-case Type (tc-env env e)
                      [listT (t) (boolT)]
                      [else (error 'tc "is-empty?C expected listT")])]
    [recC (f x argT retT e) (let* ([rec-env (extend-env (bind x argT)
                                                        (extend-env (bind f (funT argT retT)) env))]
                                   [fT (funT argT (tc-env rec-env e))])
                              (if (equal? (funT argT retT) fT)
                                  fT
                                  (error 'tc "recC type mismatch")))]
    [boxC (e) (let ([t (tc-env env e)]) (boxT t))]
    [unboxC (e) (type-case Type (tc-env env e)
                  [boxT (t) t]
                  [else (error 'tc "unboxC expected boxT")])]
    [set-box!C (e1 e2) (type-case Type (tc-env env e1)
                         [boxT (t)
                               (if (equal? t (tc-env env e2))
                                   (voidT)
                                   (error 'tc "set-box!C value type mismatch"))]
                         [else (error 'tc "set-box!C expected boxT")])]
    )
  )

(define (tc (e : Expr))
  (tc-env empty-env e))

