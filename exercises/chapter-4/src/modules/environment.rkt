#lang sicp

(#%provide set-variable-value! make-scheme-environment define-variable! lookup-variable-value)

(define (replace-entry key value alist)
  (define (replace-entry-iter acc rest)
    (cond
      [(null? rest) (reverse acc)]
      [(eq? (caar rest) key) (replace-entry-iter (cons (cons key value) acc) (cdr rest))]
      [else (replace-entry-iter (cons (car rest) acc) (cdr rest))]))
  (replace-entry-iter '() alist))

(define (make-scheme-environment)
  (let ([var-alist '()])
    (define (define-variable! var-name value)
      (if (not (symbol? var-name))
          (error "DEFINE: Name must be a symbol" var-name)
          (let ([ret (assoc var-name var-alist)])
            (cond
              [(pair? ret) (error "DEFINE: Redefining a value" var-name value)]
              [(not ret) (set! var-alist (cons (cons var-name value) var-alist))]
              [else (error "DEFINE: Unexpected branch")]))))
    (define (set-variable-value! var-name value)
      (if (not (symbol? var-name))
          (error "SET!: Name must be symbol" var-name)
          (let ([ret (assoc var-name var-alist)])
            (cond
              [(pair? ret) (set! var-alist (replace-entry var-name value var-alist))]
              [else (error "SET!: Must operate on a defined variable" var-name value)]))))
    (define (lookup-variable-value var-name)
      (let ([res (assoc var-name var-alist)])
        (if (pair? res)
            (cdr res)
            'false)))
    (lambda (s)
      (cond
        [(eq? s 'define-variable!) define-variable!]
        [(eq? s 'set-variable-value!) set-variable-value!]
        [(eq? s 'lookup-variable-value) lookup-variable-value]
        [else (error "SCHEME-ENVIRONMENT: Could not find symbol" s)]))))

(define (define-variable! var-name value env)
  ((env 'define-variable!) var-name value))

(define (set-variable-value! var-name value env)
  ((env 'set-variable-value!) var-name value))

(define (lookup-variable-value var-name env)
  "environment selector procedure. var-name should be a symbol, and env is an
  instance of the scheme environment type"
  ((env 'lookup-variable-value) var-name))
