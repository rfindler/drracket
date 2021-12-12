#lang racket
(require drracket/insulated-read-language rackunit
         syntax-color/racket-lexer
         syntax-color/color-textoid
         framework
         racket/gui)

;; this will rewrite attempts to use
;; #lang BOGUS's reader to a submodule
;; that is in this file
(define-syntax-rule
  (with-bogus-redirection mod body ...)
  (with-bogus-redirection/proc mod (λ () body ...)))
(define (with-bogus-redirection/proc mod thunk)
  (let ([mnr (current-module-name-resolver)])
    (parameterize ([current-module-name-resolver
                    (case-lambda
                      [(a b) (mnr a b)]
                      [(mp rel stx load?)
                       (define new-mp
                         (match mp
                           [`(submod BOGUS reader)
                            `(submod tests/insulated-read-language/test ,mod reader)]
                           [_ mp]))
                       (mnr new-mp rel stx load?)])])
      (thunk))))

(define (get-tokens str the-lexer*)
  (define sp (open-input-string str))
  (port-count-lines! sp)
  (let loop ([mode #f])
    (define-values (txt/eof token paren start end off new-mode)
      (if (procedure-arity-includes? the-lexer* 3)
          (the-lexer* sp 0 mode)
          (let-values ([(a b c d e) (the-lexer* sp)])
            (values a b c d e 0 #f))))
    (cond
      [(eof-object? txt/eof) '()]
      [else (cons (vector txt/eof token paren start end)
                  (loop new-mode))])))

(module in-irl-test-cannot-get-info racket/base
  (module reader racket/base
    (provide get-info)
    (error 'cannot-get-get-info)
    (define (get-info . args) (void))))
(with-bogus-redirection 'in-irl-test-cannot-get-info
  (define got-to-error #f)
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (set! got-to-error #t))
              (hash 'key (syntax-info-details any/c (flat) 'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (check-true got-to-error))

(module in-irl-test-simple-info racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) 42))))
(with-bogus-redirection 'in-irl-test-simple-info
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (error 'ack "~s" x))
              (hash 'key (syntax-info-details any/c (flat) 'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (check-equal? (call-read-language an-irl 'key) 42))

(module in-irl-test-adder-info racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (x y) (+ x y))))))
(with-bogus-redirection 'in-irl-test-adder-info
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (error 'ack "~s" x))
              (hash 'adder (syntax-info-details
                            any/c
                            (-> (x (flat)) (y (flat)) (res (flat))
                                #:failure 'wrong)
                            'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (check-equal? ((call-read-language an-irl 'adder) 15 27) 42))

(module in-irl-test-broken-adder-info racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (x y) (car 'this-should-trigger-using-the-default))))))
(with-bogus-redirection 'in-irl-test-broken-adder-info
  (define an-irl
    (make-irl (current-directory)
              void
              (hash 'adder (syntax-info-details
                            any/c
                            (-> (x (flat)) (y (flat)) (res (flat))
                                #:failure (+ x y))
                            'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define adder (call-read-language an-irl 'adder))
  (define fourty-two (adder 15 27))
  (check-equal? fourty-two 42))

(module in-irl-test-ho racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (x) (λ (y) (+ x y)))))))
(with-bogus-redirection 'in-irl-test-ho
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (error 'ack "~s" x))
              (hash 'key (syntax-info-details
                          any/c
                          (-> (x (flat))
                              (res1 (-> (y (flat))
                                        (res2 (flat))
                                        #:failure (+ x y)))
                              #:failure 'wrong)
                          'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define f (call-read-language an-irl 'key))
  (define f20 (f 20))
  (define f2022 (f20 22))
  (check-equal? f2022 42))

(module in-irl-test-cond.1 racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (x) (if (= x 1) (car #f) 5))))))
(with-bogus-redirection 'in-irl-test-cond.1
  (define an-irl
    (make-irl (current-directory)
              void
              (hash
               'key
               (syntax-info-details
                any/c
                (cond
                  f
                  [(procedure-arity-includes? f 1)
                   (-> (x (flat)) (res (flat)) #:failure x)]
                  [(procedure-arity-includes? f 2)
                   (-> (x (flat)) (y (flat)) (res (flat)) #:failure (+ x y))])
                'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define f (call-read-language an-irl 'key))
  (check-equal? (f 20) 5)
  (check-equal? (f 1) 1))

(module in-irl-test-cond.2 racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (x y) (if (= x y) (car #f) (* x y)))))))
(with-bogus-redirection 'in-irl-test-cond.2
  (define an-irl
    (make-irl (current-directory)
              void
              (hash
               'key
               (syntax-info-details
                any/c
                (cond
                  f
                  [(procedure-arity-includes? f 1)
                   (-> (x (flat)) (res (flat)) #:failure x)]
                  [(procedure-arity-includes? f 2)
                   (-> (x (flat)) (y (flat)) (res (flat)) #:failure (+ x y))])
                'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define f (call-read-language an-irl 'key))
  (check-equal? (f 4 5) 20)
  (check-equal? (f 3 3) 6))

(module in-irl-test-pre-vars racket/base
  (module reader racket/base
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (x y) (if (= x y) (car #f) (* x y)))))))
(with-bogus-redirection 'in-irl-test-pre-vars
  (define an-irl
    (make-irl (current-directory)
              void
              (hash
               'key
               (syntax-info-details
                any/c
                (-> #:pre-vars ([p 5]) (x (flat)) (res (flat)) #:failure (cons p x))
                'default))))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define f (call-read-language an-irl 'key))
  (check-equal? (f 4) (cons 5 4)))

(module in-irl-test-color-lexer.1 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) racket-lexer))))
(with-bogus-redirection 'in-irl-test-color-lexer.1
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (error 'ack "~s" x))
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define racket-lexer/protected (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "a(x)" racket-lexer/protected)
                (get-tokens "a(x)" racket-lexer)))

(module in-irl-test-color-lexer.2 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) racket-lexer*))))
(with-bogus-redirection 'in-irl-test-color-lexer.2
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (error 'ack "~s" x))
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define racket-lexer/protected (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "a(x)" racket-lexer/protected)
                (get-tokens "a(x)" racket-lexer*)))

(module in-irl-test-color-lexer.3 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (in) (car #f))))))
(with-bogus-redirection 'in-irl-test-color-lexer.3
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define broken-lexer (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "abc" broken-lexer)
                (list (vector "a" 'error #f 1 2)
                      (vector "b" 'error #f 2 3)
                      (vector "c" 'error #f 3 4))))

(module in-irl-test-color-lexer.4 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (in)
                         (define c (read-char in))
                         (cond
                           [(equal? c #\a)
                            (read-char in)
                            (values "ab" 'symbol #f 1 3)]
                           [else (car #f)]))))))
(with-bogus-redirection 'in-irl-test-color-lexer.4
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define broken-lexer (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "abcd" broken-lexer)
                (list (vector "ab" 'symbol #f 1 3)
                      (vector "" 'error #f 3 4)
                      (vector "d" 'error #f 4 5))))

(module in-irl-test-color-lexer.5 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (in)
                         ;; wrong because the numbers are negative which breaks the contract
                         (values (string (read-char in) (read-char in))
                                 'symbol #f -1 -3))))))
(with-bogus-redirection 'in-irl-test-color-lexer.5
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define broken-lexer (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "abcd" broken-lexer)
                (list (vector "" 'error #f 1 3)
                      (vector "c" 'error #f 3 4)
                      (vector "d" 'error #f 4 5))))

(module in-irl-test-color-lexer.6 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) default))))
(with-bogus-redirection 'in-irl-test-color-lexer.6
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define broken-lexer (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "abcd" broken-lexer)
                (list (vector "abcd" 'symbol #f 1 5))))

(module in-irl-test-color-lexer.7 racket/base
  (module reader racket/base
    (require syntax-color/racket-lexer)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) #f))))
(with-bogus-redirection 'in-irl-test-color-lexer.7
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define broken-lexer (call-read-language an-irl 'color-lexer))
  (check-equal? (get-tokens "abcd" broken-lexer)
                (list (vector "abcd" 'symbol #f 1 5))))

(module in-irl-test-drracket:indentation.1 racket/base
  (module reader racket/base
    (require racket/class)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default) (λ (txt pos) (string->number (send txt get-text 0 1)))))))
(with-bogus-redirection 'in-irl-test-drracket:indentation.1
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define indent (call-read-language an-irl 'drracket:indentation))
  (define t1 (new text%)) (send t1 insert "2345")
  (check-equal? (indent t1 0) 2))


(module in-irl-test-drracket:indentation.2 racket/base
  (module reader racket/base
    (require racket/class)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default)
        (define _txt #f)
        (λ (txt pos)
          (unless _txt (set! _txt txt))
          (string->number (send (or _txt txt) get-text 0 1)))))))
(with-bogus-redirection 'in-irl-test-drracket:indentation.2
  (define an-irl
    (make-irl (current-directory)
              void
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define indent (call-read-language an-irl 'drracket:indentation))
  (define t1 (new text%)) (send t1 insert "2345")
  (define t2 (new text%)) (send t2 insert "2345")
  (check-equal? (indent t1 0) 2)
  (check-equal? (indent t2 0) #f))

(module in-irl-test-drracket:grouping-position racket/base
  (module reader racket/base
    (require racket/class)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default)
        (λ (txt start limit dir) #t)))))
(with-bogus-redirection 'in-irl-test-drracket:grouping-position
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (raise x))
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define grouping-position (call-read-language an-irl 'drracket:grouping-position))
  (define t1 (new racket:text%)) (send t1 insert "(x ())")
  (send t1 freeze-colorer) (send t1 thaw-colorer)
  (check-equal? (grouping-position t1 2 100 'up) 0)
  (check-equal? (grouping-position t1 2 100 'down) 4)
  (check-equal? (grouping-position t1 2 100 'backward) 1)
  (check-equal? (grouping-position t1 2 100 'forward) 5))

(module in-irl-test-range-indentation racket/base
  (module reader racket/base
    (require racket/class)
    (provide get-info)
    (define (get-info inp mp line col pos)
      (λ (key default)
        (case key
          [(drracket:range-indentation)
           (λ (txt start-pos end-pos)
             (cond
               [(and (= 1 start-pos) (= 2 end-pos)) (list (list 11 " "))]
               [else #f]))]
          [(drracket:indentation)
           (λ (txt pos)
             (+ pos 1))])))))
(with-bogus-redirection 'in-irl-test-range-indentation
  (define an-irl
    (make-irl (current-directory)
              (λ (x) (raise x))
              simple-irl-keys))
  (reset-irl! an-irl (open-input-string "#lang BOGUS"))
  (define range-indentation (call-read-language an-irl 'drracket:range-indentation))
  (define t1 (new racket:text%)) (send t1 insert "(x ())")
  (send t1 freeze-colorer) (send t1 thaw-colorer)
  (check-equal? (range-indentation t1 1 2) (list (list 11 " ")))
  (check-equal? (range-indentation t1 3 4) '?))
