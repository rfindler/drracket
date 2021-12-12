#lang racket/base

(require racket/match
         racket/math
         racket/class
         racket/contract/option
         racket/contract
         racket/port
         (submod drracket/insulated-read-language skip-past-comments)
         syntax/modread)
(provide reset-irl!/inside
         call-read-language/inside
         get-read-language-port-start+end/inside
         get-read-language-last-position/inside
         get-read-language-name/inside
         get-insulated-module-lexer/inside
         get-submit-predicate/inside
         set-irl-mcli-vec!/inside
         ;; for test suite
         compute-lang-info)

(define language-get-info #f)
(define before-port-position 0)
(define after-port-position #f)
(define mcli-vec #f)
(define read-language-last-position #f)

(define (set-irl-mcli-vec!/inside _mcli-vec) (set! mcli-vec _mcli-vec))

(define (get-submit-predicate/inside)
  (or (and mcli-vec
           (let ([get-info
                  ((dynamic-require (vector-ref mcli-vec 0)
                                    (vector-ref mcli-vec 1))
                   (vector-ref mcli-vec 2))])
             (add-contract
              'drracket:submit-predicate
              (key->contract 'drracket:submit-predicate)
              (get-info 'drracket:submit-predicate #f))))
      (call-read-language/inside 'drracket:submit-predicate
                                 #f)))

(define (key->contract key)
  (error 'key->contract
         "this is the old way of doing things and should go away;\n  key ~s"
         key))

(define (copy-the-strings val)
  (cond
    [(pair? val) (cons (copy-the-strings (car val))
                       (copy-the-strings (cdr val)))]
    [(string? val) (if (immutable? val)
                       val
                       (string->immutable-string val))]
    [else val]))

(define module-lexer #f)
(define lang-name "<<unknown>>")
  
(define (get-insulated-module-lexer/inside)
  (unless module-lexer
    (set! module-lexer (waive-option (dynamic-require 'syntax-color/module-lexer 'module-lexer*))))
  module-lexer)

(define-logger drracket-language)

(define (compute-lang-info port)
  (skip-past-comments port)
  (define peeking-port (peeking-input-port port))
  (port-count-lines! peeking-port)
    
  (define-values (_1 _2 before-pos) (port-next-location port))
  (define language-get-info
    (with-module-reading-parameterization
     (λ ()
       (parameterize ([current-load-relative-directory (current-directory)])
         (with-handlers ([exn:fail? (λ (exn) exn)])
           (read-language peeking-port))))))
  (cond
    [(exn:fail? language-get-info)
     (define-values (_3 _4 peeking-pos) (port-next-location peeking-port))
     (values language-get-info #f #f #f (+ before-pos peeking-pos -2))]
    [language-get-info
     (define-values (_3 _4 peeking-pos) (port-next-location peeking-port))
     (define lang-name (make-string (- peeking-pos 1)))
     (read-string! lang-name port)
     (define-values (_5 _6 after-pos) (port-next-location port))
     (values language-get-info
             lang-name
             (- before-pos 1)
             (- after-pos 1)
             (- after-pos 1))]
    [else
     (define-values (_5 _6 peeking-pos) (port-next-location peeking-port))
     (values #f #f #f #f (+ before-pos peeking-pos -2))]))
    
(define (reset-irl!/inside port)
  (set!-values (language-get-info
                lang-name
                before-port-position
                after-port-position
                read-language-last-position)
               (compute-lang-info port))
  (when (exn:fail? language-get-info)
    (define exn language-get-info)
    (set! language-get-info #f)
    (raise exn)))

(define-values (uniq-default uniq-default?)
  (let ()
    (struct uniq-default ())
    (values (uniq-default) uniq-default?)))
(define (call-read-language/inside key default ctc wrapper)
  (define val
    (cond
      [language-get-info
       (language-get-info key uniq-default)]
      [else
       uniq-default]))
  (cond
    [(uniq-default? val) default]
    [else (wrapper (add-contract ctc key val))]))

(define (add-contract ctc key val)
  (contract ctc
            val
            lang-name
            "irl client" ;; what should be done about this contract name?
            (format "~a's ~a on ~a" lang-name (object-name language-get-info) key)
            #f))

(define (get-read-language-last-position/inside) read-language-last-position)

(define (get-read-language-port-start+end/inside)
  (values before-port-position after-port-position))

(define (get-read-language-name/inside) lang-name)

(module+ test
  (require rackunit)

  (define (compute-lang-info/wrap str)
    (define sp (open-input-string str))
    (port-count-lines! sp)
    (define-values (language-get-info lang-name
                                      before-port-position after-port-position
                                      read-language-last-position)
      (compute-lang-info sp))
    (list lang-name
          before-port-position
          after-port-position
          read-language-last-position))
  
  (check-equal? (compute-lang-info/wrap "#lang racket")
                (list "#lang racket" 0 12 12))
  (check-equal? (compute-lang-info/wrap "#lang racket/base")
                (list "#lang racket/base" 0 17 17))
  (check-equal? (compute-lang-info/wrap ";; abc\n#lang racket")
                (list "#lang racket" 7 19 19))
  (check-equal? (compute-lang-info/wrap ";; abc\n#lang racket\n;; fdajk")
                (list "#lang racket" 7 19 19))
  (check-equal? (compute-lang-info/wrap ";; abc\n(stuff)")
                (list #f #f #f 8))
  (check-equal? (compute-lang-info/wrap ";; abcdefgjd\n\n\n(stuff)")
                (list #f #f #f 16))
  (check-equal? (compute-lang-info/wrap "(stuff)")
                (list #f #f #f 1))
  (check-equal? (compute-lang-info/wrap "123 456")
                (list #f #f #f 1)))
