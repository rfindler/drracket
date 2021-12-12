#lang racket/base

#|

todo:
 - figure out default values (don't give the tools `#f`)
 - add some support for untrusted language extensions (and figure out performance)
 - figure out what's going to happen with the copying of strings; here's an old note:
   NB: all string?s are expected to be inside only lists
   or are expected to be immutable (via the contract);
   (the ones inside lists that are not required to be immutable
   is for backwards compatibility; they are copied)
 - figure out how to integrate module->language-info (and which keys use what, I suppose?)
   looks like it is currently used only for 'drracket:submit-predicate(?)

 - I think we want to call read-language at most once for any given key? Or is it okay to call the function over and over,
   cf the use of 'drracket:submit-predicate in drracket

 - make sure that a language that actually uses 'definitions-text-surrogate still works

 - make sure that the initial repl construction works properly (that REPL before "Run" is clicked)

Will not work with the definitions text surrogate interposition that
#lang allows. Need to deprecate that one

--> scribble (in at-exp-lib) uses it to add keybindings; should
    add support for that directly

--> it may take a long time for the read-language call to return (or one
    of the others, I suppose?) use a separate thread?

|#

(require racket/class
         racket/contract
         racket/runtime-path
         racket/match
         racket/math
         syntax-color/racket-lexer
         syntax-color/lexer-contract
         syntax-color/color-textoid
         syntax-color/racket-navigation
         syntax-color/racket-indentation

         ;; this is some error checking code that I'd like to avoid redundancy on
         syntax-color/lexer-contract

         (for-syntax racket/base syntax/parse racket/sequence))

(provide
 syntax-info-details
 (contract-out
  [call-read-language (-> irl? symbol? any)]

  ;; returns the part of the port that contributed to the actual language name
  ;; so, the first number is how many characters were comments and the
  ;; second is how of the following part of the port got read
  ;; (unlike positions in a port, these count from 0)
  [get-read-language-port-start+end
   (-> irl?
       (values (or/c #f exact-nonnegative-integer?)
               (or/c #f exact-nonnegative-integer?)))]
  [get-read-language-last-position (-> irl? (or/c #f exact-nonnegative-integer?))]

  [get-read-language-name (-> irl? (or/c #f string?))]

  [get-insulated-module-lexer (-> irl? (procedure-arity-includes/c 3))]

  [set-irl-mcli-vec! (-> irl? (or/c mcli? #f) void?)]

  ;; supplies a callback that's invoked when an error happens that disables the `irl`.
  ;; the path is used for the current-load-relative-directory
  [make-irl (->* (path-string?
                  (-> exn:fail? any)
                  (hash/c symbol? syntax-info-key-details? #:immutable #t))
                 (#:in-dynamic-extent (-> (-> any) any)
                  #:shared-modules (listof (or module-path? resolved-module-path?)))
                 irl?)]
  [make-simple-irl (-> path-string? (-> exn:fail? any) irl?)]
  [simple-irl-keys (hash/c symbol? syntax-info-key-details? #:immutable #t)]

  ;; call to let the abstraction know that it needs a new `read-language`
  ;; result. If the boolean is #t, then it jettisons all cached loaded
  ;; modules too (otherwise, it re-uses them).
  [reset-irl! (->* (irl? port?)
                   (#:directory (or/c #f path-string?)
                    #:flush-cache? boolean?
                    #:in-dynamic-extent (or/c #f (-> (-> any) any)))
                   void?)]
  [mcli? procedure?]))

(struct irl ([namespace #:mutable]
             [in-dynamic-extent #:mutable]
             [use-evaluator? #:mutable]
             [directory #:mutable]
             fail-callback
             shared-modules
             keys->wrappers))

(define (get-read-language-port-start+end an-irl)
  (call-irl-proc an-irl
                 (λ () (values #f #f))
                 'get-read-language-port-start+end/inside))

(define (get-read-language-name an-irl)
  (call-irl-proc an-irl
                 (λ () #f)
                 'get-read-language-name/inside))

(define (get-read-language-last-position an-irl)
  (call-irl-proc an-irl
                 (λ () #f)
                 'get-read-language-last-position/inside))

(define (3arg-racket-lexer in offset mode)
  (define-values (a b c d e) (racket-lexer in))
  (values a b c d e 0 #f))

(define (get-insulated-module-lexer an-irl)
  (define module-lexer
    (call-irl-proc an-irl
                   (λ () 3arg-racket-lexer)
                   'get-insulated-module-lexer/inside))
  (λ (in offset mode)
    (call-in-irl-context/abort
     an-irl
     (λ () (3arg-racket-lexer in offset mode))
     (λ () (module-lexer in offset mode)))))

(struct syntax-info-key-details (info/c convert default false-as-default?) #:transparent)
(define-syntax (syntax-info-details stx)
  (syntax-parse stx
    [(_ info/c guide default
        (~optional (~seq #:false-as-default false-as-default?:expr)))
     #`(syntax-info-key-details
        info/c
        (λ (an-irl val) (in-irl an-irl val #f guide))
        default
        #,(or (attribute false-as-default?) #'#t))]))

(define-syntax (in-irl stx)
  (define/syntax-parse (_ an-irl:id val:id in-irl?:boolean guide) stx)
  (define/syntax-parse not-in?:boolean (not (syntax-e #'in-irl?)))
  (syntax-parse #'guide
    #:literals (-> cond values color-textoid<%>)
    #:datum-literals (flat)
    [(-> (~optional (~seq #:pre-vars ([maybe-p:id maybe-pre-e:expr] ...)))
         (d-x:id doms) ...
         rng
         (~optional (~seq #:adjust-result maybe-adjustment-e:expr))
         #:failure fail:expr)
     #:when (syntax-e #'not-in?)

     (define/syntax-parse (p-vars ...) (or (attribute maybe-p) #'()))
     (define/syntax-parse (pre-e ...) (or (attribute maybe-pre-e) #'()))
     (define/syntax-parse ((res rngs) ...)
       (syntax-parse #'rng
         #:literals (values)
         [(values (res rngs) ...)
          #'((res rngs) ...)]
         [(res rng)
          #'((res rng))]))

     (define/syntax-parse (invalidate-textoid-code ...)
       (for/list ([dom (in-syntax #'(doms ...))]
                  [a-d-x (in-syntax #'(d-x ...))]
                  #:when
                  (syntax-parse dom
                    #:literals (color-textoid<%>)
                    [color-textoid<%> #t]
                    [_ #f]))
         #`(invalidate-textoid #,a-d-x)))

     #`(λ (d-x ...)
         (let ([d-x (in-irl an-irl d-x not-in? doms)] ...)
           (let-values ([(p-vars ...) pre-e] ...)
             (let-values ([(res ...)
                           (call-in-irl-context/abort
                            an-irl
                            (λ () fail)
                            (λ () (val d-x ...)))])

               #,(cond
                   [(attribute maybe-adjustment-e)
                    #`(let-values ([(res ...) #,(attribute maybe-adjustment-e)])
                        invalidate-textoid-code ...
                        (values (in-irl an-irl res in-irl? rngs) ...))]
                   [else
                    #`(begin
                        invalidate-textoid-code ...
                        (values (in-irl an-irl res in-irl? rngs) ...))])))))]
    [(-> . anything)
     #:when (syntax-e #'in-irl?)
     (raise-syntax-error
      'syntax-info-details
      (string-append
       "cannot leave irl context;\n"
       " a function passed into an irl context might be called from within\n"
       " the irl context but it would not be able to switch back out currently,\n"
       " so this is not supported")
      #'guide)]
    [(cond x:id
           [e:expr a] ...)
     #'(cond
         [(let ([x val]) e)
          (in-irl an-irl val in-irl? a)]
         ...)]
    [color-textoid<%> #'(proxy-textoid val)]
    [(flat (~optional (~seq #:in-irl-adjustment maybe-id:id maybe-adjustment-e:expr maybe-fail-e:expr)))
     (if (attribute maybe-id)
         #`(let ([#,(attribute maybe-id) val])
             (call-in-irl-context/abort
              an-irl
              (λ () #,(attribute maybe-fail-e))
              (λ () #,(attribute maybe-adjustment-e))))
         #'val)]))

(define-local-member-name invalidate!)
(define proxy-textoid%
  (class* object% (color-textoid<%>)
    (init proxy)
    (define _proxy proxy)
    (define/public (invalidate!) (set! _proxy #f))
    (define-syntax (m stx)
      (syntax-parse stx
        [(_ m args ...)
         (define/syntax-parse (x ...)
           (for/list ([arg (in-syntax #'(args ...))])
             (syntax-parse arg
               [x:id #'x]
               [(x:id e:expr) #'x])))
         #'(define/public (m args ...)
             (unless _proxy (error 'proxy-textoid% "this proxy has been invalidated"))
             (send _proxy m x ...))]))
    (m get-text [start 0] [end 'eof])
    (m get-character start)
    (m last-position)
    (m position-paragraph start [at-eol? #f])
    (m paragraph-start-position para [visible? #t])
    (m paragraph-end-position para [visible? #t])
    (m skip-whitespace position direction comments?)
    (m backward-match position cutoff)
    (m backward-containing-sexp position cutoff)
    (m forward-match position cutoff)
    (m classify-position position)
    (m classify-position* position)
    (m get-regions)
    (m get-token-range position)
    (m get-backward-navigation-limit start)
    (super-new)))
(define (proxy-textoid val) (new proxy-textoid% [proxy val]))
(define (invalidate-textoid val) (send val invalidate!))

(define mcli? (vector/c module-path? symbol? any/c #:flat? #t))
(define (set-irl-mcli-vec! an-irl mcli/f)
  (call-irl-proc an-irl
                 void
                 'set-irl-mcli-vec!/inside
                 mcli/f))

(define (call-in-irl-context/abort an-irl fallback-thunk thunk)
  (match-define (irl namespace in-dynamic-extent use-evaluator?
                     directory failure shared-modules keys->wrappers)
    an-irl)
  (cond
    [use-evaluator?
     (parameterize ([current-directory directory]
                    [current-load-relative-directory directory]
                    [current-namespace namespace])
       (in-dynamic-extent
        (λ ()
          (let/ec k
            (call-with-exception-handler
             (λ (exn)
               (cond
                 [(exn:fail? exn)
                  (failure exn)
                  (set-irl-use-evaluator?! an-irl #f)
                  (call-with-values
                   fallback-thunk
                   (λ args
                     (apply k args)))]
                 [else exn]))
             thunk)))))]
    [else (fallback-thunk)]))

(define (call-read-language an-irl key)
  (define a-syntax-info-key-details (hash-ref (irl-keys->wrappers an-irl) key #f))
  (unless a-syntax-info-key-details
    (raise-argument-error
     'call-read-language "a key known to the irl"
     1 an-irl key))
  (define default (syntax-info-key-details-default a-syntax-info-key-details))
  (define result
    (call-irl-proc an-irl
                   (λ () default)
                   'call-read-language/inside
                   key
                   default
                   (syntax-info-key-details-info/c a-syntax-info-key-details)
                   (λ (val) ((syntax-info-key-details-convert a-syntax-info-key-details) an-irl val))))
  (cond
    [(and (syntax-info-key-details-false-as-default? a-syntax-info-key-details)
          (not result))
     default]
    [else result]))

;; this is copied from framework/private/racket
(define default-paren-matches
  '((|(| |)|)
    (|[| |]|)
    (|{| |}|)))

(define (reset-irl! an-irl port
                    #:directory [path #f]
                    #:in-dynamic-extent [in-dynamic-extent #f]
                    #:flush-cache? [flush-cache? #f])
  (when flush-cache?
    (set-irl-namespace! an-irl (make-irl-namespace (irl-shared-modules an-irl))))
  (set-irl-use-evaluator?! an-irl #t)
  (when path (set-irl-directory! an-irl path))
  (when in-dynamic-extent (set-irl-in-dynamic-extent! an-irl in-dynamic-extent))
  (call-irl-proc an-irl
                 void
                 'reset-irl!/inside port))

(define (call-irl-proc an-irl fallback proc . args)
  (call-in-irl-context/abort
   an-irl
   fallback
   (λ () (apply (dynamic-require in-irl-namespace.rkt proc) args))))

(define (make-simple-irl directory callback)
  (make-irl directory callback simple-irl-keys
            #:shared-modules '(racket/contract)
            #:in-dynamic-extent (λ (t) (t))))

;; if pos-before is the same as pos-after then we know
;; that nothing was consumed from the port
;; if they are different, however, then probably the
;; lexer we were tring to use consumed some stuff and then
;; crashed, so we account for what was consumed
(define (failing-lexer pos-before in)
  (define-values (_1 _2 pos-after) (port-next-location in))
  (cond
    [(= pos-before pos-after)
     (define c (read-char in))
     (cond
       [(eof-object? c)
        (values c 'eof #f #f #f)]
       [else
        (values (string c)
                'error
                #f
                pos-before
                (+ pos-before 1))])]
    [else
     (values ""
             'error
             #f
             pos-before
             pos-after)]))

(define (default-grouping-position txt start limit dir)
  (case dir
    [(forward) (racket-forward-sexp txt start)]
    [(backward) (racket-backward-sexp txt start)]
    [(up) (racket-up-sexp txt start)]
    [(down) (racket-down-sexp txt start)]))

(define simple-irl-keys
  (hash 'color-lexer
        (syntax-info-details
         (or/c #f lexer*/c-without-random-testing)
         (cond
           l
           [(procedure-arity-includes? l 3)
            (-> #:pre-vars ([pos-before (let-values ([(_1 _2 pos) (port-next-location in)]) pos)])
                (in (flat))
                (offset (flat))
                (mode (flat))
                (values (str/eof (flat))
                        (token (flat))
                        (paren (flat))
                        (start (flat))
                        (end (flat))
                        (backup (flat))
                        (newmode (flat)))
                #:failure
                (let ()
                  (define-values (a b c d e) (failing-lexer pos-before in))
                  (values a b c d e #f 0 #f)))]
           [#t
            (-> #:pre-vars ([pos-before (let-values ([(_1 _2 pos) (port-next-location in)]) pos)])
                (in (flat))
                (values (str/eof (flat))
                        (token (flat))
                        (paren (flat))
                        (start (flat))
                        (end (flat)))
                #:failure
                (failing-lexer pos-before in))])
         racket-lexer*)

        'drracket:submit-predicate
        (syntax-info-details
         (or/c (-> input-port? boolean? boolean?) #f)
         (-> (inp (flat))
             (only-whitespace-after-insertion-point? (flat))
             (result (flat))
             #:failure #t)
         (λ (x) #t))


        'drracket:default-filters
        (syntax-info-details (or/c #f (listof (list/c string? string?))) (flat) '())
        'drracket:default-extension
        (syntax-info-details (or/c #f (and/c string? (not/c #rx"[.]"))) (flat) "")
        'drracket:indentation
        (syntax-info-details (or/c #f
                                   (-> (is-a?/c color-textoid<%>)
                                       exact-nonnegative-integer?
                                       (or/c #f exact-nonnegative-integer?)))
                             (-> (txt color-textoid<%>)
                                 (pos (flat))
                                 (where (flat))
                                 #:adjust-result (or where (racket-amount-to-indent txt pos))
                                 #:failure (racket-amount-to-indent txt pos))
                             (λ (x y) (racket-amount-to-indent x y)))

        'drracket:range-indentation
        (syntax-info-details
         (or/c #f
               (-> (is-a?/c color-textoid<%>)
                   exact-nonnegative-integer?
                   exact-nonnegative-integer?
                   (or/c #f (listof (list/c exact-nonnegative-integer? string?)))))
         (-> (txt color-textoid<%>)
             (start (flat))
             (end (flat))
             (res (flat))
             #:failure #f)
         (λ (txt start end) #f))

        'drracket:grouping-position
        (syntax-info-details (or/c #f
                                   (-> (is-a?/c color-textoid<%>)
                                       natural? natural? (or/c 'up 'down 'backward 'forward)
                                       (or/c #f #t natural?)))
                             (-> (text color-textoid<%>)
                                 (start (flat))
                                 (limit (flat))
                                 (dir (flat))
                                 (answer (flat))
                                 #:adjust-result
                                 (cond
                                   [(equal? answer #t) (default-grouping-position text start limit dir)]
                                   [else answer])
                                 #:failure (default-grouping-position text start limit dir))
                             default-grouping-position)

        'drracket:paren-matches
        (syntax-info-details (or/c #f (listof (list/c symbol? symbol?)))
                             (flat)
                             default-paren-matches)
        'drracket:quote-matches
        (syntax-info-details (or/c #f (listof char?))
                             (flat)
                             (list #\" #\|))))

(define (make-irl directory fail-callback keys->wrappers
                  #:shared-modules [shared-modules '(racket/base)]
                  #:in-dynamic-extent [in-dynamic-extent (λ (t) (t))])
  (irl (make-irl-namespace shared-modules)
       in-dynamic-extent
       #f
       directory
       fail-callback
       shared-modules
       keys->wrappers))

(define (make-irl-namespace shared-modules)
  (define ns (make-base-empty-namespace))
  (define trusted-namespace (current-namespace))
  (parameterize ([current-namespace ns])
    (namespace-attach-module trusted-namespace 'racket/contract)
    (namespace-attach-module trusted-namespace 'racket/class)
    (for ([module (in-list shared-modules)])
      (namespace-attach-module trusted-namespace module)))
  ns)

(define-runtime-path in-irl-namespace.rkt
  '(lib "in-irl-namespace.rkt" "drracket" "private"))

(module skip-past-comments racket/base
  (provide skip-past-comments)
  (require (for-syntax racket/base))
  (define (skip-past-comments port)
    (define (get-it str)
      (for ([c1 (in-string str)])
        (define c2 (read-char-or-special port))
        (unless (equal? c1 c2)
          (error 'get-it
                 "expected ~s, got ~s, orig string ~s"
                 c1 c2 str))))
    (let loop ()
      (define p (peek-char-or-special port))
      (cond-strs
       port
       [";"
        (let loop ()
          (define c (read-char-or-special port))
          (case c
            [(#\linefeed #\return #\u133 #\u8232 #\u8233)
             (void)]
            [else
             (unless (eof-object? c)
               (loop))]))
        (loop)]
       ["#|"
        (let loop ([depth 0])
          (define p1 (peek-char-or-special port))
          (cond
            [(eof-object? p1) (void)]
            [(and (equal? p1 #\|)
                  (equal? (peek-char-or-special port 1) #\#))
             (get-it "|#")
             (cond
               [(= depth 0) (void)]
               [else (loop (- depth 1))])]
            [(and (equal? p1 #\#)
                  (equal? (peek-char-or-special port 1) #\|))
             (get-it "#|")
             (loop (+ depth 1))]
            [else
             (read-char-or-special port)
             (loop depth)]))
        (loop)]
       ["#;"
        (let/ec k
          (with-handlers ([exn:fail:read? (λ (x) (k (void)))])
            (read port))
          (loop))]
       ["#! "
        (read-line-slash-terminates port)
        (loop)]
       ["#!/"
        (read-line-slash-terminates port)
        (loop)]
       [else
        (define p (peek-char-or-special port))
        (cond
          [(eof-object? p) (void)]
          [(and (char? p) (char-whitespace? p))
           (read-char-or-special port)
           (loop)]
          [else (void)])])))


  (define-syntax (cond-strs stx)
    (syntax-case stx (else)
      [(_ port [chars rhs ...] ... [else last ...])
       (begin
         (for ([chars (in-list (syntax->list #'(chars ...)))])
           (unless (string? (syntax-e chars))
             (raise-syntax-error 'chars "expected a string" stx chars))
           (for ([char (in-string (syntax-e chars))])
             (unless (< (char->integer char) 128)
               (raise-syntax-error 'chars "expected only one-byte chars" stx chars))))
         #'(cond
             [(check-chars port chars)
              rhs ...]
             ...
             [else last ...]))]))

  (define (check-chars port chars)
    (define matches?
      (for/and ([i (in-naturals)]
                [c (in-string chars)])
        (equal? (peek-char-or-special port i) c)))
    (when matches?
      (for ([c (in-string chars)])
        (read-char-or-special port)))
    matches?)

  (define (read-line-slash-terminates port)
    (let loop ([previous-slash? #f])
      (define c (read-char-or-special port))
      (case c
        [(#\\) (loop #t)]
        [(#\linefeed #\return)
         (cond
           [previous-slash?
            (define p (peek-char-or-special port))
            (when (and (equal? c #\return)
                       (equal? p #\linefeed))
              (read-char-or-special port))
            (loop #f)]
           [else
            (void)])]
        [else
         (unless (eof-object? c)
           (loop #f))]))))

(require (submod "." skip-past-comments))

(module+ test
  (require rackunit racket/port)
  (define (clear-em str)
    (define sp (if (port? str) str (open-input-string str)))
    (skip-past-comments sp)
    (for/list ([i (in-port read-char-or-special sp)])
      i))
  (check-equal? (clear-em ";") '())
  (check-equal? (clear-em ";\n1") '(#\1))
  (check-equal? (clear-em ";  \n1") '(#\1))
  (check-equal? (clear-em ";  \r\n1") '(#\1))
  (check-equal? (clear-em ";  \u8233\n1") '(#\1))
  (check-equal? (clear-em "         1") '(#\1))
  (check-equal? (clear-em "#| |#1") '(#\1))
  (check-equal? (clear-em "#| #| #| #| #| |# |# |# |# |#1") '(#\1))
  (check-equal? (clear-em "#| #| #| #| #| |# |# #| |# |# |# |#1") '(#\1))
  (check-equal? (clear-em "#||#1") '(#\1))
  (check-equal? (clear-em "#|#|#|#|#||#|#|#|#|#1") '(#\1))
  (check-equal? (clear-em "#|#|#|#|#||#|##||#|#|#|#1") '(#\1))
  (check-equal? (clear-em " #!    \n         1") '(#\1))
  (check-equal? (clear-em " #!/    \n         1") '(#\1))
  (check-equal? (clear-em " #!/    \\\n2\n         1") '(#\1))
  (check-equal? (clear-em " #!/    \\\r2\n         1") '(#\1))
  (check-equal? (clear-em " #!/    \\\r\n2\n         1") '(#\1))
  (check-equal? (clear-em " #!/    \n\r\n         1") '(#\1))
  (check-equal? (clear-em "#;()1") '(#\1))
  (check-equal? (clear-em "#;  (1 2 3 [] {} ;xx\n 4)  1") '(#\1))
  (check-equal? (clear-em "#||##|#lang rong|#1") '(#\1))
  (check-equal? (clear-em "#|") '()) ;; make sure this terminates

  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (display ";" out)
       (write-special '(x) out)
       (display "\n1" out)
       (close-output-port out)))
    (check-equal? (clear-em in) '(#\1)))

  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (write-special '(x) out)
       (display "\n1" out)
       (close-output-port out)))
    (check-equal? (clear-em in) '((x) #\newline #\1))))
