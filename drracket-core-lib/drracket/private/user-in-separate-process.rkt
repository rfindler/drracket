#lang racket/base
(require "run-module-language-program.rkt"
         racket/serialize
         racket/match
         racket/gui/base
         racket/pretty)

#|

This file runs in a separate process created by DrRacket to run
Racket programs (when the `run-in-separate-process` option in the
language dialog is set).

It uses stdin and stdout to communicate with DrRacket, leaving stderr
for bugs in this code to hopefully have some useful debugging information.

|#



(define original-output-port (current-output-port))
(define original-error-port (current-error-port))

(define (send-msg msg)
  (writeln (serialize msg) original-output-port)
  (flush-output original-output-port))

(file-stream-buffer-mode original-error-port 'none) ;; stderr isn't supposed to be used; it'll show error messages from bugs, tho

(define oprintf
  (λ args
    (apply fprintf original-error-port args)
    (flush-output original-error-port)))

(let ([o-e-h (exit-handler)])
  (exit-handler
   (λ (x)
     (close-output-port current-output-pipe-out)
     (close-output-port current-error-pipe-out)
     (close-output-port current-value-pipe-out)
     (custodian-shutdown-all user-custodian)
     (o-e-h x))))

(define debug-error-display-handler
  (let ([original-error-display-hander (error-display-handler)])
    (λ (str exn)
      (when (exn? exn)
        (define srclocs1
          (filter values (map cdr (continuation-mark-set->context (exn-continuation-marks exn)))))
        (define srclocs2
          '())
        (send-msg `("print-bug-to-stderr" ,(exn-message exn) ,srclocs1 ,srclocs2)))
      (original-error-display-hander str exn))))

(define-values (current-output-pipe-in current-output-pipe-out) (make-pipe))
(define-values (current-error-pipe-in current-error-pipe-out) (make-pipe))
(define-values (current-value-pipe-in current-value-pipe-out) (make-pipe))

(define (forward-output-back from-port name)
  (define bts (make-bytes 256))
  (void
   (thread
    (λ ()
      (let loop ()
        (define res (read-bytes-avail! bts from-port))
        (cond
          [(eof-object? res)
           ;;; stop forwarding data if the pipe is closed
           (void)]
          [(procedure? res)
           ;; ignore specials
           (loop)]
          [else
           (send-msg
            `(,name ,(if (= res (bytes-length bts))
                         bts
                         (subbytes bts 0 res))))
           (loop)]))))))

(forward-output-back current-output-pipe-in "stdout")
(forward-output-back current-error-pipe-in "stderr")
(forward-output-back current-value-pipe-in "value")

(define user-break-parameterization
  (parameterize-break 
   #t 
   (current-break-parameterization)))

;; when running code inside DrRacket directly, this parameter is looked at
;; by the current-eval handler but here we don't set that handler, so we
;; just make a dummy parameter that's ignored
(define outermost (make-parameter #f))

;; these two hopeless functions are stub versions of the functions with the same names
;; in module-language.rkt. here we never have direct access to the interactions window,
;; so we just report the error and then kill the process
(define (raise-hopeless-exception exn [suffix #f])
  ((error-display-handler)
   (if (exn? exn) (exn-message exn) "Interactions disabled")
   exn)
  (flush-output (current-error-port))
  (exit -1))

(define (raise-hopeless-syntax-error . error-args)
  (with-handlers ([exn:fail? raise-hopeless-exception])
    (apply raise-syntax-error '|Module Language|
           error-args)))

(define user-custodian (make-custodian))
(define user-eventspace (parameterize ([current-custodian user-custodian])
                          (make-eventspace)))
(define drracket-determined-width (make-parameter 'infinity))

(define (drracket-current-print val)
  (unless (void? val)
    (define port
      (if (equal? (current-output-port) current-output-pipe-out)
          current-value-pipe-out
          (current-output-port)))
    (parameterize ([pretty-print-columns (drracket-determined-width)])
      (print val port))
    (newline port)))

(parameterize ([current-eventspace user-eventspace])
  (queue-callback
   (λ ()
     (error-display-handler debug-error-display-handler)
     (current-print drracket-current-print)
     (current-output-port current-output-pipe-out)
     (current-error-port current-error-pipe-out))))

(let loop ()
  (match (read (current-input-port))
    [(list "complete-program" pretty-print-width submodules-to-run path-as-bytes the-bytes)
     (parameterize ([current-eventspace user-eventspace])
       (queue-callback
        (λ ()
          (drracket-determined-width pretty-print-width)

          ;; the following code is not yet working, but it is copies
          ;; of the code that the module language uses to initialize
          ;; the REPL in the user's program; it is here so we know what it all is.
          #;
          (begin
            (cond
              [the-irl
               (parameterize ([drracket:language:lang-default-annotations
                               (call-read-language the-irl
                                                   'drracket:default-instrumentation
                                                   'debug)])
                 (super on-execute settings run-in-user-thread))]
              [else (super on-execute settings run-in-user-thread)])
            ;; these are the steps that the language.rkt does in `on-execute`
            (define annotations
              (cond
                [(equal? (simple-settings-annotations setting) 'lang-default)
                 (lang-default-annotations)]
                [else (simple-settings-annotations setting)]))
            (run-in-user-thread
             (λ ()
               (case annotations
                 [(debug)
                  ;; errortrace-annotate probably comes from this:
                  #;(define-values/invoke-unit/infer stacktrace/errortrace-annotate/key-module-name@)
                  (current-compile (make-debug-compile-handler/errortrace-annotate (current-compile) errortrace-annotate))
                  (error-display-handler
                   (drracket:debug:make-debug-error-display-handler
                    (error-display-handler)))]
           
                 [(debug/profile)
                  (drracket:debug:profiling-enabled #t)
                  (error-display-handler
                   (drracket:debug:make-debug-error-display-handler
                    (error-display-handler)))
                  (current-eval (drracket:debug:make-debug-eval-handler (current-eval)))]
           
                 [(test-coverage)
                  (drracket:debug:test-coverage-enabled #t)
                  (error-display-handler
                   (drracket:debug:make-debug-error-display-handler
                    (error-display-handler)))
                  (current-eval (drracket:debug:make-debug-eval-handler (current-eval)))])
       
               (define-values (my-setup-printing-parameters
                               drracket-pretty-print-size-hook
                               drracket-pretty-print-print-hook)
                 (make-setup-printing-parameters/extras))

               (pretty-print-print-hook drracket-pretty-print-print-hook)
               (pretty-print-size-hook drracket-pretty-print-size-hook)
               (define first-time? (make-parameter #t))
               (global-port-print-handler
                (λ (value port [depth 0])
                  (define-values (converted-value write?)
                    (call-with-values (lambda () (simple-module-based-language-convert-value value setting))
                                      (case-lambda
                                        [(converted-value) (values converted-value #t)]
                                        [(converted-value write?) (values converted-value write?)])))
                  (define cols
                    (cond
                      [(not (simple-settings-insert-newlines setting)) 'infinity]
                      [(exact-integer? (print-value-columns)) (print-value-columns)]
                      [else (drracket:module-language:drracket-determined-width)]))
          
                  (my-setup-printing-parameters
                   (λ ()
                     (define (do-print)
                       (if write?
                           (pretty-write converted-value port)
                           (pretty-print converted-value port depth)))
                     (cond
                       [(first-time?)
                        (define orig-pretty-print-print-line (pretty-print-print-line))
                        (define pppl
                          (if (simple-settings-insert-newlines setting)
                              ;; when drracket:module-language:drracket-determined-width
                              ;; is set, we need to compensate for the newline
                              ;; difference, so we do this to avoid that last newline
                              (if (equal? (drracket:module-language:drracket-determined-width) 'infinity)
                                  orig-pretty-print-print-line
                                  (λ (new-line-number port len cols)
                                    (when new-line-number
                                      (orig-pretty-print-print-line new-line-number port len cols))))
                              orig-pretty-print-print-line))
                        (parameterize ([pretty-print-columns cols]
                                       [pretty-print-print-line pppl]
                                       [first-time? #f])
                          (do-print))]
                       [else (do-print)]))
                   setting
                   'infinity)))
               (current-inspector (make-inspector)) ;; this is effectively done already b/c a new process got created
               (read-case-sensitive (simple-settings-case-sensitive setting))))
            
            ;; module language steps
            ;; need to get `currently-open-files` from the drracket process
            (set-module-language-parameters 
             (module-language-settings->prefab-module-settings settings #:irl the-irl)
             #f ;; module-language-parallel-lock-client -- we don't support this
             currently-open-files))
          
          (define path (bytes->path path-as-bytes))
          (define (get-reader)
            (λ (src port)
              (define v
                (parameterize ([read-accept-reader #t])
                  (read-syntax src port)))
              (if (eof-object? v)
                  v
                  (namespace-syntax-introduce v))))
          (define repl-init-thunk (make-thread-cell #f))
          (define get-sexp/syntax/eof
            (front-end/complete-program get-reader
                                        path
                                        (λ () #f) ;; get-pre-compiled
                                        submodules-to-run
                                        'drracket:init:system-eventspace ;; ignored when the-irl is #f
                                        raise-hopeless-exception raise-hopeless-syntax-error
                                        repl-init-thunk

                                        void ;; call-set-irl-mcli-vec
                                        ;; we don't need to set-irl-mcli-vec! because we'll get the
                                        ;; drracket:submit-predicate via read-language, I believe

                                        (open-input-bytes the-bytes path)
                                        #f ;; the-irl
                                        ))

          (run-some-user-code user-break-parameterization
                              outermost
                              pretty-print-width
                              get-sexp/syntax/eof)

          ;; this prompt is the same as in rep.rkt in evaluate-from-port
          (call-with-continuation-prompt
           (λ ()
             (call-with-break-parameterization
              user-break-parameterization
              (λ ()
                ;; this is the module language's front-end/finished-complete-program
                (cond [(thread-cell-ref repl-init-thunk)
                       => (λ (t) (thread-cell-set! repl-init-thunk #f) (t))]))))
           (default-continuation-prompt-tag)
           (λ args (void)))

          (flush-output current-output-pipe-out)
          (flush-output current-error-pipe-out)
          (send-msg `("finished-evaluation")))))
     (loop)]
    [(list "interaction" pretty-print-width the-bytes)
     (parameterize ([current-eventspace user-eventspace])
       (queue-callback
        (λ ()
          (drracket-determined-width pretty-print-width)
          (define get-sexp/syntax/eof
            (front-end/interaction (open-input-bytes the-bytes #f)))
          (run-some-user-code user-break-parameterization
                              outermost
                              pretty-print-width
                              get-sexp/syntax/eof)
          (flush-output current-output-pipe-out)
          (flush-output current-error-pipe-out)
          (send-msg `("finished-evaluation")))))
     (loop)]
    [(? eof-object?)
     (exit 0)]))
