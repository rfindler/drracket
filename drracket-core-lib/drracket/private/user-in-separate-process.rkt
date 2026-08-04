#lang racket/base
(require "run-module-language-program.rkt"
         racket/match
         racket/gui/base)

(define original-output-port (current-output-port))
(define original-error-port (current-error-port))

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
     (o-e-h x))))

#|

This file runs a loop in a separate process created by DrRacket to run
Racket programs (when the 'drracket:run-in-separate-process pref is set).

It uses stdin and stdout to communicate with DrRacket, leaving stderr
for bugs in this code to hopefully have some useful debugging information

|#

(define-values (current-output-pipe-in current-output-pipe-out) (make-pipe))
(define-values (current-error-pipe-in current-error-pipe-out) (make-pipe))

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
           (writeln
            `(,name ,(if (= res (bytes-length bts))
                         bts
                         (subbytes bts 0 res)))
            original-output-port)
           (flush-output original-output-port)
           (loop)]))))))

(forward-output-back current-output-pipe-in "stdout")
(forward-output-back current-error-pipe-in "stderr")

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

(parameterize ([current-eventspace user-eventspace])
  (queue-callback
   (λ ()
     (current-output-port current-output-pipe-out)
     (current-error-port current-error-pipe-out))))

(let loop ()
  (match (read (current-input-port))
    [(list "complete-program" pretty-print-width submodules-to-run path-as-bytes the-bytes)
     (parameterize ([current-eventspace user-eventspace])
       (queue-callback
        (λ ()
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
          (writeln `("finished-evaluation") original-output-port)
          (flush-output original-output-port))))
     (loop)]
    [(list "interaction" pretty-print-width the-bytes)
     (parameterize ([current-eventspace user-eventspace])
       (queue-callback
        (λ ()
          (define get-sexp/syntax/eof
            (front-end/interaction (open-input-bytes the-bytes #f)))
          (run-some-user-code user-break-parameterization
                              outermost
                              pretty-print-width
                              get-sexp/syntax/eof)
          (flush-output current-output-pipe-out)
          (flush-output current-error-pipe-out)
          (writeln `("finished-evaluation") original-output-port)
          (flush-output original-output-port))))
     (loop)]
    [(? eof-object?)
     (exit 0)]))
