#lang racket/base
(require "run-module-language-program.rkt"
         racket/match)


(define original-output-port (current-output-port))
(define original-error-port (current-error-port))

(define oprintf
  (λ args
    (apply fprintf original-error-port args)
    (flush-output original-error-port)))

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
           (loop)]))))))

(forward-output-back current-output-pipe-in "stdout")
(forward-output-back current-error-pipe-in "stderr")

(current-output-port current-output-pipe-out)
(current-error-port current-error-pipe-out)

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

(let loop ()
  (match (read (current-input-port))
    [(list "complete-program" pretty-print-width submodules-to-run path-as-bytes the-bytes)
     (define path (bytes->path path-as-bytes))
     (define (get-reader)
       (λ (src port)
         (define v
           (parameterize ([read-accept-reader #t])
             (read-syntax src port)))
         (if (eof-object? v)
             v
             (namespace-syntax-introduce v))))
     (define get-sexp/syntax/eof
       (front-end/complete-program get-reader
                                   path
                                   (λ () #f) ;; get-pre-compiled
                                   submodules-to-run
                                   'drracket:init:system-eventspace ;; ignored when the-irl is #f
                                   raise-hopeless-exception raise-hopeless-syntax-error
                                   (open-input-bytes the-bytes path)
                                   #f ;; the-irl
                                   ))
     (run-some-user-code user-break-parameterization
                         outermost
                         pretty-print-width
                         get-sexp/syntax/eof)
     (flush-output current-output-pipe-out)
     (flush-output current-error-pipe-out)
     (writeln `("finished-evaluation") original-output-port)
     (loop)]
    [(? eof-object?)
     (exit 0)]))
