#lang racket/base
(require racket/gui/base "private/key.rkt")

(define debugging? (getenv "PLTDRDEBUG"))
(define profiling? (getenv "PLTDRPROFILE"))

(define install-cm? (and (not debugging?)
                         (getenv "PLTDRCM")))

(define cm-trace? (or (equal? (getenv "PLTDRCM") "trace")
                      (equal? (getenv "PLTDRDEBUG") "trace")))


;; the flush is only here to ensure that the output is 
;; appears when running in cygwin under windows.
(define (flprintf fmt . args)
  (apply printf fmt args)
  (flush-output))

(when debugging?
  (flprintf "PLTDRDEBUG: installing CM to load/create errortrace zos\n")
  (let-values ([(zo-compile
                 make-compilation-manager-load/use-compiled-handler
                 manager-trace-handler)
                (parameterize ([current-namespace (make-base-empty-namespace)]
                               [use-compiled-file-paths '()])
                  (values
                   (dynamic-require 'errortrace/zo-compile 'zo-compile)
                   (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)
                   (dynamic-require 'compiler/cm 'manager-trace-handler)))])
    (current-compile zo-compile)
    (use-compiled-file-paths (list (build-path "compiled" "errortrace")))
    (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
    (error-display-handler (dynamic-require 'errortrace/errortrace-lib
                                            'errortrace-error-display-handler))
    (when cm-trace?
      (flprintf "PLTDRDEBUG: enabling CM tracing\n")
      (manager-trace-handler
       (λ (x) 
         (when (regexp-match #rx"compiling:|end compile:" x)
           (display "1: ") (display x) (newline) (flush-output)))))))

(when install-cm?
  (flprintf "PLTDRCM: installing compilation manager\n")
  (let-values ([(make-compilation-manager-load/use-compiled-handler
                 manager-trace-handler)
                (parameterize ([current-namespace (make-base-empty-namespace)])
                  (values
                   (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)
                   (dynamic-require 'compiler/cm 'manager-trace-handler)))])
    (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
    (when cm-trace?
      (flprintf "PLTDRCM: enabling CM tracing\n")
      (let ([evt (make-log-receiver (current-logger) 'info)])
        (void
         (thread
          (λ ()
            (let loop ()
              (define vec (sync evt))
              (define str (vector-ref vec 1))
              (when (regexp-match #rx"^cm: *compil(ing|ed)" str)
                (display str)
                (newline))
              (loop)))))))))

(when profiling?
  (flprintf "PLTDRPROFILE: installing profiler\n")
  ;; NOTE that this might not always work.
  ;; it creates a new custodian and installs it, but the
  ;; original eventspace was created on the original custodian
  ;; and this code does not create a new eventspace. 
  (let ([orig-cust (current-custodian)]
        [orig-eventspace (current-eventspace)]
        [new-cust (make-custodian)])
    (current-custodian new-cust)
    ((dynamic-require 'drracket/private/profile-drs 'start-profile) orig-cust)))

(dynamic-require 'drracket/private/drracket-normal #f)