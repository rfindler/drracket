#lang racket/base

#|


Will not work with the definitions text surrogate interposition that
#lang allows. Need to deprecate that one

--> scribble (in at-exp-lib) uses it to add keybindings; should
    add support for that directly

--> it may take a long time for the read-language call to return (or one
    of the others, I suppose?) use a separate thread?

|#

(require racket/gui/base
         racket/class
         racket/contract
         racket/runtime-path
         racket/port
         syntax-color/racket-lexer
         syntax-color/lexer-contract
         drracket/syncheck-drracket-button
         (submod drracket/insulated-read-language skip-past-comments)
         (for-syntax racket/base))
(provide
 (contract-out
  #:∀ S
  [pick-new-language
   (-> (is-a?/c text%)
       (listof (object/c [metadata->settings (->m string? S)]))
       (or/c #f object?)
       S
       (values (or/c #f object?)
               (or/c S #f)))]
  [looks-like-module?
   (-> (is-a?/c text%) boolean?)]))

(define (pick-new-language text all-languages module-language
                           module-language-settings)
  (with-handlers ([exn:fail:read? (λ (x) (values #f #f))])
    (define found-language? #f)
    (define settings #f)
    (for ([lang (in-list all-languages)])
      (define lang-spec (send lang get-reader-module))
      (when lang-spec
        (let* ([lines (send lang get-metadata-lines)]
               [str (send text get-text
                          0
                          (send text paragraph-end-position (- lines 1)))]
               [sp (open-input-string str)])
          (when (regexp-match #rx"#reader" sp)
            (define spec-in-file (read sp))
            (when (equal? lang-spec spec-in-file)
              (set! found-language? lang)
              (set! settings (send lang metadata->settings str))
              (send text while-unlocked
                    (λ () 
                      (send text delete 0
                            (send text paragraph-start-position lines)))))))))
      
    ;; check to see if it looks like the module language.
    (unless found-language?
      (when module-language
        (when (looks-like-module? text)
          (set! found-language? module-language)
          (set! settings module-language-settings))))
    (values found-language?
            settings)))

(define (looks-like-module? text)
  (or (looks-like-new-module-style? text)
      (looks-like-old-module-style? text)))

(define (looks-like-old-module-style? text)
  (with-handlers ([exn:fail:read? (λ (x) #f)])
    (define tp (open-input-text-editor text 0 'end (lambda (s) s) text #t))
    (define r1 (parameterize ([read-accept-reader #f]) (read tp)))
    (define r2 (parameterize ([read-accept-reader #f]) (read tp)))
    (and (eof-object? r2)
         (pair? r1)
         (eq? (car r1) 'module))))

(define (looks-like-new-module-style? text)
  (looks-like-new-module-style?/port
    (open-input-text-editor text 0 'end (lambda (s) s) text #t)))

(define (looks-like-new-module-style?/port special-tp)
  (define (special-filter f bytes)
    ;; @ is not accepted anywhere in either of the regexps below
    (bytes-set! bytes 0 (char->integer #\@))
    1)
  (define tp (special-filter-input-port special-tp special-filter))
  (skip-past-comments tp)
  (or (regexp-match? #rx"^#lang " (peeking-input-port tp))
      (regexp-match? #rx"^#![a-zA-Z0-9+-_]" tp)))

(module+ test
  (require rackunit racket/port)

  (check-equal? (let ([t (new text%)])
                  (send t insert "#lang racket/base")
                  (looks-like-new-module-style? t))
                #t)
  (check-equal? (let ([t (new text%)])
                  (send t insert "#ang racket/base")
                  (looks-like-new-module-style? t))
                #f)
  (check-equal? (let ([t (new text%)])
                  (send t insert ";; abc\n#lang racket/base")
                  (looks-like-new-module-style? t))
                #t)
  (check-equal? (let ([t (new text%)])
                  (send t insert ";; abc\n #lang racket/base")
                  (looks-like-new-module-style? t))
                #t)
  (check-equal? (let ([t (new text%)])
                  (send t insert ";; abc\n #!r6rs")
                  (looks-like-new-module-style? t))
                #t)

  (let ()
    (define-values (in out) (make-pipe-with-specials))
    (thread
     (λ ()
       (write-special '(x) out)
       (display "\n1" out)
       (close-output-port out)))
    (check-false (looks-like-new-module-style?/port in)))

  (check-false (looks-like-new-module-style?/port
                (open-input-string "(module m racket/base")))
  (check-true (looks-like-new-module-style?/port
               (open-input-string "#lang racket/base")))
  (check-true (looks-like-new-module-style?/port (open-input-string "#!r")))
  (check-false (looks-like-new-module-style?/port (open-input-string "#langg")))
  (check-false (looks-like-new-module-style?/port (open-input-string "#la"))))
