#lang scribble/manual
@(require (for-label racket drracket/insulated-read-language
                     syntax-color/lexer-contract
                     syntax-color/color-textoid)
          racket/match
          racket/contract
          scribble/examples
          drracket/insulated-read-language)

@(define the-eval (make-base-eval))
@(the-eval '(require drracket/insulated-read-language))

@title{Insulated Read Language}
@defmodule[drracket/insulated-read-language]

This library manages access to information about a
@tt{#lang}-based language that is available via
@racket[read-language]. The language's
@racket[read-language] function is not trusted to work
correctly and this library creates a new kind of value
called an @deftech{irl} that encapsulates access to the
@racket[read-language] function, catching errors that it
raises and substituing in default values if an error ever is
raised.

This library does not currently provide full protection
against malicious languages whose @racket[read-language]
function (or the procedures it returns) are attempting to
subvert the encapsulation. Instead, it just tries to recover
from common failure modes via catching exceptions.

@examples[
 #:eval the-eval
 #:label "Here's an example illustrating a typical use when nothing goes wrong:"

 (code:comment "instead of void, pass a real callback here to be alerted of failures")
 (define an-irl (make-simple-irl (current-directory) void))

 (code:comment "inform the irl of the language being used by")
 (code:comment "supplying it a port that it can call `read-language` on")
 (reset-irl! an-irl (open-input-string "#lang racket"))

 (code:comment "interrogate the language via `call-read-language`")
 (call-read-language an-irl 'drracket:paren-matches)]

@defproc[(make-simple-irl [dir path-string?]
                          [fail (-> exn:fail? any)])
         irl?]{

 Creates a new @tech{irl} by calling @racket[make-irl],
 passing along @racket[dir] and @racket[fail] and passing
 these for the other arguments:

 @itemlist[
 @item{
   For @racket[_keys->contracts], passes @racket[simple-irl-keys+contracts].
  }

 @item{For @racket[_shared-modules], passes the empty list
   (meaning only @racketmodname[racket/contract] and @racketmodname[racket/class] are shared}

 @item{For @racket[_in-dynamic-extent], passes @racket[(λ (t) (t))], meaning that the dynamic
   extent of a call into read language is not specially adjusted.
  }
 ]
}

@defproc[(make-irl [dir path-string?]
                   [fail (-> exn:fail? any)]
                   [keys->wrappers (hash/c symbol? syntax-info-key-details? #:immutable #t)]
                   [#:shared-modules shared-modules
                    '(racket/contract racket/class)
                    (listof (or module-path? resolved-module-path?))]
                   [#:in-dynamic-extent in-dynamic-extent (-> (-> any) any) (λ (t) (t))]
                   )
         irl?]{
Creates a new @tech{irl}.

 The @racket[dir] argument is used to set
 @racket[current-directory] and
 @racket[current-load-relative-directory] when calling into
 the language's @racket[read-language] result.

 The @racket[fail] argument is called when an error is raised
 while using the language's @racket[read-language] result.

 The @racket[shared-modules] are add to the newly created
 @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{namespace}
 used when calling the language's @racket[read-language]
 function using @racket[namespace-attach-module], copying
 from the namespace active when @racket[reset-irl!] is
 called. The modules @racketmodname[racket/contract] and
 @racketmodname[racket/class] are always
 shared, no matter what @racket[shared-modules] is.

 When calling into the @racket[read-language] function of the language,
 the @racket[in-dynamic-extent] function is always called; it is
 expected to return whatever values its argument thunk returns.
}

@defproc[(reset-irl!
          [irl irl?]
          [lang-port input-port?]
          [#:flush-cache? flush-cache? #f boolean?]
          [#:directory dir #f (or/c path-string? #f)]
          [#:in-dynamic-extent in-dynamic-extent (or/c #f (-> (-> any) any))])
         void?]{

 Changes @racket[irl] to match the language at the beginning of @racket[lang-port];
 this function calls @racket[read-language] to get the thunk it returns.

 If @racket[flush-cache?] is @racket[#t], then the namespace held inside
 @racket[irl] is discarded and a new one is created (using the same
 @racket[_shared-modules] as when the @racket[irl] was created). Pass
 @racket[#t] when the implementation of the language might have changed
 and should be reloaded. Pass @racket[#f] when the beginning of the program
 is edited and the language might be a different one, but any modules that
 both languages use do not need to be reloaded.

 If the @racket[dir] or @racket[in-dynamic-extent] are not
 @racket[#f], then the @racket[irl] is updated to them, as if
 they were passed to @racket[make-irl] in the first place.
}

@defproc[(call-read-language [irl irl?]
                             [key recognized-read-language-symbol/c]
                             [default any/c])
         any]{

 Calls the @racket[read-language] returned by the last time
 that @racket[reset-irl!] was called with @racket[irl]. If
 @racket[reset-irl!] has not yet been called with @racket[irl],
 then @racket[default] is returned.

 During the dynamic extent of the call to the
 @racket[read-language] function's result that
 @racket[call-read-language] invokes, there are exception
 handlers installed that catch any errors. Similarly, if the
 result of that key is another function, then it too is
 wrapped with checks for errors. If an error occurs, then any
 results that have come from the function are discarded and a
 default value is returned instead and the function passed as
 the @racket[_fail] argument to @racket[make-irl] is called
 with the exception. From that point forward, until
 @racket[reset-irl!] is called, the default values are used.
}

@defthing[simple-irl-keys+contracts (hash/c symbol? contract? #:immutable #t)]{
  This is a mapping from symbols to @racket[syntax-info-key-details?] value for a use of
  by @racket[call-read-language]. It is currently these; the contracts put
  on the values are as specified in
 @secref["lang-languages-customization" #:doc '(lib "scribblings/tools/tools.scrbl")];
 the contracts shown here govern what the result from @racket[call-read-language] can be;
 generally they are simpler, filling in defaults in various places.
 @(let ()
    (define documented-syms '())
    (define (one sym ctc . more)
      (set! documented-syms (sort (cons sym documented-syms) symbol<?))
      (apply item (racketvalfont (format "'~a" sym)) " : " ctc more))
    (define available-syms
      (sort (hash-keys simple-irl-keys) symbol<?))
    (begin0
      (itemlist
       @one['color-lexer @racket[lexer*/c]]
       @one['drracket:default-extension
            @racket[(and/c string? (not/c #rx"[.]"))]]
       @one['drracket:submit-predicate
            @racket[(-> input-port? boolean? boolean?)]]
       @one['drracket:paren-matches
            @racket[(listof (list/c symbol? symbol?))]]
       @one['drracket:quote-matches
            @racket[(listof char?)]]
       @one['drracket:indentation
            @racketblock[(-> (is-a?/c color-textoid<%>)
                             exact-nonnegative-integer?
                             exact-nonnegative-integer?)]]
       @one['drracket:range-indentation
            @racketblock[(-> (is-a?/c color-textoid<%>)
                             exact-nonnegative-integer?
                             exact-nonnegative-integer?
                             (or/c #f (listof (list/c exact-nonnegative-integer? string?))))]]
       @one['drracket:grouping-position
            @racketblock[(-> (is-a?/c color-textoid<%>)
                             natural? natural? (or/c 'up 'down 'backward 'forward)
                             (or/c #f natural?))]]
       @one['drracket:default-filters
            @racket[(listof (list/c string? string?))]])
      (unless (equal? documented-syms available-syms)
        (error 'insulated-read-language.scrbl
               "documented and available symbols don't match\n  docum: ~s\n  avail: ~s"
               documented-syms available-syms))))
}

@defproc[(get-read-language-port-start+end [irl irl?])
         (values (or/c #f exact-nonnegative-integer?)
                 (or/c #f exact-nonnegative-integer?))]{
  Returns the portion of the port last passed to @racket[reset-irl!]
 that determined the name of the language.

 If no language was present in the port (e.g if
 @litchar{#lang} did not appear) or @racket[reset-irl!] has
 not yet been called, this function returns two @racket[#f]s.

 Otherwise, the first restult is the place where the @litchar{#} in
 the @litchar{#lang} appeared and the second result is the end of the
 language that followed the @litchar{#lang}.

 }

@defproc[(get-read-language-last-position [irl irl?])
         (or/c #f exact-nonnegative-integer?)]{

 Returns the position where @racket[read-language] stopped reading
 in the port given to @racket[reset-irl!] or @racket[#f] if
 @racket[reset-irl!] has not yet been called.
}

@defproc[(get-read-language-name [irl irl?]) (or/c #f string?)]{

 Returns the name of the language (the portion following
 @litchar{#lang }) in the port last passed to @racket[reset-irl!],
 or @racket[#f] if one has not yet been passed.
}

@defform[(syntax-info-details
          contract-expr
          guide
          default-expr
          maybe-false-as-default?)
         #:grammar
         ([guide
           (-> maybe-pre-vars
               (dom-id guide) ...
               rng
               maybe-adjust-result
               #:failure fail-expr)
           (cond val-id
                 [question-expr guide] ...)
           color-textoid<%>
           (flat)]
          [rng (rng-id guide)
               (values (rng-id guide) ...)]
          [maybe-pre-vars
           (code:line)
           (code:line #:pre-vars ([pre-id pre-expr] ...))]
          [maybe-adjust-result
           (code:line)
           (code:line #:adjust-result adjust-expr)]
          [maybe-false-as-default
           (code:line)
           (code:line #:false-as-default false-as-default-expr)])]{

 Produces a @racket[syntax-info-key-details?] for use with @racket[make-irl],
 specifically passed in the hash in the @racket[_keys->wrappers] argument.

 When the key corresponding to a particular use of
 @racket[syntax-info-details] is passed to
 @racket[call-read-language], the contract given by
 @racket[contract-expr] is put on the resulting value. If the
 language does not support this key the result of
 @racket[default-expr] is returned and, if
 @racket[false-as-default-expr] is a true value, then the
 result of @racket[#f] from the language is replaced with the
 value of @racket[default-expr].

 The @racket[guide] specifies interations with
 the language are mediated. The primary concern is catching
 exceptions so they can be reported via the @racket[_fail]
 argument passed to @racket[make-irl] (or
 @racket[make-simple-irl]) but it also mediates other aspects
 of the communication with the language-provided functions.
 @itemlist[
 @item{
   If the @racket[guide] is
 @racket[flat], no exceptions are caught.}

 @item{If the @racket[guide] is built with @racket[->],
   then the result is expected to be a function (use the
   @racket[contract-expr] to guarantee it is a function) and
   calls to the function are wrapped with exception handlers.
   If the call to the function ever fails, the exception is
   handled and the @racket[fail-expr]'s result is used instead (the
   variables specified next to the inputs are bound to the specific
   inputs used in that call).

   If @racket[adjust-expr] is provided, its value is used instead
   of the value returned by the function; it has access to both
   the original inputs and the result of the function.

   If @racket[#:pre-vars] are specified, the corresponding expressions
   are evaluated before the function is called and the variables are
   bound and available in the @racket[fail-expr].
  }

 @item{ If the @racket[guide] is built with @racket[cond], then
   each of the @racket[question-expr]s are evaluated in order
   to determine which @racket[guide] to use. The variable @racket[val-id] is
   bound to the value returned by the language.}

 @item{If the @racket[guide] is  @racket[color-textoid<%>], then
   the value is expected to be an object implemeting the @racket[color-textoid<%>]
   interface and it is proxied. The use of @racket[color-textoid<%>] is expected
   to be in the argument of an @racket[->] and, when the function
   returns, the proxy is disabled.}
 ]
}

@defproc[(syntax-info-key-details? [val any/c]) boolean?]{
 Determines if @racket[val] was produced by @racket[syntax-info-details].
}