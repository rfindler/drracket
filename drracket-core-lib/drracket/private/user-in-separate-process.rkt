#lang racket/base
(require "eval-helpers-and-pref-init.rkt"
         racket/match)

(let loop ()
  (match (read-line (current-input-port))
    [(list "whole program" pretty-print-width the-bytes)
     #;
     (run-some-user-code user-break-parameterization outermost pretty-print-width
                         get-sexp/syntax/eof)
     ;... this needs to do the same thing as in rep.rkt on 1194
     ;... using bts as the content of the port
     'hm]
    [(? eof-object?) (exit 0)]))