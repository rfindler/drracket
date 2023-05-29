#lang racket/base
(require "private/syncheck/gui.rkt"
         "private/syncheck/blueboxes-gui.rkt"
         drracket/tool
         racket/unit)
(provide tool@)
(define-compound-unit/infer tool@
  (import [T : drracket:tool^])
  (export drracket:tool-exports^)
  (link blueboxes-gui@ gui@))
