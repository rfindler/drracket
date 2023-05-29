#lang racket/base
(provide syncheck-text<%> annotations<%>
         blueboxes-gui^)
(require racket/class 
         racket/unit
         drracket/private/syncheck/syncheck-intf
         "local-member-names.rkt")

(define syncheck-text<%>
  (interface (syncheck-annotations<%>)
    syncheck:init-arrows
    syncheck:clear-arrows
    syncheck:arrows-visible?
    syncheck:get-bindings-table
    syncheck:jump-to-next-bound-occurrence
    syncheck:jump-to-binding-occurrence
    syncheck:jump-to-definition
    syncheck:rename-identifier
    syncheck:tack/untack-arrows))


;; implemented by the editor object that
;; holds the annotations object, but put
;; here for dependencies reasons
(define annotations<%>
  (interface ()
    get-annotations
    set-annotations
    after-annotations-change))

(define-signature blueboxes-gui^
  (docs-text-defs-mixin
   docs-text-ints-mixin
   docs-editor-canvas-mixin))
