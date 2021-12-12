#lang info

(define collection 'multi)

(define deps '("base" "scribble-lib" "drracket-tool-lib"))
(define build-deps '("racket-doc" "gui-doc" "gui-lib" "drracket"
                                  "syntax-color-lib" "syntax-color-doc"))

(define pkg-desc "Docs for the programmatic interface to some IDE tools that DrRacket supports")

(define pkg-authors '(robby))

(define version "1.2")

(define license
  '(Apache-2.0 OR MIT))
