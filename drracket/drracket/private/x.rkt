#lang racket

(require racket/class
         racket/gui/base
         framework
         "../acks.rkt"
         "frame-icon.rkt"
         pict/snip pict)

(define about-frame%
  (class (frame:standard-menus-mixin frame:basic%)
    (init-field main-text)
    (inherit close)
    (define/override (on-subwindow-char receiver event)
      (cond
        [(equal? (send event get-key-code) 'escape)
         (close)]
        [else
         (super on-subwindow-char receiver event)]))
    (define/private (edit-menu:do const)
      (send main-text do-edit-operation const))
    [define/override file-menu:create-revert? (λ () #f)]
    [define/override file-menu:create-save? (λ () #f)]
    [define/override file-menu:create-save-as? (λ () #f)]
    [define/override file-menu:between-close-and-quit (λ (x) (void))]
    [define/override edit-menu:between-redo-and-cut (λ (x) (void))]
    [define/override edit-menu:between-select-all-and-find (λ (x) (void))]
    [define/override edit-menu:copy-callback (λ (menu evt) (edit-menu:do 'copy))]
    [define/override edit-menu:select-all-callback (λ (menu evt) (edit-menu:do 'select-all))]
    [define/override edit-menu:create-find? (λ () #f)]
    (super-new
     (label "About DrRacket"))))


(define (same-widths items)
  (let ([max-width (apply max (map (λ (x) (send x get-width)) items))])
    (for-each (λ (x) (send x min-width max-width)) items)))

(define (same-heights items)
  (let ([max-height (apply max (map (λ (x) (send x get-height)) items))])
    (for-each (λ (x) (send x min-height max-height)) items)))

(define wrap-edit% 
  (class (text:foreground-color-mixin
          (editor:standard-style-list-mixin
           text:hide-caret/selection%))
    (inherit begin-edit-sequence end-edit-sequence
             get-max-width find-snip position-location)
    (define/augment (on-set-size-constraint)
      (begin-edit-sequence)
      (let ([snip (find-snip 1 'after-or-none)])
        (when (is-a? snip editor-snip%)
          (send (send snip get-editor) begin-edit-sequence)))
      (inner (void) on-set-size-constraint))
    (define/augment (after-set-size-constraint)
      (inner (void) after-set-size-constraint)
      (let ([width (get-max-width)]
            [snip (find-snip 1 'after-or-none)])
        (when (is-a? snip editor-snip%)
          (let ([b (box 0)])
            (position-location 1 b #f #f #t)
            (let ([new-width (- width 4 (unbox b))])
              (when (> new-width 0)
                (send snip resize new-width
                      17) ; smallest random number
                (send snip set-max-height 'none))))
          (send (send snip get-editor) end-edit-sequence)))
      (end-edit-sequence))
    (super-new)))

(define (get-plt-pict)
  (dc
   (λ (dc dx dy)
     (define smoothing (send dc get-smoothing))
     (define pen (send dc get-pen))
     (define brush (send dc get-brush))
     (define-values (sx sy) (send dc get-scale))
     (define-values (ox oy) (send dc get-origin))
     (send dc set-origin (+ ox dx) (+ oy dy))
     (send dc set-scale mb-scale-factor mb-scale-factor)
     (send dc set-smoothing 'smoothed)
     (send dc set-pen "black" 1 'transparent)
     (when (preferences:get 'framework:white-on-black?)
       (define rgn (new region% [dc dc]))
       (define old-clip (send dc get-clipping-region))
       (define pen (send dc get-pen))
       (define brush (send dc get-brush))
       (define offset 4) ;; this offset seems to make a tight fit around the actual logo
       (send rgn set-ellipse
             offset offset
             (- mb-plain-width offset offset)
             (- mb-plain-height offset offset))
       (send dc set-clipping-region rgn)
       (send dc set-brush "white" 'solid)
       (send dc set-pen "black" 1 'transparent)
       (send dc draw-rectangle 0 0  mb-plain-width  mb-plain-height)
       (send dc set-pen pen)
       (send dc set-brush brush)
       ;;; (send dc set-clipping-region old-clip)  ;;; here
       )
     (mb-main-drawing dc)
     (send dc set-pen pen)
     (send dc set-brush brush)
     (send dc set-smoothing smoothing)
     (send dc set-scale sx sy)
     (send dc set-origin ox oy))
   mb-flat-width mb-flat-height))

(define (about-drscheme)
  (let* ([e (make-object wrap-edit%)]
         [main-text (make-object wrap-edit%)]
         [plt-pict (get-plt-pict)]
         [plt-snip (new pict-snip% [pict plt-pict])]
         [editor-snip (make-object editor-snip% e #f)]
         [f (make-object about-frame% main-text)]
         [main-panel (send f get-area-container)]
         [editor-canvas (make-object canvas:color% main-panel)]
         [button-panel (make-object horizontal-panel% main-panel)]
         [top (make-object style-delta% 'change-alignment 'top)]
         [d-usual (make-object style-delta% 'change-family 'decorative)]
         [d-dr (make-object style-delta%)]
         
         [insert/clickback
          (λ (str clickback)
            (send e change-style (gui-utils:get-clickback-delta
                                  (preferences:get 'framework:white-on-black?)))
            (let* ([before (send e get-start-position)]
                   [_ (send e insert str)]
                   [after (send e get-start-position)])
              (send e set-clickback before after 
                    (λ (a b c) (clickback))
                    (gui-utils:get-clickback-delta
                     (preferences:get 'framework:white-on-black?))))
            (send e change-style d-usual))]
         
         [insert-url/external-browser
          (λ (str url)
            (insert/clickback str void))])
    
    (send* d-usual 
      (set-delta-foreground (if (preferences:get 'framework:white-on-black?) "white" "black"))
      (set-delta 'change-underline #f))
    
    (send* d-dr (copy d-usual) (set-delta 'change-bold))
    (send d-usual set-weight-on 'normal)
    (send* editor-canvas
      (set-editor main-text)
      (stretchable-width #t)
      (stretchable-height #t))
    
    (send* editor-canvas
      (min-width (inexact->exact (floor (+ (* 5/2 (pict-width plt-pict)) 50))))
      (min-height (inexact->exact (round (+ (pict-height plt-pict) 50)))))
    
    (send* e 
      (change-style d-dr)
      (insert "Welcome....")
      (change-style d-usual))
    
    (send e insert " by ")
    
    (insert-url/external-browser "PLT" "http://racket-lang.org/")
    
    (send* e
      (insert ".\n\n")
      (insert (get-authors))
      (insert "\n\nFor licensing information see "))
    
    (insert/clickback "our software license" void)
    
    (send* e
      (insert ".\n\nBased on:\n  ")
      (insert (banner)))
    
    (send e insert "\n")
    (send e insert (get-translating-acks))
    
    (let* ([docs-button (new button% 
                             [label "Help Desk"]
                             [parent button-panel]
                             [callback void])])
      (send docs-button focus))
    (send button-panel stretchable-height #f)
    (send button-panel set-alignment 'center 'center)
    (send* e
      (auto-wrap #t)
      (set-autowrap-bitmap #f))
    (send* main-text 
      (set-autowrap-bitmap #f)
      (auto-wrap #t)
      (insert plt-snip)
      (insert editor-snip)
      (change-style top 0 2)
      (hide-caret #t))
    (send editor-snip use-style-background #t)
    (send f reflow-container)
    
    (send* main-text
      (set-position 1)
      (scroll-to-position 0)
      (lock #t))
    
    (send* e
      (set-position 0)
      (scroll-to-position 0)
      (lock #t))
    
    (when (eq? (system-type) 'macosx)
      ;; otherwise, the focus is the tour button, as above
      (send editor-canvas focus))
    
    (send f show #t)
    f))

(void (about-drscheme))
