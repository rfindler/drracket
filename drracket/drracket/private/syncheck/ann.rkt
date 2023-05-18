#lang racket
(require "intf.rkt"
         "../tooltip.rkt"
         drracket/private/syncheck/blueboxes-gui
         drracket/private/syncheck/syncheck-intf
         drracket/private/syncheck/syncheck-local-member-names
         data/interval-map

         ;; these dependencies should naturally
         ;; go away as the GUI ones go, as they're
         ;; used in callbacks
         net/url
         browser/external

         ;; these dependencies needs to go away
         ;; for this API to be shared with Emacs
         racket/gui
         framework
         string-constants/string-constant
         )

;; todo:
;;  - audit names in `define-local-member-name` in blueboxes-gui.rkt
;;  - clear-docs-range and on-insert and on-delete in blueboxes gui need
;;    to make their way into the annotations object

(provide (struct-out arrow)
         (struct-out var-arrow)
         (struct-out tail-arrow)
         (struct-out colored-region)
         (struct-out tooltip-info)
         (struct-out def-link)
         cs-check-syntax-background-colors
         ann-monitor-start
         ann-monitor-done
         ann-monitor)

(define cs-check-syntax-background-colors
  (hash 'matching-identifiers 'drracket:syncheck:matching-identifiers
        'unused-identifier 'drracket:syncheck:unused-identifier
        'document-identifier 'drracket:syncheck:document-identifier))

;; color : (or/c (is-a?/c color%) string? color-prefs:color-scheme-color-name?)
;; text: text:basic<%>
;; start, fin: number
;; used to represent regions to highlight when passing the mouse over the syncheck window
(define-struct colored-region (color text start fin) #:transparent)

(define-struct arrow () #:mutable #:transparent)
(define-struct (var-arrow arrow)
  (start-text start-pos-left start-pos-right start-px start-py
              end-text end-pos-left end-pos-right end-px end-py
              actual? level require-arrow? name-dup?)
  ;; level is one of 'lexical, 'top-level, 'import
  #:transparent)
(define-struct (tail-arrow arrow) (from-text from-pos to-text to-pos) #:transparent)
    
(define-struct tooltip-info (text pos-left pos-right msg) #:transparent)

;; id : symbol  --  the nominal-source-id from identifier-binding
;; filename : path
(define-struct def-link (id filename submods) #:transparent)

(define ann%
  (class* object% (syncheck-annotations<%>)

    ;; unused-require-table : hash-table[(list text number number) -o> #t]
    ;; this table records if a given require appears to be unused
    (define unused-require-table (make-hash))

    ;; prefix-table : hash-table[(list text number number) -o> #t]
    ;;   this table records if a given require appears to have already a prefix
    (define prefix-table (make-hash))

    (define/public (is-prefix-require? text start end)
      (hash-ref prefix-table (list text start end) #f))
    
    ;; arrow-records : (U #f hash[text% => arrow-record])
    ;; arrow-record = interval-map[(listof arrow-entry)]
    ;; arrow-entry is one of
    ;;   - (cons (U #f sym) (menu -> void))
    ;;   - def-link
    ;;   - tail-link
    ;;   - arrow
    ;;   - string
    ;;   - colored-region
    (define/private (get-arrow-record text)
      (unless (object? text)
        (error 'get-arrow-record "expected a text as the second argument, got ~e" text))
      (hash-ref! arrow-records text (lambda () (make-interval-map))))

    (define arrow-records #f)

    ;; definition-targets : hash-table[(list symbol[id-name] (listof symbol[submodname])) 
    ;;                                 -o> (list text number number)]
    (define definition-targets (make-hash))
    
    ;; syncheck:find-definition-target 
    ;;  : sym (listof sym) -> (or/c (list/c text number number) #f)
    (define/public (syncheck:find-definition-target id mods)
      (hash-ref definition-targets (list id mods) #f))
            
    (define/public (get-arrows txt pos)
      (cond
        [arrow-records
         (define im (hash-ref arrow-records txt #f))
         (if im
             (interval-map-ref im pos '())
             '())]
        [else #f]))

    (define/public (get-unused-requires)
      (hash-keys unused-require-table))
    (define/public (clear-unused-requires)
      (hash-clear! unused-require-table))
              
    (define/public (dump-arrow-records)
      (cond
        [arrow-records
         (for ([(k v) (in-hash arrow-records)])
           (printf "\n\n~s:\n" k)
           (let loop ([it (interval-map-iterate-first v)])
             (when it
               (printf "~s =>\n" (interval-map-iterate-key v it))
               (for ([v (in-list (interval-map-iterate-value v it))])
                 (printf "  ~s\n" v))
               (printf "\n")
               (loop (interval-map-iterate-next v it)))))]
        [else
         (printf "arrow-records empty\n")]))
            
    
    ;; bindings-table : hash-table[(list text number number)
    ;;                             -o> (setof (list text number number))]
    ;; this is a private field
    (define bindings-table (make-hash))

    ;; add-to-bindings-table : text number number text number number -> boolean
    ;; results indicates if the binding was added to the table. It is added, unless
    ;;  1) it is already there, or
    ;;  2) it is a link to itself
    (define/private (add-to-bindings-table start-text start-left start-right
                                           end-text end-left end-right)
      (cond
        [(and (object=? start-text end-text)
              (= start-left end-left)
              (= start-right end-right))
         #f]
        [else
         (define key (list start-text start-left start-right))
         (define priors (hash-ref bindings-table key (λ () (set))))
         (define new (list end-text end-left end-right))
         (cond
           [(set-member? priors new)
            #f]
           [else
            (hash-set! bindings-table key (set-add priors new))
            #t])]))

    ;; for use in the automatic test suite (both)
    (define/public (syncheck:get-bindings-table [tooltips? #f])
      (cond
        [tooltips?
         (define unsorted
           (apply 
            append
            (for/list ([(k interval-map) (in-hash arrow-records)])
              (apply
               append
               (dict-map
                interval-map
                (λ (key x)
                  (for/list ([x (in-list x)]
                             #:when (tooltip-info? x))
                    (list (tooltip-info-pos-left x)
                          (tooltip-info-pos-right x)
                          (tooltip-info-msg x)))))))))
         (define (compare l1 l2)
           (cond
             [(equal? (list-ref l1 0) (list-ref l2 0))
              (cond
                [(equal? (list-ref l1 2) (list-ref l2 2))
                 (string<=? (list-ref l1 2) (list-ref l2 2))]
                [else
                 (< (list-ref l1 1) (list-ref l2 1))])]
             [else
              (< (list-ref l1 0) (list-ref l2 0))]))
         (sort unsorted compare)]
        [else
         bindings-table]))

    (define require-candidates (set))
    (define/public (get-require-candidates) require-candidates)

    (define docs-im #f)
    (define/private (get-docs-im) docs-im)
    (define/private (get/start-docs-im) 
      (cond
        [docs-im docs-im]
        [else
         (set! docs-im (make-interval-map))
         docs-im]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define/private (syncheck:add-menu text start-pos end-pos key make-menu)
      (when arrow-records
        (when (<= 0 start-pos end-pos) ;; this used to guard against the last position in the editor, no longer
          (add-to-range/key text start-pos end-pos make-menu key (and key #t)))))

    (define/public (syncheck:add-text-type text start fin text-type)
      (when arrow-records
        (when (is-a? text 'text:basic<%>)
          (when (hash-has-key? cs-check-syntax-background-colors text-type)
            (define color
              (hash-ref cs-check-syntax-background-colors text-type))
            (add-to-range/key text start fin
                              (make-colored-region color text start fin)
                              #f #f)))))

    ;; these three methods are no longer used; see docs for more
    (define/public (syncheck:add-background-color text start fin raw-color)
      (when arrow-records
        (when (is-a? text 'atext:basic<%>)
          ;; we adjust the colors over here based on the white-on-black
          ;; preference so we don't have to have the preference set up
          ;; in the other place when running check syntax in online mode.
          (define color 
            (if (preferences:get 'framework:white-on-black?)
                (cond
                  [(equal? raw-color "palegreen") "darkgreen"]
                  [else raw-color])
                raw-color))
          (add-to-range/key text start fin
                            (make-colored-region color text start fin)
                            #f #f))))
    (define/public (syncheck:add-arrow start-text start-pos-left start-pos-right
                                       end-text end-pos-left end-pos-right
                                       actual? level)
      (void))
    (define/public (syncheck:add-arrow/name-dup start-text
                                                start-pos-left start-pos-right
                                                end-text
                                                end-pos-left end-pos-right
                                                actual? level require-arrow? name-dup?)
      (void))
            
    ;; pre: start-editor, end-editor are embedded in `this' (or are `this')
    (define/public (syncheck:add-arrow/name-dup/pxpy start-text
                                                     start-pos-left start-pos-right
                                                     start-px start-py
                                                     end-text
                                                     end-pos-left end-pos-right
                                                     end-px end-py
                                                     actual? level require-arrow? name-dup?)
      (when (and arrow-records
                 (preferences:get 'drracket:syncheck:show-arrows?))
        (when (add-to-bindings-table
               start-text start-pos-left start-pos-right
               end-text end-pos-left end-pos-right)
          (let ([arrow (make-var-arrow start-text start-pos-left start-pos-right
                                       start-px start-py
                                       end-text end-pos-left end-pos-right
                                       end-px end-py
                                       actual? level require-arrow? name-dup?)])
            (add-to-range/key start-text start-pos-left start-pos-right arrow #f #f)
            (add-to-range/key end-text end-pos-left end-pos-right arrow #f #f)))))
            
    ;; syncheck:add-tail-arrow : text number text number -> void
    (define/public (syncheck:add-tail-arrow from-text from-pos to-text to-pos)
      (when (and arrow-records
                 (preferences:get 'drracket:syncheck:show-arrows?))
        (let ([tail-arrow (make-tail-arrow to-text to-pos from-text from-pos)])
          (add-to-range/key from-text from-pos (+ from-pos 1) tail-arrow #f #f)
          (add-to-range/key to-text to-pos (+ to-pos 1) tail-arrow #f #f))))
            
    (define/public (syncheck:add-jump-to-definition text start end id filename submods)
      (when arrow-records
        (add-to-range/key text start end (make-def-link id filename submods) #f #f)))
    (define/public (syncheck:add-jump-to-definition/phase-level+space text start end id filename submods phase-level)
      (syncheck:add-jump-to-definition text start end id filename submods))

    (define/public (syncheck:add-prefixed-require-reference req-text
                                                            req-pos-left
                                                            req-pos-right
                                                            prefix
                                                            prefix-src
                                                            prefix-left
                                                            prefix-right)
      (hash-set! prefix-table (list req-text req-pos-left req-pos-right) #t))

    (define/public (syncheck:add-unused-require req-text
                                                req-pos-left
                                                req-pos-right)
      (hash-set! unused-require-table (list req-text req-pos-left req-pos-right) #t))
            
    ;; syncheck:add-mouse-over-status : text pos-left pos-right string -> void
    (define/public (syncheck:add-mouse-over-status text pos-left pos-right str)
      (when arrow-records
        (add-to-range/key text pos-left pos-right 
                          (make-tooltip-info text pos-left pos-right str)
                          #f #f)))
    
    (define/public (syncheck:add-require-open-menu text start-pos end-pos file)
      (define (make-require-open-menu menu)
        (define-values (base name dir?) (split-path file))
        (new menu-item%
             (label (gui-utils:format-literal-label
                     (string-constant cs-open-file) (path->string name)))
             (parent menu)
             (callback (λ (x y) (handler:edit-file file))))
        (void))
      (syncheck:add-menu text start-pos end-pos file make-require-open-menu)
      (set! require-candidates (set-add require-candidates file)))
            
    (define/public (syncheck:add-docs-menu text start-pos end-pos id
                                           the-label
                                           path
                                           definition-tag
                                           url-tag)
      (syncheck:add-docs-range start-pos end-pos definition-tag path url-tag)
      (define (visit-docs-url)
        (define url (path->url path))
        (define url2 (if url-tag
                         (make-url (url-scheme url)
                                   (url-user url)
                                   (url-host url)
                                   (url-port url)
                                   (url-path-absolute? url)
                                   (url-path url)
                                   (url-query url)
                                   url-tag)
                         url))
        (send-url (url->string url2)))
      (syncheck:add-menu 
       text start-pos end-pos id
       (λ (menu)
         (new menu-item% 
              [parent menu]
              [label (gui-utils:format-literal-label "~a" the-label)]
              [callback
               (λ (x y)
                 (visit-docs-url))]))))

    
    (define/public (syncheck:add-docs-range start end tag path url-tag)
      ;; the +1 to end is effectively assuming that there
      ;; are no abutting identifiers with documentation
      (define rng (list start (+ end 1) tag path url-tag))
      (interval-map-set! (get/start-docs-im) start (+ end 1) rng))
            
    (define/public (syncheck:add-definition-target/phase-level+space source start-pos end-pos id mods phase-level)
      (syncheck:add-definition-target source start-pos end-pos id mods))
    (define/public (syncheck:add-definition-target source start-pos end-pos id mods)
      (hash-set! definition-targets (list id mods) (list source start-pos end-pos)))

    ;; no longer used, but must be here for backwards compatibility
    (define/public (syncheck:add-rename-menu id to-be-renamed/poss name-dup?) (void))
    (define/public (syncheck:add-id-set to-be-renamed/poss name-dup?) (void))
   
            
    (define/public (syncheck:color-range source start finish style-name)
      (error 'syncheck:color-range "not supported"))

    (define/public (syncheck:find-source-object stx)
      (cond
        [(not (syntax-source stx)) #f]
        [(and (symbol? (syntax-source stx))
              (text:lookup-port-name (syntax-source stx)))
         => values]
        [else
         (let txt-loop ([text this])
           (cond
             [(and (is-a? text text:basic<%>)
                   (send text port-name-matches? (syntax-source stx)))
              text]
             [else
              (let snip-loop ([snip (send text find-first-snip)])
                (cond
                  [(not snip)
                   #f]
                  [(and (is-a? snip editor-snip%)
                        (send snip get-editor))
                   (or (txt-loop (send snip get-editor))
                       (snip-loop (send snip next)))]
                  [else 
                   (snip-loop (send snip next))]))]))]))
    
    ;; add-to-range/key : text number number any any boolean -> void
    ;; adds `key' to the range `start' - `end' in the editor
    ;; If use-key? is #t, it adds `to-add' with the key, and does not
    ;; replace a value with that key already there.
    ;; if use-key? is 'set, it adds `to-add` to a set bound to the key
    ;; in the assoc
    ;; If use-key? is #f, it adds `to-add' without a key.
    ;; pre: arrow-records is not #f
    (define/private (add-to-range/key text _start _end to-add key use-key?)
      ;; adjust the tooltip ranges to sensible values
      ;; (e.g., in bounds and not equal to each other)
      (define lp (send text last-position))

      (unless (= 0 lp)

        ;; first get them in bounds
        (define start (max 0 (min lp _start)))
        (define end (max 0 (min lp _end)))

        ;; now make sure they are in order
        (when (end . < . start) (set! end start))

        ;; now make sure they are different
        ;; (this code relies on there being at least
        ;; one character in the buffer, checked above)
        (when (= start end)
          (cond
            [(= end lp) (set! start (- end 1))]
            [else (set! end (+ start 1))]))

        (define arrow-record (get-arrow-record text))
        ;; Dropped the check (< _ (vector-length arrow-vector))
        ;; which had the following comment:
        ;;    the last test in the above and is because some syntax objects
        ;;    appear to be from the original source, but can have bogus information.
                
        ;; interval-maps use half-open intervals which works out well for positions
        ;; in the editor, since the interval [0,3) covers the characters just after
        ;; positions 0, 1, and 2, but not the character at position 3 (positions are
        ;; between characters)
        (cond [use-key?
               (interval-map-update*! arrow-record start end
                                      (λ (old)
                                        (if (for/or ([x (in-list old)])
                                              (and (pair? x) (car x) (equal? (car x) key)))
                                            old
                                            (cons (cons key to-add) old)))
                                      null)]
              [else
               (interval-map-cons*!
                arrow-record start end to-add null)])))
    
    (super-new)))

(define expansion-completed (make-channel))
(void
 (thread
  (λ ()
    (define ann-object-building-thread #f)
    (define ann-object-building-chan #f)
    (define defs-text #f)
    (let loop ()
      (sync
       (handle-evt
        expansion-completed
        (λ (defs-text+val)
          (when ann-object-building-thread
            (break-thread ann-object-building-thread))
          (define new-ann-object-building-chan (make-channel))
          (set! ann-object-building-chan new-ann-object-building-chan)
          (set! defs-text (car defs-text+val))
          (set! ann-object-building-thread
                (thread
                 (λ ()
                   (with-handlers ([exn:break? void])
                     (channel-put
                      new-ann-object-building-chan
                      (build-ann-object (car defs-text+val) (cdr defs-text+val)))))))
          (loop)))
       (handle-evt
        (or ann-object-building-chan never-evt)
        (λ (val)
          (let ([defs-text defs-text])
            (queue-callback
             (λ ()
               (finished-building-ann defs-text))))
          (set! ann-object-building-thread #f)
          (set! ann-object-building-chan #f)
          (set! defs-text #f)
          (loop))))))))

(define (finished-building-ann defs-text)
  (send defs-text syncheck:update-blue-boxes (send (send defs-text get-tab) get-ints))
  (send defs-text syncheck:update-drawn-arrows)
  (define tab (send defs-text get-tab))
  (send tab remove-bkg-running-color 'syncheck)
  (send (send tab get-frame) set-syncheck-running-mode #f))

(define (build-ann-object defs-text val)
  (define known-dead-place-channels (make-hasheq))
  (define ann (new ann%))
  (for ([trace-element (in-list val)])
    (process-trace-element known-dead-place-channels ann defs-text trace-element))
  ann)
        
(define (process-trace-element known-dead-place-channels ann defs-text trace-element)
  ;; using 'defs-text' all the time is wrong in the case of embedded editors,
  ;; but they already don't work and we've arranged for them to not appear here ....
  (match trace-element
    [`#(syncheck:add-arrow/name-dup/pxpy
        ,start-pos-left ,start-pos-right ,start-px ,start-py
        ,end-pos-left ,end-pos-right ,end-px ,end-py
        ,actual? ,level ,require-arrow? ,name-dup-pc ,name-dup-id)
     (define name-dup? (build-name-dup? name-dup-pc name-dup-id  known-dead-place-channels))
     (send ann syncheck:add-arrow/name-dup/pxpy
           defs-text start-pos-left start-pos-right start-px start-py
           defs-text end-pos-left end-pos-right end-px end-py
           actual? level require-arrow? name-dup?)]
    [`#(syncheck:add-tail-arrow ,from-pos ,to-pos)
     (send ann syncheck:add-tail-arrow defs-text from-pos defs-text to-pos)]
    [`#(syncheck:add-mouse-over-status ,pos-left ,pos-right ,str)
     (send ann syncheck:add-mouse-over-status defs-text pos-left pos-right str)]
    [`#(syncheck:add-text-type ,start ,fin ,text-type)
     (send ann syncheck:add-text-type defs-text start fin text-type)]
    [`#(syncheck:add-background-color ,start ,fin ,color) ; unused
     (send ann syncheck:add-background-color defs-text start fin color)]
    [`#(syncheck:add-jump-to-definition/phase-level+space ,start ,end ,id ,filename ,submods ,phase-level)
     (send ann syncheck:add-jump-to-definition/phase-level+space defs-text start end id filename submods phase-level)]

    [`#(syncheck:add-require-open-menu ,start-pos ,end-pos ,file)
     (send ann syncheck:add-require-open-menu defs-text start-pos end-pos file)]
    [`#(syncheck:add-docs-menu ,start-pos ,end-pos ,key ,the-label ,path ,definition-tag ,tag)
     (send ann syncheck:add-docs-menu defs-text start-pos end-pos
           key the-label path definition-tag tag)]
    [`#(syncheck:add-definition-target/phase-level+space ,start-pos ,end-pos ,id ,mods ,phase-level)
     (send ann syncheck:add-definition-target/phase-level+space defs-text start-pos end-pos id mods phase-level)]
    [`#(syncheck:add-id-set ,to-be-renamed/poss ,name-dup-pc ,name-dup-id)
     (define to-be-renamed/poss/fixed
       (for/list ([lst (in-list to-be-renamed/poss)])
         (list defs-text (list-ref lst 0) (list-ref lst 1))))
     (define name-dup? (build-name-dup? name-dup-pc name-dup-id known-dead-place-channels))
     (send ann syncheck:add-id-set to-be-renamed/poss/fixed name-dup?)]
    [`#(syncheck:add-prefixed-require-reference ,id-pos-left ,id-pos-right
                                                ,prefix ,prefix-left ,prefix-right)
     (send ann syncheck:add-prefixed-require-reference
           defs-text id-pos-left id-pos-right
           prefix defs-text prefix-left prefix-right)]
    [`#(syncheck:add-unused-require ,req-pos-left ,req-pos-right)
     (send ann syncheck:add-unused-require defs-text req-pos-left req-pos-right)]))
        
(define (build-name-dup? name-dup-pc name-dup-id known-dead-place-channels)
  (define (name-dup? name) 
    (cond
      [(hash-ref known-dead-place-channels name-dup-pc #f)
       ;; just give up here ...
       #f]
      [else
       (place-channel-put name-dup-pc (list name-dup-id name))
       (define res (sync/timeout .5 (handle-evt name-dup-pc list)))
       (cond
         [(list? res) (car res)]
         [else
          (hash-set! known-dead-place-channels name-dup-pc #t)
          #f])]))
  name-dup?)

(define (ann-monitor-start defs-text) (void))

(define (ann-monitor-done defs-text) (void))

(define (ann-monitor defs-text val)
  #;
  ;; -- do we need to let the user know that we started something? maybe not?
  (send (send defs-text get-tab) add-bkg-running-color
        'syncheck "orchid" (string-constant cs-syncheck-running))
  (channel-put expansion-completed (cons defs-text val))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;   below is the old code which is complicated by
  ;;   the lack of threads
  ;;
  
  ;; replay-state = 
  ;;  (or/c #f                  -- no replay running
  ;;        (box #t             -- keep running this replay
  ;;             (listof (listof stuff))
  ;;                            -- pick up some new elements to add to the current replay
  ;;             #f))           -- doesn't actually get set on a tab, but this means to
  ;;                               just stop running the replay

  #;
  (begin
  (define tab (send defs-text get-tab))    
  (when (send tab get-next-trace-refresh?)
    (define old-replay-state (send tab get-replay-state))
    (when (box? old-replay-state)
      (set-box! old-replay-state #f))
    (send tab set-replay-state #f)
    (send tab set-next-trace-refresh #f)
            
    ;; reset any previous check syntax information
    (send tab syncheck:clear-error-message)
    (send tab syncheck:clear-highlighting)
    (send defs-text syncheck:reset-docs-im)
    (send tab add-bkg-running-color 'syncheck "orchid" (string-constant cs-syncheck-running))
    (send defs-text syncheck:init-arrows))

  (define drr-frame (send (send defs-text get-tab) get-frame))
  (cond
    [(string? val) ;; an internal error happened
     (send tab remove-bkg-running-color 'syncheck)
     (send tab show-online-internal-error val)]
    [else
     (define current-replay-state (send tab get-replay-state))
     (cond
       [(not current-replay-state)
        (define new-replay-state (box '()))
        (send tab set-replay-state new-replay-state)
        (send drr-frame replay-compile-comp-trace
              defs-text
              val
              (box '()))] ;; should this box be new-replay-state instead?
       [else
        (set-box! current-replay-state
                  (append (unbox current-replay-state) (list val)))])])))
