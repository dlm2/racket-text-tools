;;  Hi-lock: ((";*;|.*" (0 (quote fg-gray35) t))) ;;  Hi-lock: ((";*;-.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";*;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("^/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("let\\*-values\\|let-values\\|define-values\\|when\\|case-lambda\\|make-struct-type\\|define-struct\\|def-stx-rules\\|def-stx\\|with-handlers\\|begin0\\|ƛ" (0 (quote keyword) t))) ;;  Hi-lock: (("define[/\-][a-zA-Z0-9\-\_]+" (0 (quote keyword) t))) ;;  Hi-lock: ((";;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";;$.*" (0 (quote fg-red4) t))) ;;  Hi-lock: (("[a-zA-Z0-9\-\_]+%" (0 (quote class-var) t))) ;;  Hi-lock: (("[( ']>[a-zA-Z0-9\-\_/]+" (0 (quote control-id) t))) 
#||#

(module  segm-tools
         scheme
(provide
     text->tagged-segms
     tagged-segms->width-formatted-lines
     )

;;(require (lib "std.ss" "dlm"))
;;(require  (except-in  (lib "std.ss" "dlm")   push! nl))
;;(require (lib "struct-tools.ss" "dlm"))
;;(require scheme/pretty)
;;(define pp pretty-print)

(require "std.ss")

;;/------------------------------------------------------------------
(define (next-state from-state evt)  
  ;;| - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define (event->state-table from-state)
      (def-stx-rules event->state/tables ()
        ;;|     from-state          event -> target-state tables
        ;;|     -----------      -----------------------------
          [(_)  '([:start        (#\newline      :start-nln) 
                                 (#\space        :start-space)]
                  [:start-space  (#\newline      :start-nln)
                                 (#\space        :in-space)]
                  [:start-nln    (#\space        :start-space)
                                 (#\newline      :in-nln)]
                  [:start-token  (#\newline      :in-nln)
                                 (#\space        :start-space)]
                  [:in-token     (#\newline      :start-nln)
                                 (#\space        :start-space)]
                  [:in-space     (#\newline      :start-nln)
                                 (#\space        :in-space)]
                  [:in-nln       (#\newline      :in-nln)
                                 (#\space        :start-space)])])
      (def-stx-rules get-entry ()
          [(_ $from-state)
              (let*([entry  (assq  $from-state  (event->state/tables))])
                (when (not entry) 
                    (error 'err "dlm: Invalid state passed"))
                entry)])
      (cdr (get-entry from-state)))  ;|| return a table
  ;;| - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (define (event-row table evt)
      (let loop ([lst table]  [i 0])
        (if (empty? lst) 
            #f
            (if (eq? evt (caar lst))
                i
                (loop  (cdr lst)  (add1 i))))))
  ;;| - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  (let*([table     (event->state-table from-state)]
        [row       (event-row table evt)])
    (if row
        (nth  1  (nth row table))
        (case from-state     ;;| token occurrences are the default
          [(:start-token :in-token)   ':in-token]
          [else                       ':start-token]))))

;;/==================================================================

(define (text->tagged-segms text)
  ;;| Simple (flat) state machine that translates a text string 
  ;;| into  "tagged segments";  .... 

  (let*([segm     '()]   ;;| segment; intermediate collection
        [store    '()])  ;;| procedure results
    
    (def-stx-rules segm-chrs->str ()  
        [(_ $s)  (list (car $s) (list->str (cdr $s)))])
    
    (def-stx-rules add-to-segm ()
        [(_ $e)  (set! segm (cons $e segm))])

    (def-stx-rules start-tag ()
        [(_ $s)  
         (cdr (assq $s `([:start-space . :SPC]  [:start-token . :TOK]
                         [:start-nln   . :NLN])))])
    
    (def-stx-rules copy-segm->store ()
      [(_)  (set!  store  (cons (segm-chrs->str (reverse segm))
                                  store))])
    (def-stx-rules handler ()
      ;;| event handler procedure lookup
      [(_ $state)
       (case $state
         [(:start-space :start-token :start-nln)
                (ƛ (e)  ;;| store current segm
                        (when (not-empty? segm)  (copy-segm->store))
                        ;;| start a new segm w/ appropriate tag
                        (set!  segm   (list  e  (start-tag $state))))]
         [(:in-space :in-token :in-nln)
                (ƛ (e)  ;;| add to current segm
                        (add-to-segm e))])])
    
    (def-stx-rules transition ()
      ;;| For event $E (char) transitions to the next state, calls
      ;;| the appropriate handler and returns the new state.
      [(_ $state $e) 
       (begin
         ;;(printf "----------------------------------------~n")
         ;;(printf "[~s] \t[~s] ~n" $state $e)
         (let*([$state    (next-state $state $e)]) 
           ((handler $state) $e)
           $state))])

    (define cnt 0) 
    (let loop ([lst  (str->list text)]  [state ':start])
      (set! cnt (add1 cnt))
      (if (or  (empty? lst)  (> cnt 9999999))
          (begin
            (copy-segm->store)
            ;;(printf "store       : ~s~n" (reverse store))
            ;;(printf "segm        : ~s~n" (reverse segm))
            (reverse store))
          (loop  (cdr lst)  
                 (transition state (car lst)))))))

;;/------------------------------------------------------------------
(define (tagged-segms->width-formatted-lines segm-lst max-width)
  (let*([line      '()]
        [store     '()]
        [nln-cnt   0])
    
    (def-stx-rules segm-type ()       [(_ $a)  (nth 0 $a) ])
    (def-stx-rules segm-str ()        [(_ $a)  (nth 1 $a) ])
    (def-stx-rules start-of-line? ()  [(_)     (empty? line)])
    
    (def-stx-rules copy-line-to-store ()
        [(_)  (set!   store    (cons  (apply str+ (reverse line))  store))])
    
    (def-stx-rules add-empty-lines-to-store ()
        [(_ $n)  (set! store   (append  (make-list $n "")  store))])
    
    (def-stx-rules handle-newlines ()
      [(_  $type $str $w $newlines?)  
         (cond  [(= $w 1)    (set! $type ':SPC)
                             (set! $str " ")]
                [else        (set! $newlines? #t)])])
    
    (let loop ([lst segm-lst] [width 0])

      (def-stx-rules add-to-line ()
        [(_ $a)    (begin  (set! line (cons $a line))
                           (set! width  (+ (str-len $a) width)))])
      (def-stx-rules clear-line ()
        [(_)       (begin  (set! line  '()) 
                           (set! width 0))])
      
      (if (empty? lst)
          (begin  (copy-line-to-store)
                  (reverse store))
          (let*([segm         (car lst)]
                [type         (segm-type segm)]
                [str          (segm-str segm)]
                [w            (str-len str)]
                [newlines? #f]
                )
            ;;(printf "----------------------------------------~n")
            (when (and (eq? ':TOK type)  (> w max-width)) 
                (error 'err "TOK is longer than max-width"))
            
            (when (eq? ':NLN type)
                (handle-newlines type str w newlines?))
            
            (cond
             [newlines? 
                     (copy-line-to-store)
                     (clear-line)
                     (add-empty-lines-to-store (sub1 w))]
             [(<=  (+ w width)  max-width)
                     (when (or  (not-eq? ':SPC type)  (not (start-of-line?)))
                         (add-to-line  str))]
             [else   ;;| current line to full to accommodate str
                     (copy-line-to-store)
                     (cond 
                      [(eq? ':SPC type)       (clear-line)]
                      [(not-eq? ':SPC type)   (set! line   (list str))
                                              (set! width  (str-len str))]
                      [else                   (error 'err "unhandled cond")])])
            ;;(printf "segm  : ~s~n" segm)
            ;;(printf "line  : ~s~n" line)
            ;;(printf "store : ~s~n" store)
            (loop (cdr lst)  width)
            )))))



);| module