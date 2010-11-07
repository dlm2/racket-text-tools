;;  Hi-lock: ((";*;|.*" (0 (quote fg-gray35) t))) ;;  Hi-lock: ((";*;-.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";*;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("^/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("let\\*-values\\|let-values\\|define-values\\|when\\|case-lambda\\|make-struct-type\\|define-struct\\|def-stx-rules\\|def-stx\\|with-handlers\\|begin0\\|ƛ" (0 (quote keyword) t))) ;;  Hi-lock: (("define[/\-][a-zA-Z0-9\-\_]+" (0 (quote keyword) t))) ;;  Hi-lock: ((";;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";;$.*" (0 (quote fg-red4) t))) ;;  Hi-lock: (("[a-zA-Z0-9\-\_]+%" (0 (quote class-var) t))) ;;  Hi-lock: (("[( ']>[a-zA-Z0-9\-\_/]+" (0 (quote control-id) t))) 
#|
/   TEXT-TOOLS.SS
/     Tools for text handling
|#

(module  text-tools
         scheme
(provide
     expand-template
     ;; :here
     )

(require "std.ss")

(define (expand-template 
    template 
    key/val-alist 
    #:var-regexp [var-re #rx"<([a-zA-Z0-9_-]+)>"]
    #:return-as  [return-as 'string])

  (def-stx-rules insure-key-is-symbol ()
    [(_ $key/val)   (if (number? (car $key/val))
                        (cons (num->sym (car $key/val)) (cdr $key/val))
                        $key/val)])

  (define (keys-as-symbols key/val-alist)
    ;;| Converts any keys that are numbers to symbols
    (let loop ([lst  key/val-alist] [store '()])
      (if (empty? lst)
          (reverse store)
          (let*([key/val    (car lst)])
            (loop (cdr lst) 
                  (cons  (insure-key-is-symbol key/val) 
                         store))))))
  (def-stx-rules as-list ()
    [(_ $a)   
       (cond  [(string? $a)  (regexp-split #rx"\n" $a)]
              [(list? $a)    template]
              [else          (error 'EXPAND-TEMPLATE 
                       "parameter 'template' must be string or list of strings")])])
  (def-stx-rules pre-var ()
    [(_ $line $i $r)  (substr  $line  $i  (car $r))])
    
  (def-stx-rules post-var ()
    [(_ $line $r)  (substr  $line  (cdr $r))])

  (def-stx-rules var->id ()
    [(_ $var)  (str->sym   (nth  1  (regexp-match var-re $var))) ])

  (let*([key/val-alist      (keys-as-symbols  key/val-alist)])

    (define (expand line range-pairs)
      (let loop ([lst range-pairs]  [i 0]  [store '()])
        (if (empty? lst)
            (apply  str+  (reverse   (cons (substr line i) store) ))
            (let*([r         (car lst)] 
                  [var       (substr  line  (car r)  (cdr r))]
                  [key/val   (assq  (var->id var)  key/val-alist)]
                  [sub       (if key/val  (cdr key/val)  var)])
              (loop  (cdr lst)
                     (cdr r)
                     (cons   (str+  (pre-var line i r)  sub)
                             store))))))
    
    (let loop ([lst  (as-list template)]  [store '()])
      (if (empty? lst)
          (cond 
           [(eq? 'string return-as)
                     (str-join (reverse store) "\n")]
           [(eq? 'list return-as)
                     (reverse store)])
          (let*([line    (car lst)]
                [m       (regexp-match-positions*  var-re line)])
            (if (empty? m)
                (loop  (cdr lst)  (cons line store))
                (loop  (cdr lst)  (cons (expand line m)  store)))))) ))


);| module