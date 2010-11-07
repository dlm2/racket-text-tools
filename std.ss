;;  Hi-lock: ((";*;|.*" (0 (quote fg-gray35) t))) ;;  Hi-lock: ((";*;-.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";*;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("^/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("let\\*-values\\|let-values\\|define-values\\|when\\|case-lambda\\|make-struct-type\\|define-struct\\|def-stx-rules\\|def-stx\\|with-handlers\\|begin0\\|ƛ" (0 (quote keyword) t))) ;;  Hi-lock: (("define[/\-][a-zA-Z0-9\-\_]+" (0 (quote keyword) t))) ;;  Hi-lock: ((";;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";;$.*" (0 (quote fg-red4) t))) ;;  Hi-lock: (("[a-zA-Z0-9\-\_]+%" (0 (quote class-var) t))) ;;  Hi-lock: (("[( ']>[a-zA-Z0-9\-\_/]+" (0 (quote control-id) t))) 
#|
/   STD.SS
/     Some of my standard tools (abbreviations and such).
|#

(module  std
         scheme
(provide
     ;;| :abbreviations
     def-stx-rules
     not-empty?
     nth
     str->sym
     num->str
     num->sym
     str+
     str+*
     substr
     str-join
     str-join/nl
     ;; :here
     )

(define-syntax def-stx-rules
  (syntax-rules ()
    [(_ name literials body ...)  
             (define-syntax name
               (syntax-rules literials
                 body ...
                 ))]))

(define-syntax not-empty?
  (syntax-rules ()
    [(_ a)   (not (empty? a))]))

(define-syntax nth
  (syntax-rules ()
    ([_ i lst]    [list-ref lst i])))

(define str->sym     string->symbol)
(define num->str     number->string)
  (def-stx-rules num->sym ()
    [(_ $a)  (str->sym (num->str $a))])
(define str+         string-append)
(define str+*        string-append*)
(define substr       substring)
(define str-join     string-join)
(define-syntax str-join/nl
  (syntax-rules ()
    [(_ lst)      (string-join lst "\n")]))


);| module