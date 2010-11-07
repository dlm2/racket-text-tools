#!/usr/bin/mzscheme
#lang scheme
;;  Hi-lock: ((";*;|.*" (0 (quote fg-gray35) t))) ;;  Hi-lock: ((";*;-.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";*;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("^/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("let\\*-values\\|let-values\\|define-values\\|when\\|case-lambda\\|make-struct-type\\|define-struct\\|def-stx-rules\\|def-stx\\|with-handlers\\|begin0\\|ƛ" (0 (quote keyword) t))) ;;  Hi-lock: (("define[/\-][a-zA-Z0-9\-\_]+" (0 (quote keyword) t))) ;;  Hi-lock: ((";;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";;$.*" (0 (quote fg-red4) t))) ;;  Hi-lock: (("[a-zA-Z0-9\-\_]+%" (0 (quote class-var) t))) ;;  Hi-lock: (("[( ']>[a-zA-Z0-9\-\_/]+" (0 (quote control-id) t))) 
#|
/   USAGE.TEMPLATE-EXPAND.SCM
/     Usage examples for EXPAND-TEMPLATE
|#

(require "std.ss")
(require "text-tools.ss")

(define (example-1)
  ;;| Use defaults for #:VAR-REGEXP and #:RETURN-AS parmeters.
  (define memo-format-1
    (str-join/nl 
     '("FROM     : <from>"
       "TO       : <to>"
       "<body>")))
  
  (expand-template
     memo-format-1
     `([from . "Mr. Black"] 
       [to   . "Bob"] 
       [body . "Meeting at 10:20am."])))

(define (example-2)
  ;;| Use #:VAR-REGEXP parameter to change regexp used to identify
  ;;|  template variables.
  (define memo-format-2
    (str-join/nl 
     '("FROM     : $from"
       "TO       : $to"
       "$body")))
  
  (expand-template 
     memo-format-2
     '([from . "Mr. Black"] 
       [to   . "Bob"] 
       [body . "Meeting at 10:20am."])
     #:var-regexp #rx"\\$([a-zA-Z0-9_-]+)"))

(define (example-3)
  ;;| Use #:RETURN-AS parmeter to change the return type to a
  ;;| list for post expansion processing.
  (define memo-format-1
    (str-join/nl 
     '("FROM     : <from>"
       "TO       : <to>"
       "<body>")))
  
  (expand-template
     memo-format-1
     `([from . "Mr. Black"] 
       [to   . "Bob"] 
       [body . "Meeting at 10:20am."])
     #:return-as 'list))


(printf "~a~n~n"  (example-1))

(printf "~a~n~n"  (example-2))

(let loop ([lst   (example-3)])
  (when (not-empty? lst)
     (let*([s    (car lst)])
       (printf "#### ~a~n" s)
       (loop (cdr lst)))))

