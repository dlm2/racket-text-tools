#!/usr/bin/mzscheme
#lang scheme
;;  Hi-lock: ((";*;|.*" (0 (quote fg-gray35) t))) ;;  Hi-lock: ((";*;-.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";*;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("^/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: (("let\\*-values\\|let-values\\|define-values\\|when\\|case-lambda\\|make-struct-type\\|define-struct\\|def-stx-rules\\|def-stx\\|with-handlers\\|begin0\\|ƛ" (0 (quote keyword) t))) ;;  Hi-lock: (("define[/\-][a-zA-Z0-9\-\_]+" (0 (quote keyword) t))) ;;  Hi-lock: ((";;/.*" (0 (quote fg-gray1) t))) ;;  Hi-lock: ((";;$.*" (0 (quote fg-red4) t))) ;;  Hi-lock: (("[a-zA-Z0-9\-\_]+%" (0 (quote class-var) t))) ;;  Hi-lock: (("[( ']>[a-zA-Z0-9\-\_/]+" (0 (quote control-id) t))) 
#|
/   USAGE.TEXT-TOOLS.SCM
/     Usage examples for FORMAT-TEXT-TO-WIDTH
|#
;; (require 'lisp-ed-tools)    ;; elisp command

;;(require (lib "std.ss" "dlm"))
;;(require  (except-in  (lib "std.ss" "dlm")   push! nl))
;;(require (lib "time-tools.ss" "dlm"))
;;(require (for-syntax (lib "std.ss" "dlm")))
;;(require (lib "struct-tools.ss" "dlm"))
;;(require scheme/pretty)
;;(define pp pretty-print) (define pd pretty-display)
;;(require (prefix-in d: (lib "http-client-tools.ss"  "dlm")))
;;(require (prefix-in d: (lib "html-tools.ss"         "dlm")))
;;(require (prefix-in d: (lib "xml-tools.ss"          "dlm")))


(require "std.ss")
(require "text-tools.ss")


(define test-text (list

;;--(
"Returns a new mutable string 
whose content is the list  of characters in lst. That is, the length of the string is (length lst), and the"

;;--(
"Returns a new mutable string whose content is the list  of characters

 in lst. That is, the length of the string is (length lst), and the
"
))

(let loop ([lst  test-text])
  (when (not-empty? lst)
      (let*([text        (car lst)]
            [line-lst    (format-text-to-width  text 45)])
        (printf "----------------------------------------~n")
        (for ([s  line-lst])    (printf "~s~n" s))
        (loop (cdr lst))
        )))
