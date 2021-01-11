#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(define (string->boolean x) (if (equal? x "true") #t (if (equal? x "false") #f (error))))
(define simple-math-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ((:or "true" "false")(token-BOOL (string->boolean lexeme)))
            ((:: "'" (:+ any-char) "'")(token-STRING lexeme))
            ("+" (token-plus))
            ("-" (token-minus))
            ("*" (token-mult))
            ("/" (token-div))
            (";" (token-semicolon))
            ("while" (token-while))
            ("do" (token-do))
            ("end" (token-end))
            ("if" (token-if))
            ("then" (token-then))
            ("else" (token-else))
            ("=" (token-assignment))
            ("return" (token-return))
            (">" (token-gt))
            ("<" (token-lt))
            ("==" (token-eq))
            ("!=" (token-neq))
            ("(" (token-paropen))
            (")" (token-parclose))
            ("[" (token-bracopen))
            ("]" (token-bracclose))
            ("," (token-comma))
            ("null" (token-null))
            ("ob" (token-ob))
            ("cb" (token-cb))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (NUM BOOL STRING))
(define-empty-tokens b (EOF plus minus mult div semicolon while do end if then else assignment return gt lt eq neq paropen parclose bracopen bracclose comma null ob cb))

(define simple-math-parser
           (parser
            (start exp)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
           (exp ((exp plus NUM) (list 'plusnumbers $1 $3)) ((NUM) (list 'anumber $1)))
             )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "false 65848 == 45 'adssadasdad343543rsdfcf' =")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)