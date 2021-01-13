#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define (string->boolean x) (if (equal? x "true") #t (if (equal? x "false") #f (error))))

#|string has a problem|#
(define simple-math-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ((:or "true" "false")(token-BOOL (string->boolean lexeme)))
            ((:: "'" (:+ any-char) "'")(token-STRING lexeme))
            ("print" (token-print))
            ("switch" (token-switch))
            ("case" (token-case))
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
            ((:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))(token-VARIABLE lexeme))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (VARIABLE NUM BOOL STRING))
(define-empty-tokens b (EOF print switch case plus minus mult div semicolon while do end if then else assignment return gt lt eq neq paropen parclose bracopen bracclose comma null ob cb))

(define simple-math-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
             (command ((keyword) (list 'keyword $1)) ((command semicolon keyword) (list 'commandkeyword $1 $3)))
             (keyword ((if_statement) (list 'if_statement $1)) ((assignment_statement) (list 'assignment_statement $1))
                      ((while_statement) (list 'while_statement $1)) ((return_statement) (list 'return_statement $1))
                      ((print paropen exp parclose) (list 'print $3)))
             (while_statement ((while exp do command end) (list 'while $2 $4)))
             (if_statement ((if exp then command else command end) (list 'if $2 $4 $6)))
             (assignment_statement ((VARIABLE assignment exp) (list 'assignment $1 $3)))
             (return_statement ((return exp) (list 'return $2)))
             (exp ((aexp) (list 'aexp $1)) ((aexp gt aexp) (list 'gt $1 $3)) ((aexp lt aexp) (list 'lt $1 $3))
                  ((aexp eq aexp) (list 'eq $1 $3)) ((aexp neq aexp) (list 'neq $1 $3)))
             (aexp ((bexp) (list 'bexp $1)) ((bexp minus aexp) (list 'sub $1 $3))
                   ((bexp plus aexp) (list 'add $1 $3)))
             (bexp ((cexp) (list 'cexp $1)) ((cexp mult bexp) (list 'mult $1 $3))
                   ((cexp div bexp) (list 'div $1 $3)))
             (cexp ((minus cexp) (list 'negetive $2))
                   ((paropen exp parclose) (list 'exp $2))
                   ((NUM) (list 'number $1)) ((null) (list 'null))
                   ((VARIABLE) (list 'varval $1)) ((BOOL) (list 'boolean $1))
                   ((STRING) (list 'string $1)) ((list_statement) (list 'list $1))
                   ((VARIABLE listMember) (list 'varlistmem $1 $2)))
             (list_statement ((bracopen listValues bracclose) (list 'listval $2))
                   ((bracopen bracclose) (list 'emptylist)))
             (listValues ((exp) (list 'onememlist $1)) ((exp comma listValues) (list 'explistval $1 $3)))
             (listMember ((bracopen exp bracclose) (list 'exp $2))
                   ((ob exp cb listMember) (list 'explistmem $2 $4)))
             )))


#|(all variable related call void func) |#
;return should end program but dont
;switch case should be added
(define (control command) (cond
             [(equal? (car command) 'keyword) (control (cadr command))]
             [(equal? (car command) 'commandkeyword) (begin (control (cadr command)) (control (caddr command)))]

             [(equal? (car command) 'if_statement) (control (cadr command))]
             [(equal? (car command) 'while_statement) (control (cadr command))]
             [(equal? (car command) 'assignment_statement) (control (cadr command))]
             [(equal? (car command) 'return_statement) (control (cadr command))]
             [(equal? (car command) 'print) (display (control (cadr command)))]
             
             [(equal? (car command) 'while) (while-func (cadr command) (caddr command))]

             [(equal? (car command) 'if) (if-func (cadr command) (caddr command) (cadddr command))]
             
             [(equal? (car command) 'assignment) (void)]

             [(equal? (car command) 'return) (control (cadr command))]

             [(equal? (car command) 'aexp) (control (cadr command))]
             [(equal? (car command) 'gt) (gt (control (cadr command)) (control (caddr command)))]
             [(equal? (car command) 'lt) (lt (control (cadr command)) (control (caddr command)))]
             [(equal? (car command) 'eq) (eq (control (cadr command)) (control (caddr command)))]
             [(equal? (car command) 'neq) (not (eq (control (cadr command)) (control (caddr command))))]
             
             [(equal? (car command) 'bexp) (control (cadr command))]
             [(equal? (car command) 'sub) (do-sub (control (cadr command)) (control (caddr command)))]
             [(equal? (car command) 'add) (do-add (control (cadr command)) (control (caddr command)))]
             
             [(equal? (car command) 'cexp) (control (cadr command))]
             [(equal? (car command) 'mult) (do-mul (control (cadr command)) (control (caddr command)))]
             [(equal? (car command) 'div) (do-div (control (cadr command)) (control (caddr command)))]
             
             [(equal? (car command) 'negetive) (negetive (control (cadr command)))]
             [(equal? (car command) 'exp) (control (cadr command))]
             [(equal? (car command) 'null) null]
             [(equal? (car command) 'varval) (void)]
             [(equal? (car command) 'boolean) (cadr command)]
             [(equal? (car command) 'string) (cadr command)]
             [(equal? (car command) 'list) (control (cadr command))]
             [(equal? (car command) 'varlistmem) (void)]
             [(equal? (car command) 'number) (cadr command)]

             [(equal? (car command) 'listval)  (control (cadr command))]
             [(equal? (car command) 'emptylist) (list)]
             
             [(equal? (car command) 'explistval) (cons (control (cadr command)) (control (caddr command)))]
             [(equal? (car command) 'onememlist) (cons (control (cadr command)) null)]
             
             [(equal? (car command) 'explistmem) (void)]
             
             [else command]
                           ))

(define (while-func exp command)
             (if (control exp) (begin
                                 (control command)
                                 (while-func exp command))
                              null))

(define (if-func exp command1 command2)
             (if (control exp) (control command1) (control command2)))

(define (eq a b) (cond
              [(and (number? a) (number? b)) (= a b)]
              [(and (string? a) (string? b)) (equal? a b)]
              [(and (null? a) (null? b)) #t]
              [(and (boolean? a) (boolean? b)) (equal? a b)]
              [(and (list? a) (list? b)) (equal? b a)]
              [else (error "cant compare this two types")]
                  ))

(define (gt a b) (cond
              [(and (number? a) (number? b)) (> a b)]
              [(and (string? a) (string? b)) (string>? a b)]
              [(and (list? a) (number? b)) (list-gt-number a b)]
              [(and (list? a) (string? b)) (list-gt-string a b)]
              [(and (list? b) (number? a)) (list-lt-number b a)]
              [(and (list? b) (string? a)) (list-lt-string b a)]
              [else (error "cant compare this two types")]
                  ))

(define (list-gt-number lst num)(cond
              [(null? lst) #t]                    
              [else (if (number? (car lst))
                  (if (> (car lst) num) (list-gt-number (cdr lst) num) #f)
                  (error "list has an item that is not number")
                    )]))

(define (list-gt-string lst str)(cond
              [(null? lst) #t]                    
              [else (if (string? (car lst))
                  (if (string>? (car lst) str) (list-gt-string (cdr lst) str) #f)
                  (error "list has an item that is not string")
                    )]))

(define (lt a b) (cond
              [(and (number? a) (number? b)) (< a b)]
              [(and (string? a) (string? b)) (string<? a b)]
              [(and (list? a) (number? b)) (list-lt-number a b)]
              [(and (list? a) (string? b)) (list-lt-string a b)]
              [(and (list? b) (number? a)) (list-gt-number b a)]
              [(and (list? b) (string? a)) (list-gt-string b a)]
              [else (error "cant compare this two types")]
                  ))

(define (list-lt-number lst num)(cond
              [(null? lst) #t]                    
              [else (if (number? (car lst))
                  (if (< (car lst) num) (list-lt-number (cdr lst) num) #f)
                  (error "list has an item that is not number")
                    )]))

(define (list-lt-string lst str)(cond
              [(null? lst) #t]                    
              [else (if (string? (car lst))
                  (if (string<? (car lst) str) (list-lt-string (cdr lst) str) #f)
                  (error "list has an item that is not string")
                    )]))



(define (negetive item) (cond
             [(number? item) (* -1 item)]
             [(boolean? item) (not item)]
             [(list? item) (negetive-list item)]
             [else (error "can't negetive this format")]
             
                          ))
(define (negetive-list list)
             (if (null? (cdr list)) (cons (negetive (car list)) null)
                 (cons (negetive (car list)) (negetive-list (cdr list)))))

(define (do-add a b) (cond
              [(and (number? a) (number? b)) (+ a b)]
              [(and (boolean? a) (boolean? b)) (or a b)]
              [(and (boolean? a) (list? b)) (map (lambda (x) (or a x)) b)]
              [(and (boolean? b) (list? a)) (map (lambda (x) (or b x)) a)]
              [(and (number? a) (list? b)) (map (lambda (x) (+ a x)) b)]
              [(and (number? b) (list? a)) (map (lambda (x) (+ b x)) a)]
              [(and (string? a) (string? b)) (string-append a b)]
              [(and (string? a) (list? b)) (map (lambda (x) (string-append a x)) b)]
              [(and (string? b) (list? a)) (map (lambda (x) (string-append x b)) a)]
              [(and (list? a) (list? b)) (append a b)]
              [else (error "cant add this two types")]
              ))

(define (do-sub a b) (cond
              [(and (number? a) (number? b)) (- a b)]
              [(and (number? a) (list? b)) (map (lambda (x) (- a x)) b)]
              [(and (number? b) (list? a)) (map (lambda (x) (- x b)) a)]
              [else (error "cant sub this two types")]
              ))

(define (do-mul a b) (cond
              [(and (number? a) (number? b)) (* a b)]
              [(and (boolean? a) (boolean? b)) (and a b)]
              [(and (boolean? a) (list? b)) (map (lambda (x) (and a x)) b)]
              [(and (boolean? b) (list? a)) (map (lambda (x) (and b x)) a)]
              [(and (number? a) (list? b)) (map (lambda (x) (* a x)) b)]
              [(and (number? b) (list? a)) (map (lambda (x) (* b x)) a)]
              [else (error "cant mult this two types")]
              ))

(define (do-div a b) (cond
              [(and (number? a) (number? b)) (/ a b)]
              [(and (number? a) (list? b)) (map (lambda (x) (/ a x)) b)]
              [(and (number? b) (list? a)) (map (lambda (x) (/ x b)) a)]
              [else (error "cant div this two types")]
              ))


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "print((false + [false, true] + true) * false)")))
;(let ((parser-res (simple-math-parser my-lexer)))  parser-res)
(let ((parser-res (simple-math-parser my-lexer))) (control parser-res))
