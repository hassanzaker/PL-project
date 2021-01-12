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
            ((:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))(token-VARIABLE lexeme))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (VARIABLE NUM BOOL STRING))
(define-empty-tokens b (EOF plus minus mult div semicolon while do end if then else assignment return gt lt eq neq paropen parclose bracopen bracclose comma null ob cb))

(define simple-math-parser
           (parser
            (start cexp)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
             (command ((keyword) (list 'keyword $1)) ((command semicolon keyword) (list 'commandkeyword $1 $3)))
             (keyword ((if_statement) (list 'if_statement $1)) ((assignment_statement) (list 'assignment_statement $1))
                      ((while_statement) (list 'while_statement $1)) ((return_statement) (list 'return_statement $1)))
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
             (listValues ((exp) (list 'exp $1)) ((exp comma listValues) (list 'explistval $1 $3)))
             (listMember ((bracopen exp bracclose) (list 'exp $2))
                   ((ob exp cb listMember) (list 'explistmem $2 $4)))
             )))


#| cexp complete except varval and varlistmem |#
(define (control command) (cond
             [(equal? (car command) 'keyword) (control (cadr command))]
             [(equal? (car command) 'return_statement) (control (cadr command))] 
             [(equal? (car command) 'negetive) (* -1 (control (cadr command)))]
             [(equal? (car command) 'exp) (control (cadr command))]
             [(equal? (car command) 'null) null]
             [(equal? (car command) 'varval) void]
             [(equal? (car command) 'boolean) (cadr command)]
             [(equal? (car command) 'string) (cadr command)]
             [(equal? (car command) 'list) (control (cadr command))]
             [(equal? (car command) 'varlistmem) (void)]
             [(equal? (car command) 'number) (cadr command)]
             [else command]
                           ))


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "'asdasfa'")))
(let ((parser-res (simple-math-parser my-lexer))) (control parser-res))

