;;The general game plan:
;;  - Break string up into tokens
;;  - Build tree structure of operations
;;  - Recursive walk to execute tree

;;Some macros, to ease my suffering
%macro set_cons_address 2   ;Cons, address part
    mov word [%1], %2
%endmacro

%macro set_cons_data 2      ;Cons, data part
    mov word [%1 + 2], %2
%endmacro

%macro set_cons_flags 2     ;Cons, flags
    mov byte [%1 + 4], %2
%endmacro

%macro make_cons 3          ;Address part, data part, flags
    call alloc_cons         ;Allocate a new cons cell
    set_cons_address si, %1 ;Set address part
    set_cons_data si, %2    ;Set data part
    set_cons_flags si, %3   ;Set flags
%endmacro

;Lets do it right this time -- Turn input string into tree of tokens
;   SI - Input string to parse
;   DI - destination cons, initially should be set as 00
;
;Returns:
;   SI - token tree
parse:
    call sprint
    call newline

    cmp byte [si], ')'
    je .done

    cmp byte [si], 0
    je .done

    cmp byte [si], '('          ;Is this the start of an S-expression
    je .start_list              ;Then we need to start a new list

    ;Parsing rules for other token types will go here...
    jmp .string

    xor si, si                  ;THIS IS FOR ERROR HANDLING, WILL NEED TO BE IMPLEMENTED LATER...
    jmp .done

.string:
    call terminate_token        ;Terminate the token
    pushf
    push si

    mov di, si                  ;Grab the string's location
    make_cons 0, di, FLAG_STR   ;Make a new cons cell that points to the string
    mov di, si                  ;Save over the new cons

    pop si
    popf        ;;;The termination case needs to be smarter... right now I stop on internal closes
                ;;;;    I changed terminate to detect ") " as the end of a token
                ;;;     so instead of ending the routine now, I need to recur
                ;;;     then terminate if I hit a 0
    ;jc .done   ;;; <- just dropping this is actually really close!

    ;Now we need to parse something... agian
    ;At this point SI is the original string passed to parse
    push di                     ;Protect our current cons cell
    call str_advance            ;Advance to next token
    call parse                  ;Pull a parse
    pop di                      ;DI is back to the current cons cell
    set_cons_address di, si     ;Make the current cons point to the address of the next cons
    mov si, di                  ;We want to return the list in SI

    jmp .done

.start_list:
    push si
    make_cons 0, 0, FLAG_POINTER    ;Make node cons to represent new expression
    mov di, si                      ;Save it to DI
    pop si

    ;We know the first part here will be a raw string, so terminate it
    inc si
    call terminate_token
    pushf
    push si

    mov bx, si
    make_cons 0, bx, FLAG_STR   ;Start the new cons, it points to the string
    set_cons_data di, si        ;Set the node cons to point to the string cons
    mov bx, si                  ;Save the cons cell for later

    pop si                      ;SI is back to holding the string (tm)
    popf

    ;If carry is set then the token ended with a ), we are done
    ;If carry is clear then the token ended in a " ", we can keep going
    ;jc .done

    ;Now we need to parse something... agian
    ;this is a little different since for ( we need to return the right node
    ;A rundown:
    ;   SI - current string being parsed
    ;   DI - Root cons for this S-expression
    ;   BX - Cons pointing to current token
    push di                     ;Protect our current cons cell
    push bx
    call str_advance            ;Advance to next token
    call parse                  ;Pull a parse
    pop bx
    pop di                      ;DI is back to the current cons cell
    set_cons_address bx, si     ;Make the current cons point to the address of the next cons
    mov si, di                  ;We want to return the list in SI

.done:
    ret

;I am in pain, lets try the most brute-force way possible
;   No tokens! No tree structure! We are doing assembly language string comprehension, sucka!
;       This should just work (tm) since LISP is structured for simple parsing and execution
;
;   SI - String to evaluate, as bare 0-terminated string...
;
;Returns:
;   SI - pointer to cons cell, or to an atom
;   DI - Points to next part of string to consume
eval:
    call sprint
    call newline

    call advance_past_spaces

    ;Make sure this is an S-expression
    mov al, byte [si]
    cmp al, '('
    jne .atom       ;Otherwise, this is a bare atom

    inc si          ;Get past the opening paren

    ;Next, we need to terimate on the next space
    call terminate_token

    call search_primatives  ;Check the lookup table
    cmp di, 0               ;Throw an error if we have no pointer
    je .err

    call str_advance        ;Load SI up with string folowing the function
    call word [di]          ;Then fire off the primative!
    jmp .done
.atom:                      ;This will need to return a pointer to the atom
    ;Figure out what we actually have here...
    cmp byte [si], "'"              ;This is a string
    je .string

    cmp byte [si], '"'
    je .string

;;ORIGINAL LOGIC FOR ABSTRACT SYMBOLS
;;This needs to be changed to search the defs table

    ;Terminate the token
    call terminate_token
    mov di, si

    call alloc_cons                 ;Allocate an empty cons
    mov word [si + 2], di           ;Set the data part to point to the token
    ;mov byte [si + 4], FLAG_POINTER ;Set flag as pointer

    ;This should actually be flagged as a pointer, but just say these are "strings" for testing
    mov byte [si + 4], FLAG_STR     ;Set the flag as a pointer (make str flag later...)

    jmp .next
.string:
    ;Replace the terminating "/' with a terminating 0
    mov al, byte [si]
    mov ah, 0
    inc si
    call str_replace
    mov di, si

    call alloc_cons                 ;Make a new cons
    mov word [si + 2], di           ;Set the data part to point to this token
    mov byte [si + 4], FLAG_STR     ;Set the flag as a string

    jmp .next
.next:
    ;Advance to next token
    push si
    mov si, di
    call str_advance
    mov di, si
    pop si
    jmp .done
.err:
.done:
    ret

advance_past_spaces:
.loop:
    cmp byte [si], ' '
    jne .done
    inc si
    jmp .loop

.done:
    ret

;Simple, replace " " or ")" in SI with 0
;stc if there is no space, but a ) is encoutnered
;clc if the termination is just on a space
terminate_token:
    pusha

    push si
    push si
    mov ax, ') '
    call str_find_w
    mov di, si
    pop si

    cmp di, 0           ;Bail if no ) found
    je .terminate_space

    mov al, ' '
    call str_find

    cmp si, 0
    je .terminate_close

    cmp si, di          ;Does a space come before a )?
    jl .terminate_space

.terminate_close:
    pop si
    mov al, ' '         ;Terminate on ' '
    mov ah, 0
    call str_replace

    mov al, ')'         ;...then terminate on )
    mov ah, 0
    call str_replace

    popa

    stc                 ;Flag this as the end of an expression

    ret

.terminate_space:
    pop si

    ;Retrieve SI and do a normal termination,replacing ' ' with 0
    mov al, ' '
    mov ah, 0
    call str_replace
    popa

    clc                 ;Flag this as NOT the end!

    ret

;Find char AL in SI
;sets SI to NIL if none found
str_find:
.loop:
    cmp byte [si], al
    je .done

    cmp byte [si], 0
    je .none

    inc si
    jmp .loop
.none:
    mov si, 0
.done:
    ret

;Find the chars in AX in SI
;sets SI to NIL if none found
str_find_w:
.loop:
    cmp word [si], ax
    je .done

    cmp byte [si], 0
    je .none

    inc si
    jmp .loop
.none:
    mov si, 0
.done:
    ret

;   SI - string to find in primative table
;
;Returns:
;   DI - Pointer to primative function
search_primatives:
    push si
    mov al, byte [_primative_table_len]     ;Get length of table
    mov di, _primative_table                ;... and pointer to actual table
    xor cx, cx                              ;Clear CX, CL is going to be out counter
.loop:
    call str_cmp                             ;Check for a string match
    jc .match

    push si                                 ;Increment to next entry
    push ax
    mov si, di                              ;We need to use SI for the arg to str_len
    call str_len
    add di, ax                              ;Add string length to DI
    add di, 3                               ;Increment by the termination byte, plus 2 for the function pointer
    pop ax
    pop si

    inc cl
    cmp cl, al
    jl .loop

.no_match:
    xor di, di
    jmp .done
.match:
    mov si, di                              ;Get the length of the string in the table
    call str_len
    add di, ax                              ;Add that to DI
    add di, 1                               ;Then advance past the terminator
.done:
    pop si
    ret

_primative_table_len: db 7
_primative_table:
    db "atom", 0
    dw _atom

    db "eq", 0
    dw _eq

    db "car", 0
    dw _car

    db "cdr", 0
    dw _cdr

    db "cons", 0
    dw _cons

    db "lambda", 0
    dw _lambda

    db "def", 0
    dw _def

;;Primative functions!
;These all recieve...
;   SI - String to consume
;Returns:
;   SI - Pointer to a cons cell

_atom:
    call eval
    cmp byte [si + 4], FLAG_POINTER     ;THIS IS NOT AN ATOM!
    je .nil
.t:
    mov si, bx
    mov word [si], 0                ;This is an atom, so no pointer
    mov word [si + 2], BOOL_T       ;Set data to true
    mov word [si + 4], FLAG_BOOL    ;Flag as a boolean
    jmp .done
.nil:
    mov si, bx
    mov word [si], 0                ;This is an atom, so no pointer
    mov word [si + 2], BOOL_NIL     ;Set data to false
    mov word [si + 4], FLAG_BOOL    ;Flag as a boolean
.done:
    ret

_eq:
    call eval           ;Get first element

    push si
    call eval           ;Get second element
    pop di

    ;We are going to allocate a new atom and then fill it with a boolean value here
    push si
    call alloc_cons
    mov bx, si
    pop si

    ;SI now contains second arg, DI contains first arg
    ;EQ calls for strict address-level comparison, so lets do that...
    cmp si, di
    je .t

.nil:
    mov si, bx
    mov word [si], 0                ;This is an atom, so no pointer
    mov word [si + 2], BOOL_NIL     ;Set data to false
    mov word [si + 4], FLAG_BOOL    ;Flag as a boolean
    jmp .done

.t:
    mov si, bx
    mov word [si], 0                ;This is an atom, so no pointer
    mov word [si + 2], BOOL_T       ;Set data to true
    mov word [si + 4], FLAG_BOOL    ;Flag as a boolean

.done:
    ret

_car:
    call eval           ;First, get the argument
    ;Later I need to decide if I need to terminate the list or if I need to make a new cons
    ;For now lets just terimante
    mov word [si], 0
    ret

_cdr:
    call eval           ;First, get the argument
    mov si, word [si]   ;Next, take the address part
    ret

_cons:
    ;Cons takes 2 arguments and constructs a list out of them
    call eval       ;Get first argument. This will be the root of the list

    push si
    mov si, di
    call eval       ;Get second argument
    mov di, si      ;Save second arg into di
    pop si
    
    mov word [si], di ;Append second argument to the first

    ret

_def:                       ;;Warmup for lambda
    call terminate_token    ;Grab next token as raw string
    mov bx, si              ;Save over the name
    call str_advance        ;Advance past the name

    push bx
    call eval               ;Get the next argument
    pop bx
    mov ax, si              ;Save the results of eval

    ;AX is now the value of eval
    ;BX is now the string name to define
    ;Next:
    ;   - add a cons to the definitions list
    ;   - set it to point at a list like this...
    ;       (name->[DI] eval->[SI])
    make_cons 0, 0, FLAG_POINTER    ;Get a new cons, it's empty for now

    mov di, word [_def_list_start]  ;Get start of def list
    call append_to_list             ;Add new cons to end of def list

    mov di, si                      ;Save new cons address as destination
    make_cons 0, bx, FLAG_STR       ;Make a new cons for def name
    set_cons_data di, si            ;Set cons on def list to point to new name cons

    push si
    mov di, si                      ;Save address of def name cons
    make_cons 0, ax, FLAG_POINTER   ;Make a new cons for the def value, it will point to ANOTHER cons...
    set_cons_address di, si         ;Make def name cons point to the def value cons
    pop si

    ret

_lambda:    ;;This is the big one, and will require some new tables of PAIN
    ret

str_advance:
    push ax
    call str_len
    add si, ax
    inc si
    pop ax
    ret

;Replace fisrt AL with AH in string SI
str_replace:
    push si
.loop:
    mov bl, byte [si]
    cmp bl, 0           ;Check if we are at the end of the string
    je .done

    cmp bl, al          ;Are we at the specified character?
    je .repl

    inc si
    jmp .loop

.repl:
    mov byte [si], ah   ;Make the replacement

.done:
    pop si
    ret

;Get length of a string
;	SI - string
; Out
;	AX - length of string
str_len:
	push si

	xor ax, ax
.loop:
	cmp byte[si], 0
	je .done

	inc si
	inc ax
	jmp .loop

.done:
	pop si
	ret

;Compare SI and DI
;	Set carry on match
str_cmp:
	pusha
.loop:
	mov al, byte [si]
	mov bl, byte [di]

	cmp al, bl
	jne .bad

	cmp al, 0
	je .done

	inc si
	inc di
	jmp .loop

.bad:
	popa
	clc
	ret

.done:
	popa
	stc
	ret