[org 0x100]

;;Our cons structure is a little wastefull, it goes like this:
;   |Addr            |Data            |Flags   |
;Flags are used to set if the Data part contains just data or a pointer
;In this schema an atom will just be a cons with Addr set to 0 (aka NIL)

CONS_SIZE equ 5

;Flags are set as...
;   0 - value
;   1 - pointer
;   2 - boolean value (0 - Nil, 1 - T)
;   3 - pointer to a bare string
;   4 - number

FLAG_DATA equ 0
FLAG_POINTER equ 1
FLAG_BOOL equ 2
FLAG_STR equ 3
FLAG_NUM equ 4

BOOL_NIL equ 0
BOOL_T equ 1

;;Start by getting command line arg, which is just a file name...
startup:
    mov si, _hello
    call sprint

    ;;What I need to do...
    ;; 1 - Figure out start of free memory
    mov di, _end_of_program
    mov word [_free_list_start], 0x1000 ;;;THIS IS FOR DEBUGGING, CHANGE TO DYNAMIC LATER

    ;; 2 - Allocate a list of free elements, starting at free mem
    mov si, _free_cell_msg
	call sprint

    mov di, word [_free_list_start]
    call make_free_list
    call itoa
    call sprint
    call newline

    ;; 3 - Write code to add/remove linked list elements

    ;; 4 - Write parser... this will hurt the most

    call alloc_cons
    mov word [_def_list_start], si

    mov si, _test_exp
    xor di, di
    call parse
    call dump_list
    call print_list
    call newline

    xor ax, ax ;Clear ah
    int 0x21

;;;OLD STUFF THAT DID WORK
;;; but i'm writting a token tree now...
    call eval

    call print_list
    call newline

    mov si, word [_def_list_start]
    call print_defs

    mov si, word [_def_list_start]
    mov di, _test
    call search_defs
    call print_list
    call newline

    xor ax, ax ;Clear ah
    int 0x21

_hello: db "Welcome to LISPish!", 10, 13, 0
_free_cell_msg: db "Free cells: ", 0
_free_list_start: dw 0
_def_list_start: dw 0

;_test_exp: db "(cons 'hello' (cons A (cons B C))", 0
_test_exp: db "(cons (cons A B) C)", 0
_test: db "test", 0

%include "parse.asm"

;Make a new cons element
;Internally, it needs to look like this:
;   |Address vvv|Data |
;   |Address ...|Data |
;
;   DI - start of free list
;
;Returns
;   AX - number of cells in list
;   DI - end of list
make_free_list:
    xor ax, ax
.loop:
    mov si, di              ;Save DI to SI
    add di, CONS_SIZE       ;Increment to next cons cell
    mov word [si], di       ;Set address part to point at next cell
    add si, 2               ;Increment SI to point to address part of cell
    mov word [si], 0x0000   ;Set data part to a big fat 0 (word)
    add si, 1
    mov byte [si], 0x00     ;Set flags to nothing

    inc ax                  ;Count the cell, for tracking purposes

    ;cmp di, 0xFFF0         ;Right now just stop near the end of the segment...
    cmp ax, 100
    jge .done
    jmp .loop

.done:                      ;Terminate the list
    mov word [di], 0x0000
    mov word [di + 2], 0x0000

    add di, CONS_SIZE
    ret

;   SI - cons to append
;   DI - list to append to
append_to_list:
    push di
.loop:
    cmp word [di], 0
    je .append

    mov di, word [di]
    jmp .loop

.append:
    mov word [di], si
    pop di
    ret

;   SI - definitions list to display
print_defs:
.loop:
    cmp word [si + 2], 0    ;Skip empty defs
    je .next

    ;Now actually print some stuff...
    push si
    mov si, word [si + 2]   ;Get to the def name cons

    call print_atom

    push si
    mov si, .sep
    call sprint
    pop si

    mov si, word [si]       ;ADVANCE... to the value cons
    mov si, word [si + 2]   ;Get the data pointer
    call print_list
    call newline
    pop si

    cmp word [si], 0        ;Terminate on Nil pointer
    je .done

.next:
    mov si, word [si]       ;Follow the link
    jmp .loop

.done:
    ret

    .sep: db " = ", 0

;   SI - defs list to search
;   DI - name to search for
search_defs:
.loop:
    cmp word [si + 2], 0
    je .next

    push si
    ;;Here we do the actual string compare!
    mov si, word [si + 2]   ;Get up to the name cons
    mov si, word [si + 2]   ;Get the pointer to the name string
    call str_cmp            ;Do the comparison
    jc .found
    pop si

    cmp word [si], 0
    je .none

.next:
    mov si, word [si]
    jmp .loop

.found:
    pop si                  ;We are back to SI pointing to the entry in the defs list
    mov si, word [si + 2]   ;Follow data pointer to the name cons
    mov si, word [si]       ;Follow one more link to the value cons
    mov si, word [si + 2]   ;... and finally get the cons that the value cons points to
    ret

.none:
    mov si, 0               ;Nil if none found
    ret

;Follow the pointers! Count the list!
;   SI - List to get length of
;
;Returns:
;   AX - Length of the list
list_len:
    xor ax, ax
.loop:
    call dump_cons
    cmp word [si], 0
    je .done

    inc ax
    call car
    jmp .loop

.done:
    ret

dump_cons:
    pusha
    mov ax, si
    call hprint             ;Print address

    mov al, '|'
    call cprint

    mov ax, word [si]
    call hprint             ;Print pointer part

    mov al, ' '
    call cprint

    mov ax, word [si + 2]
    call hprint             ;Print data part

    mov al, ' '
    call cprint

    mov al, byte [si + 4]
    call hprint_byte        ;Print flags

    call newline
    popa
    ret

dump_list:
    pusha

.loop:
    call dump_cons
    call car
    cmp si, 0x1000
    je .done
    cmp si, 0
    jne .loop

.done:
    popa
    ret

print_list:
    pusha
.loop:
    call print_atom

    mov al, '|'
    call cprint

    mov si, word [si]
    cmp si, 0
    jne .loop

    popa
    ret

print_atom:
    pusha
    mov al, byte [si + 4]       ;Grab flags

    ;Time to see what kind of data we have
    cmp al, FLAG_BOOL
    je .bool

    cmp al, FLAG_STR
    je .str

    cmp al, FLAG_NUM
    je .num

    cmp al, FLAG_POINTER
    je .pointer

    jmp .done

.bool:
    cmp word [si + 2], BOOL_T
    je .t

.nil:
    mov si, .msg_nil
    jmp .done

.t:
    mov si, .msg_t
    jmp .done

.str:
    mov si, word [si + 2]
    jmp .done

.num:

.pointer:
    ;push si
    ;mov si, .msg_ptr    ;Print formatting message
    ;call sprint
    ;pop si

    ;mov ax, word [si + 2]
    ;call hprint

    mov al, '{'
    call cprint

    mov si, word [si + 2]
    call print_list

    mov al, '}'
    call cprint
    jmp .bail

.done:
    call sprint
.bail:
    popa
    ret

    .msg_nil: db "Nil", 0
    .msg_t: db "T", 0
    .msg_ptr: db "CONS@", 0

;   SI - Pointer to list
;
;Returns:
;   SI - Address of next cons cell
car:
    mov si, word [si]
    ret

;   SI - Pointer to list
;
;Returns:
;   SI - Data part of cons cell
cdr:
    mov si, word [si + 2]
    ret

;Get the next free cell off the free list, remove it and return it
;Returns:
;   SI - pointer to your new free cons cell
alloc_cons:
    push di
    push ax
    mov si, word [_free_list_start]     ;Start at free list
    mov di, si                          ;Save for later use
    mov si, word [si]                   ;Skip first cell, we need this for record keeping

    ;Now we need to slice it out of the list
    cmp si, 0                           ;Is this the end of a list?
    je .done                            ;If so then don't slice, just return

    mov ax, word [si]                   ;Get the address part of the free cell
    mov word [di], ax                   ;Set the first cell in free list to point to the next free cell
.done:
    mov word [si], 0                    ;Clear the address part
    mov word [si + 2], 0                ;Clear the data part
    mov byte [si + 4], 0                ;Clear flags part

    pop ax
    pop di
    ret                                 ;Returns with SI pointing to clean cons cell

;   AX - number of cells to dump
dump_free_list:
    mov si, word [_free_list_start]
    xor cx, cx
.loop:
    call dump_cons

    inc cx
    add si, CONS_SIZE

    cmp cx, ax
    jl .loop
    ret

;Slice item out of linked list
;   SI - Cons cell to slice
remove_from_list:
    ret

;Return a cons cell to the free list
;   SI - Cell to add back to list
free_cons:
    ret

;;;Helper functions

_dos_sprint:
    pusha
    mov dx, si
	mov ah, 0x09
	int 0x21
    popa
    ret

sprint:
    pusha
.loop:
    mov al, byte [si]
    cmp al, 0
    je .done

    mov ah, 0x0E
	mov bh, 0
	int 0x10

    inc si
    jmp .loop

.done:
    popa
    ret

cprint:
	pusha
	mov ah, 0x0E
	mov bh, 0
	int 0x10
	popa
	ret

newline:
	pusha
	mov dl, 10
	mov ah, 0x02
	int 0x21
	mov dl, 13
	mov ah, 0x02
	int 0x21
	popa
	ret

;Int to dec string
;	AX - integer to convert
;	SI - converted string, 0 terminated
itoa:
    pusha
    mov cx, 0
    mov bx, 10
    mov di, .t

.push:
    mov dx, 0
    div bx
    inc cx
    push dx
    test ax, ax
    jnz .push

.pop:
    pop dx
    add dl, '0'
    mov [di], dl
	inc di
    dec cx
    jnz .pop

    mov byte [di], 0
    popa
    mov si, .t
	ret

    .t times 8 db 0

;Int to hex string
;	AL - integer to convert
;	SI - converted string
htoa:
	pusha

   	push ax
	shr al, 4
   	cmp al, 10
	sbb al, 69h
   	das
 
	mov byte [.temp], al

   	pop ax
   	ror al, 4
   	shr al, 4
   	cmp al, 10
   	sbb al, 69h
   	das

   	mov byte [.temp + 1], al
   	popa

   	mov si, .temp

   	ret

   .temp db 0, 0, 0, 0

;Convert (small) dec string to integer
;	SI - string to convert
;	AL - converted string's value
atoi:
	push si
	push dx
	push cx
	xor ax, ax
	mov dl, 10
.loop:
	mov cl, byte [si]
	cmp cl, 0
	je .done
	cmp cl, '0'
	jl .done
	cmp cl, '9'
	jg .done

	mov dl, 10
	mul dl						;;Multiply ax by 10, since we are onto a new number
	sub cl, '0'					;;Convert char to int
	add al, cl					;;Add new digit
	inc si						;;Increment us up and prepare to iterate!
	jmp .loop

.done:
	pop cx
	pop dx
	pop si
	ret


;Dump chumk of memory to screen
;	ES:SI - location to dump
;	AX - number of bytes to display
dump_mem:
	pusha

	mov cx, ax							;Get iterater loaded
	mov ax, 16							;Do 16 byte lines
.loop:
	call dump_mem_line 					;Do one line
	call newline

	;Update iterators, addresses
	sub cx, 16
	add si, 16
	cmp cx, 16
	jge .loop

	popa
	ret

dump_mem_line:
	pusha

	push ax							;Save for later

	mov cx, ax						;Prepare iterator
	mov ax, es
	call hprint
	mov al, ':'
	call cprint
	mov ax, si
	call hprint						;Print address

	mov al, '|'				
	call cprint
.hex_loop:							;Print out hex string of RAM
	mov al, byte [es:si]
	call hprint_byte

    mov al, ' '
    call cprint

	dec cx
	inc si
	cmp cx, 0
	jne .hex_loop

	mov al, '|'
	call cprint

	pop cx
	sub si, cx
.chr_loop:							;Print out char string of RAM
	mov al, byte[es:si]
	call cprint

	inc si
	dec cx
	cmp cx, 0
	jne .chr_loop

	popa
	ret

;Print an integer to the screen in hex (word)
;	AX - integer to print
hprint:
	pusha

	mov bx, ax

	mov al, bh 
	call hprint_byte

	mov al, bl
	call hprint_byte

	popa
	ret

;Print an integer to the screen in hex (byte_)
;	AL - integer to print
hprint_byte:
	pusha

	call htoa
	call sprint

	popa
	ret

;Print regesters to TTY
print_regs:
	pusha

	;Push regs to display to stack
	push fs
	push es
	push di
	push si
	push dx
	push cx
	push bx
	push ax

	xor cx, cx
.loop:					;Iterate over registers on stack
	mov si, .labels		;Fetch register's label
	mov ax, 6
	mul cx
	add si, ax

	call sprint 		;Print register

	inc cx				;Inc for next loop

	pop ax				;Grab register from stack
	call hprint

	cmp cx, 8			;Loop until we print out all registers
	jne .loop

	call newline

	popa
	ret

	.labels:
		db ' AX: ', 0
		db ' BX: ', 0
		db ' CX: ', 0
		db ' DX: ', 0
		db ' SI: ', 0
		db ' DI: ', 0
		db ' ES: ', 0
		db ' FS: ', 0

_end_of_program: