_start_of_buffer: dw 0
_buffer_offset: dw 0

;Get a single char
;Returns:
;   AH - Scan code
;   AL - ASCII char
cscan:
    xor ax, ax
    int 0x16
    ret

;Scan until an enter
sscan:
    mov si, word [_start_of_buffer]
    add si, word [_buffer_offset]
    push si                             ;Save the current top of the buffer, this will point to the new string

    ;Get char
    ;End if it's an enter
    ;   - terminate
    ;   - increment buffer_offset for safety
    ;   - return
    ;Save char to buffer
    ;Increment _buffer_offset

.done:
    pop si
    ret

;Prompt with a string, then scan until an enter
prompt:
    ret
