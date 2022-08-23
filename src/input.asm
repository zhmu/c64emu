;
; input.asm
;
; this will handle keyboard input
;
.model          small
.386
.data
in_old_vector   dd      0                       ; old keyboard irq
in_keydown      db      128 dup (0)             ; key down data
in_keycode      db      0

.code
public          in_init,in_done,in_keydown,in_keycode,in_waitrelease

;
; in_kbdirq
;
; this is the new keyboard irq
;
in_kbdirq:      push    ds si ax bx
                push    @data
                pop     ds                      ; ds = data

                sti

                xor     ah,ah
                in      al,60h                  ; al = key code
                mov     [in_keycode],al

                mov     bl,al
                sub     bl,7fh
                jnc     in_kbdirq_1

                inc     ah                      ; it's a make code
                jmp     short in_kbdirq_2

in_kbdirq_1:    sub     al,80h                  ; nuke off high bit

in_kbdirq_2:
                movzx   si,al                   ; si = (word)keycode
                add     si,offset in_keydown
                mov     ds:[si],ah              ; keydown[al] = ah

                mov     al,20h
                out     20h,al                  ; signal eoi
                pop     bx ax si ds
                iret

;
; in_init
;
; this will initialize the input stuff
;
in_init         proc
                mov     ax,3509h                ; dos: get irq vector 1
                int     21h
                mov     word ptr [in_old_vector],bx
                mov     word ptr [in_old_vector]+2,es

                sub     ah,10h                  ; dos: set irq vector 1
                push    ds cs
                pop     ds                      ; ds = cs
                mov     dx,offset in_kbdirq
                int     21h
                pop     ds

                ret
in_init         endp

;
; in_done
;
; this will deinitialize the input stuff
;
in_done         proc
                cmp     [in_old_vector],0       ; vector hooked?
                jz      in_done_1               ; nope, skip restore

                mov     ax,2509h                ; dos: set irq vector 1
                push    ds
                lds     dx,in_old_vector        ; old vector
                int     21h
                pop     ds

in_done_1:      ret
in_done         endp

;
; in_waitrelease
;
; this will for for the release of the key in [bl].
;
in_waitrelease  proc
                push    es
                push    40h
                pop     es

                xor     bh,bh
                add     bx,offset in_keydown

                mov     eax,es:[6ch]
                add     eax,3

in_waitrelease1:mov     edx,es:[6ch]
                cmp     edx,eax
                jge     in_waitrelease2

                cmp     byte ptr [bx],0
                jnz     in_waitrelease1

in_waitrelease2:mov     byte ptr [bx],0

                pop     es
                ret
in_waitrelease  endp

end
