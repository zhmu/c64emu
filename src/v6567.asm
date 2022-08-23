;
; v6567.asm
;
; this will handle the emulation of the 6567 video IC (also knows as VIC-II)
;
.model          small
.386
.data
v_base          dw      400h
v_charptr       dw      1000h
v_rastereg      db      0h
v_bankbase      dw      0h

v_oldmode       db      0
v_oldnofrows    db      0

v_bitab         db      80h,40h,20h,10h,8h,4h,2h,1h

extrn           p_reg_pc:word

.code
public          v_init,v_done,v_memchange,v_poll,v_base,v_bankbase,v_drawall

;
; v_init
;
; this will initialize the video stuff
;
v_init          proc
                mov     ax,1130h                ; video: get font information
                xor     bh,bh
                int     10h
                mov     [v_oldnofrows],dl

                mov     ah,0fh                  ; video: get current mode
                int     10h
                mov     [v_oldmode],al

                mov     ax,0013h                ; video: set mode 13h
                int     10h
                ret
v_init          endp

;
; v_done
;
; this will deinitialize the video stuff
;
v_done          proc
                movzx   ax,[v_oldmode]          ; video: set mode [v_oldmode
                int     10h

                cmp     [v_oldnofrows],18h      ; in 25-line mode?
                je      v_done_1                ; yeah, visit v_done_1

                ; set the vga extended 50-line mode
                mov     ax,01112h               ; video: set 8x8 chars
                xor     bl,bl
                int     10h

v_done_1:       ret
v_done          endp

;
; v_memchange
;
; this will be called whenever a video memory location changes. the changed
; memory is in fs:[si].
;
v_memchange     proc
                cmp     si,0d018h
                jne     v_memchange2

                mov     al,fs:[si]
                mov     ah,al

                and     al,0fh                  ; al = low nibble of fs:[si]
                shr     ah,4                    ; ah = hi nibble of fs:[si]

                movzx   bx,ah                   ; bx = (word)ah
                shl     bx,10
                mov     [v_base],bx             ; set new video base

                movzx   bx,al                   ; bx = (word)al
                shl     bx,10
                mov     [v_charptr],bx          ; set new char memory
                ret

v_memchange2:   mov     ax,[v_base]
                cmp     si,ax
                jl      v_memchange1

                add     ax,(40*25)
                cmp     si,ax
                jg      v_memchange1            ; yeah, visit v_memchange1

                shl     ax,3
                mov     di,ax

                mov     ax,si
                mov     bx,[v_base]
                sub     ax,bx                   ; ax = 40 * row + col

                mov     bx,40
                xor     dx,dx

v_memchange5:   cmp     ax,bx
                jl      v_memchange6

                sub     ax,bx
                inc     dx
                jmp     v_memchange5

v_memchange6:   mov     cx,ax

                shl     cx,3                    ; cx = cx * 8
                shl     dx,3                    ; dx = dx * 8
                mov     al,fs:[si]
                call    v_putchar

v_memchange4:   ;mov     ax,[p_reg_pc]
;                call    v_drawall

v_memchange1:   ret
v_memchange     endp

;
; v_poll
;
; this will be called every cycle and gives the 6567 time to do it's own
; stuff
;
v_poll          proc
                ret
v_poll          endp

;
; v_putchar
;
; this will put char [al] at [cx,dx]
;
v_putchar       proc
                ; get pointer
                xor     ah,ah
                shl     ax,3                    ; ax = (word)al * 8

                mov     si,[v_charptr]
                add     si,[v_bankbase]
                add     si,ax                   ; si = v_charptr[al]

                push    0a000h
                pop     es

                ; calculate position in video memory
                mov     ax,320
                mul     dx
                add     ax,cx
                mov     di,ax

;                shl     bx,8
;                shl     dx,6
;                add     bx,dx
;                add     bx,cx
;                mov     di,bx                   ; di = 320 * dx + cx

                mov     dl,8

v_putchar_1:    mov     cx,8
                mov     bx,offset v_bitab

v_putchar_2:    mov     al,byte ptr fs:[si]

                mov     ah,[bx]                 ; ah = v_bitab[cx]
                inc     bx

                and     al,ah
                jz      v_putchar_3

                mov     al,0fh                  ; COLOUR!!!!

v_putchar_3:    stosb

                dec     cl
                jnz     v_putchar_2

                ; next row
                add     di,(320-8)

                inc     si

                ; had 'em all?
                dec     dl                      ; next row
                jnz     v_putchar_1             ; no, visit v_putchar_1

                ret
v_putchar       endp

v_drawall       proc
                ret

                mov     si,[v_base]

                xor     dx,dx

v_poll_1:       xor     cx,cx

v_poll_2:       push    cx dx si
                shl     cx,3                    ; cx = cx * 8
                shl     dx,3                    ; dx = dx * 8
                mov     al,fs:[si]
                call    v_putchar
                pop     si dx cx

                inc     si

                inc     cx                      ; next char
                cmp     cx,40                   ; had 'em all?
                jne     v_poll_2                ; no, visit v_poll_2

                inc     dx                      ; next line
                cmp     dx,25                   ; had 'em all?
                jne     v_poll_1                ; no, visit v_poll_1
                ret
v_drawall       endp

end
