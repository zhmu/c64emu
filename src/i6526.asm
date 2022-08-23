;
; i6526.asm
;
; this is the 6526 CIA emulation code
;
.model          small
.386
.data
extrn           v_bankbase:word
extrn           in_keycode:byte

v_banktab       dw      0c000h,08000h,04000h,0h

; i_keytable is the keyboard table, the format is:
;
; [ibm scan code], [c64 matrix row], [c64 matrix col], [zero]
;
i_keytable      db      80, 0feh, 07fh, 0h              ; down
                db      63, 0feh, 0bfh, 0h              ; f5
                db      61, 0feh, 00dh, 0h              ; f3
                db      59, 0feh, 0efh, 0h              ; f1
                db      65, 0feh, 0f7h, 0h              ; f7
                db      77, 0feh, 0fbh, 0h              ; right
                db      28, 0feh, 0fdh, 0h              ; enter
                db      14, 0feh, 0feh, 0h              ; backspace

                db      42, 0fdh, 07fh, 0h              ; left shift
                db      18, 0fdh, 0bfh, 0h              ; e
                db      31, 0fdh, 00dh, 0h              ; s
                db      44, 0fdh, 0efh, 0h              ; z
                db       5, 0fdh, 0f7h, 0h              ; 4
                db      30, 0fdh, 0fbh, 0h              ; a
                db      17, 0fdh, 0fdh, 0h              ; w
                db       4, 0fdh, 0feh, 0h              ; 3

                db      45, 0fbh, 07fh, 0h              ; x
                db      20, 0fbh, 0bfh, 0h              ; t
                db      33, 0fbh, 00dh, 0h              ; f
                db      46, 0fbh, 0efh, 0h              ; c
                db       7, 0fbh, 0f7h, 0h              ; 6
                db      32, 0fbh, 0fbh, 0h              ; d
                db      19, 0fbh, 0fdh, 0h              ; r
                db       6, 0fbh, 0feh, 0h              ; 5

                db      47, 0f7h, 07fh, 0h              ; v
                db      22, 0f7h, 0bfh, 0h              ; u
                db      35, 0f7h, 00dh, 0h              ; h
                db      48, 0f7h, 0efh, 0h              ; b
                db       9, 0f7h, 0f7h, 0h              ; 8
                db      34, 0f7h, 0fbh, 0h              ; g
                db      21, 0f7h, 0fdh, 0h              ; y
                db       8, 0f7h, 0feh, 0h              ; 7

                db      49, 0efh, 07fh, 0h              ; n
                db      24, 0efh, 0bfh, 0h              ; o
                db      37, 0efh, 00dh, 0h              ; k
                db      50, 0efh, 0efh, 0h              ; m
                db      24, 0efh, 0f7h, 0h              ; o
                db      36, 0efh, 0fbh, 0h              ; j
                db      23, 0efh, 0fdh, 0h              ; i
                db      10, 0efh, 0feh, 0h              ; 9

                db      51, 0dfh, 07fh, 0h              ; comma
                db       3, 0dfh, 0bfh, 1h              ; @ (shift-2)
                db      40, 0dfh, 0dfh, 0h              ; : (mapped to ')
                db      52, 0dfh, 0efh, 0h              ; .
                db      12, 0dfh, 0f7h, 0h              ; -
                db      38, 0dfh, 0fbh, 0h              ; l
                db      25, 0dfh, 0fdh, 0h              ; p
                db      13, 0dfh, 0feh, 0h              ; +

                db      53, 0dfh, 07fh, 0h              ; /
                db       7, 0dfh, 0bfh, 1h              ; ^ (shift-6)
                db      13, 0dfh, 00dh, 0h              ; =
                db      54, 0dfh, 0efh, 0h              ; right shift
                db      71, 0dfh, 0f7h, 0h              ; home
                db      39, 0dfh, 0fbh, 0h              ; ;
                db      27, 0dfh, 0fdh, 0h              ; * (mapped to ])
                db      26, 0dfh, 0feh, 0h              ; pound (mapped to [)

                db      15, 07fh, 07fh, 0h              ; stop (mapped to tab)
                db      16, 07fh, 0bfh, 0h              ; q
                db      56, 07fh, 00dh, 0h              ; c= (mapped to alt)
                db      57, 07fh, 0efh, 0h              ; space
                db       3, 07fh, 0f7h, 0h              ; 2
                db      29, 07fh, 0fbh, 0h              ; cntrl
                db    0ffh, 07fh, 0fdh, 0h              ; escape (not mapped)
                db       2, 07fh, 0fdh, 0h              ; 1

                dd      0

i_matrix        dd      0
                dd      0

.code
public          i_init,i_done,i_poll,i_memchange

;
; i_init
;
; this will initialize the 6526 emulation
;
i_init          proc
                ret
i_init          endp

;
; i_done
;
; this will deinitialize the 6526 emulation
;
i_done          proc
                ret
i_done          endp

;
; i_matrix2ind
;
; this will convert keyboard matrix [al] to an array index in [al].
;
i_matrix2ind    proc
                cmp     al,0feh
                jne     i_matrix2ind1

                xor     al,ah
                ret

i_matrix2ind1:  cmp     al,0fdh
                jne     i_matrix2ind2

                mov     al,1
                ret

i_matrix2ind2:  cmp     al,0fbh
                jne     i_matrix2ind3

                mov     al,2
                ret

i_matrix2ind3:  cmp     al,0f7h
                jne     i_matrix2ind4

                mov     al,3
                ret

i_matrix2ind4:  cmp     al,0efh
                jne     i_matrix2ind5

                mov     al,4
                ret

i_matrix2ind5:  cmp     al,0dfh
                jne     i_matrix2ind6

                mov     al,5
                ret

i_matrix2ind6:  cmp     al,0bfh
                jne     i_matrix2ind7

                mov     al,6
                ret

i_matrix2ind7:  mov     al,7
                ret
i_matrix2ind    endp

;
; i_poll
;
; this will poll the io subsystem
;
i_poll          proc
                push    gs 40h
                pop     gs

                mov     al,gs:[6fh]
                mov     fs:[0dd00h],al
                mov     fs:[0d012h],al

                pop     gs

                cmp     [in_keycode],0
                jz      i_poll1

                mov     dword ptr ds:[offset i_matrix],0ffffffffh
                mov     dword ptr ds:[offset i_matrix+4],0ffffffffh

;                movzx   bx,[in_keycode]

                mov     si,offset i_keytable
                mov     cx,64
i_poll2:        lodsd
                cmp     bl,al
                je      i_poll3

                loop    i_poll2

                ret

i_poll3:        shr     eax,8                   ; now, ah = row, al = col
                mov     bl,al
                shr     ax,8
                xchg    al,bl

                call    i_matrix2ind
                add     ax,offset i_matrix
                mov     di,ax

                mov     ds:[di],bl

                mov     [in_keycode],0
i_poll1:        ret
i_poll          endp

;
; i_memchange
;
; this will handle memory changes of the CIA. fs:[si] will hold the changed
; memory
;
i_memchange     proc
                cmp     si,0dd00h
                jne     i_memchange1

                mov     al,fs:[0dd02h]
                and     al,3
                cmp     al,3
                jz      i_memchange1

                mov     al,fs:[si]
                and     al,3                    ; al = bottom 2 bits

                movzx   bx,al                   ; bx = (word)al
                shl     bx,1
                add     bx,offset v_banktab
                mov     bx,[bx]
                mov     [v_bankbase],bx         ; set new video bank base

i_memchange1:   cmp     si,0dc00h
                jne     i_memchange2

                movzx   ax,byte ptr fs:[si]     ; al = (byte)[0dc00h]
                call    i_matrix2ind
                mov     di,ax
                add     di,offset i_matrix

                mov     al,ds:[di]              ; al = i_matrix[[si]]
                mov     fs:[0dc01h],al

i_memchange2:   ret
i_memchange     endp

end
