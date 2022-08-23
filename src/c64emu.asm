;
; c64emu.asm
;
; this is the main c64 emulator code
;
.model          small
.stack          1024
.386
.data
extrn           d_disasm_base:word

extrn           p_reg_pc:word

.code
extrn           p_init:proc
extrn           p_done:proc
extrn           p_run:proc
extrn           p_showpc:proc
extrn           p_showint:proc

extrn           in_init:proc
extrn           in_done:proc
extrn           in_keydown

extrn           v_init:proc
extrn           v_done:proc
extrn           v_memchange:proc
extrn           v_poll:proc
extrn           v_drawall:proc

extrn           i_init:proc
extrn           i_done:proc
extrn           i_poll:proc

extrn           p_update6510cnt:dword
extrn           p_updateviccnt:dword
extrn           p_updatevic:dword
extrn           p_update6510:dword
extrn           p_irq:byte

extrn           d_init:proc
extrn           d_done:proc
extrn           d_invoke:proc


.startup
                ; first free all unused memory
                mov     bx,ds                   ; bx = data seg
                mov     ax,es                   ; ax = psp
                sub     bx,ax
                mov     ax,sp                   ; stack pointer is at free memory
                shr     ax,4                    ; change to paragraphs
                add     bx,ax                   ; add with data seg minus psp
                mov     ah,4ah                  ; dos: shrink/expand memory block
                int     21h

                ; initialize the microprocessor
                call    p_init

                ; initialize the debugger
                call    d_init

                ; initialize the input
                call    in_init

                ; initialize the video
                call    v_init

                ; initialize the io
                call    i_init

                ; keep running while escape is not pressed
c64_loop:       inc     [p_update6510cnt]
                mov     eax,[p_update6510cnt]

                cmp     eax,[p_update6510]
                jne     c64_loop1

                ; update the 6510
                call    p_run

                ; clear the flag
                mov     [p_update6510cnt],0

c64_loop1:      inc     [p_updateviccnt]
                mov     eax,[p_updateviccnt]

                cmp     eax,[p_updatevic]
                jne     c64_loop2

;                call    v_drawall

                mov     [p_updateviccnt],0

c64_loop2:      ; poll the video stuff
                call    v_poll

                ; poll the io subsystem
                call    i_poll

                cmp     byte ptr in_keydown[41],0	; ~
                jz      c64_loop3

                call    v_done
                mov     ax,[p_reg_pc]
                mov     [d_disasm_base],ax
                call    d_invoke
                call    v_init

c64_loop3:      cmp     byte ptr in_keydown[1],0	; escape
                jnz     die

                jmp     c64_loop

die:            ; deinitialize the debugger
                call    d_done

                ; deinitialize the io
                call    i_done

                ; deinitialize the video
                call    v_done

                ; deinitialize the input
                call    in_done

;                mov     ax,fs:[0c1h]
;                call    p_showint

                ; deinitialize the microprocessor
                call    p_done

                mov     ax,4c00h                ; dos: exit, error code 0
                int     21h

end
