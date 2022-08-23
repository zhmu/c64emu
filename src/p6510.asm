;
; p6510.asm
;
; this is the 6510 microprocessor emulation code
;
.model          small
.386
.data
KERNEL_MEMOFS   equ     0e000h                  ; kernel rom offset
KERNEL_ROMSIZE  equ     8192                    ; kernel rom size

BOOTPTR         equ     0fffch                  ; pointer to boot code
IRQ_PTR         equ     0fffeh                  ; pointer to irq

BASIC_MEMOFS    equ     0a000h                  ; basic rom offset
BASIC_ROMSIZE   equ     8192                    ; basic rom size

CHAR_MEMOFS     equ     01000h                  ; character rom size
CHAR_ROMSIZE    equ     4096                    ; character rom size

STACK_END       equ     0ffh                    ; stack end (+100h in memory)
STACK_START     equ     0h                      ; stack start

STACK_BASE      equ     100h                    ; stack base offset

VIDEO_OFFSET    equ     0d000h                  ; video memory start
VIDEO_END       equ     0d3ffh                  ; video memory end

IO_OFFSET       equ     0dc00h                  ; io memory start
IO_END          equ     0ddffh                  ; io memory end

BREAKPOINT      equ     0h

MEMSIZE         equ     0ffffh                  ; 6510 memory size

p_reg_a         db      (?)                     ; accumulator register
p_reg_x         db      (?)                     ; x register
p_reg_y         db      (?)                     ; y register
p_reg_flags     db      (?)                     ; flags register
p_reg_pc        dw      (?)                     ; program counter
p_reg_sp        db      (?)                     ; stack pointer

FLAG_N          equ     80h                     ; negative flag
FLAG_V          equ     40h                     ; overflow flasg
FLAG_UNUSED     equ     20h                     ; unused flag, must be 1
FLAG_B          equ     10h                     ; break flag
FLAG_D          equ     08h                     ; decimal flag
FLAG_I          equ     04h                     ; interrupt flag
FLAG_Z          equ     02h                     ; zero flag
FLAG_C          equ     01h                     ; carry flag

p_updatevic     dd      0b00h                   ; cycles after a VIC update
p_update6510    dd      1                       ; cycles before a 6510 update

p_update6510cnt dd      0
p_updateviccnt  dd      0

p_irqcount      dd      0
p_irqcountcnt   dd      02000h

p_irq           db      0                       ; non-zero when IRQ arrives

c64_mem         dw      (?)                     ; c64 memory (segment)

err_outofmem    db      "Out of memory$"        ; error messages
err_loaderr     db      "Unable to load file$"

p_kromfile      db      "roms\kernel.rom",0     ; kernel rom file
p_bromfile      db      "roms\basic.rom",0      ; basic rom file
p_cromfile      db      "roms\char.rom",0       ; character rom file

p_calltab       dw      offset p_instr_brk      ; 00 - BRK
                dw      offset p_instr_ora_inx  ; 01 - ORA (Indirect, X)
                dw      offset p_instr_futexp   ; 02 - Future Expansion
                dw      offset p_instr_futexp   ; 03 - Future Expansion
                dw      offset p_instr_futexp   ; 04 - Future Expansion
                dw      offset p_instr_ora_zp   ; 05 - ORA (Zero page)
                dw      offset p_instr_asl_zp   ; 06 - ASL (Zero page)
                dw      offset p_instr_futexp   ; 07 - Future Expansion
                dw      offset p_instr_php      ; 08 - PHP
                dw      offset p_instr_ora_im   ; 09 - ORA (Immediate)
                dw      offset p_instr_asl_ac   ; 0A - ASL (Accumulator)
                dw      offset p_instr_futexp   ; 0B - Future Expansion
                dw      offset p_instr_futexp   ; 0C - Future Expansion
                dw      offset p_instr_ora_ab   ; 0D - ORA (Absolute)
                dw      offset p_instr_asl_ab   ; 0E - ASL (Absolute)
                dw      offset p_instr_futexp   ; 0F - Future Expansion
                dw      offset p_instr_bpl      ; 10 - BPL
                dw      offset p_instr_ora_iny  ; 11 - ORA (Indirect, Y)
                dw      offset p_instr_futexp   ; 12 - Future Expansion
                dw      offset p_instr_futexp   ; 13 - Future Expansion
                dw      offset p_instr_futexp   ; 14 - Future Expansion
                dw      offset p_instr_ora_zpx  ; 15 - ORA (Zero page, X)
                dw      offset p_instr_asl_zpx  ; 16 - ASL (Zero page, X)
                dw      offset p_instr_futexp   ; 17 - Future Expansion
                dw      offset p_instr_clc      ; 18 - CLC
                dw      offset p_instr_ora_aby  ; 19 - ORA (Absolute, Y)
                dw      offset p_instr_futexp   ; 1A - Future Expansion
                dw      offset p_instr_futexp   ; 1B - Future Expansion
                dw      offset p_instr_futexp   ; 1C - Future Expansion
                dw      offset p_instr_ora_abx  ; 1D - ORA (Absolute, X)
                dw      offset p_instr_asl_abx  ; 1E - ASL (Absolute, X)
                dw      offset p_instr_futexp   ; 1F - Future Expansion
                dw      offset p_instr_jsr      ; 20 - JSR
                dw      offset p_instr_and_inx  ; 21 - AND (Indirect, X)
                dw      offset p_instr_futexp   ; 22 - Future Expansion
                dw      offset p_instr_futexp   ; 23 - Future Expansion
                dw      offset p_instr_bit_zp   ; 24 - BIT (Zero page)
                dw      offset p_instr_and_zp   ; 25 - AND (Zero page)
                dw      offset p_instr_rol_zp   ; 26 - ROL (Zero page)
                dw      offset p_instr_futexp   ; 27 - Future Expansion
                dw      offset p_instr_plp      ; 28 - PLP
                dw      offset p_instr_and_im   ; 29 - AND (Immediate)
                dw      offset p_instr_rol_ac   ; 2A - ROL (Accumulator)
                dw      offset p_instr_futexp   ; 2B - Future Expansion
                dw      offset p_instr_bit_ab   ; 2C - BIT (Absolute)
                dw      offset p_instr_and_ab   ; 2D - AND (Absolute)
                dw      offset p_instr_rol_ab   ; 2E - ROL (Absolute)
                dw      offset p_instr_futexp   ; 2F - Future Expansion
                dw      offset p_instr_bmi      ; 30 - BMI
                dw      offset p_instr_and_iny  ; 31 - AND (Indirect, Y)
                dw      offset p_instr_futexp   ; 32 - Future Expansion
                dw      offset p_instr_futexp   ; 33 - Future Expansion
                dw      offset p_instr_futexp   ; 34 - Future Expansion
                dw      offset p_instr_and_zpx  ; 35 - AND (Zero page, X)
                dw      offset p_instr_rol_zpx  ; 36 - ROL (Zero page, X)
                dw      offset p_instr_futexp   ; 37 - Future Expansion
                dw      offset p_instr_sec      ; 38 - SEC
                dw      offset p_instr_and_aby  ; 39 - AND (Absolute, Y)
                dw      offset p_instr_futexp   ; 3A - Future Expansion
                dw      offset p_instr_futexp   ; 3B - Future Expansion
                dw      offset p_instr_futexp   ; 3C - Future Expansion
                dw      offset p_instr_and_abx  ; 3D - AND (Absolute, X)
                dw      offset p_instr_rol_abx  ; 3E - ROL (Absulute, X)
                dw      offset p_instr_futexp   ; 3F - Future Expansion
                dw      offset p_instr_rti      ; 40 - RTI
                dw      offset p_instr_eor_inx  ; 41 - EOR (Indirect, X)
                dw      offset p_instr_futexp   ; 42 - Future Expansion
                dw      offset p_instr_futexp   ; 43 - Future Expansion
                dw      offset p_instr_futexp   ; 44 - Future Expansion
                dw      offset p_instr_eor_zp   ; 45 - EOR (Zero page)
                dw      offset p_instr_lsr_zp   ; 46 - LSR (Zero page)
                dw      offset p_instr_futexp   ; 47 - Future Expansion
                dw      offset p_instr_pha      ; 48 - PHA
                dw      offset p_instr_eor_im   ; 49 - EOR (Immediate)
                dw      offset p_instr_lsr_ac   ; 4A - LSR (Accumulator)
                dw      offset p_instr_futexp   ; 4B - Future Expansion
                dw      offset p_instr_jmp_ab   ; 4C - JMP (Absolute)
                dw      offset p_instr_eor_ab   ; 4D - EOR (Absolute)
                dw      offset p_instr_lsr_ab   ; 4E - LSR (Absolute)
                dw      offset p_instr_futexp   ; 4F - Future Expansion
                dw      offset p_instr_bvc      ; 50 - BVC
                dw      offset p_instr_eor_iny  ; 51 - EOR (Indirect, Y)
                dw      offset p_instr_futexp   ; 52 - Future Expansion
                dw      offset p_instr_futexp   ; 53 - Future Expansion
                dw      offset p_instr_futexp   ; 54 - Future Expansion
                dw      offset p_instr_eor_zpx  ; 55 - EOR (Zero page, X)
                dw      offset p_instr_lsr_zpx  ; 56 - LSR (Zero page, X)
                dw      offset p_instr_futexp   ; 57 - Future Expansion
                dw      offset p_instr_cli      ; 58 - CLI
                dw      offset p_instr_eor_aby  ; 59 - EOR (Absolute, Y)
                dw      offset p_instr_futexp   ; 5A - Future Expansion
                dw      offset p_instr_futexp   ; 5B - Future Expansion
                dw      offset p_instr_futexp   ; 5C - Future Expansion
                dw      offset p_instr_eor_abx  ; 5D - EOR (Absolute, X)
                dw      offset p_instr_lsr_abx  ; 5E - LSR (Absolute, X)
                dw      offset p_instr_futexp   ; 5F - Future Expansion
                dw      offset p_instr_rts      ; 60 - RTS
                dw      offset p_instr_adc_inx  ; 61 - ADC (Indirect, X)
                dw      offset p_instr_futexp   ; 62 - Future Expansion
                dw      offset p_instr_futexp   ; 63 - Future Expansion
                dw      offset p_instr_futexp   ; 64 - Future Expansion
                dw      offset p_instr_adc_zp   ; 65 - ADC (Zero page)
                dw      offset p_instr_ror_zp   ; 66 - ROR (Zero page)
                dw      offset p_instr_futexp   ; 67 - Future Expansion
                dw      offset p_instr_pla      ; 68 - PLA
                dw      offset p_instr_adc_im   ; 69 - ADC (Immediate)
                dw      offset p_instr_ror_ac   ; 6A - ROR (Accumulator)
                dw      offset p_instr_futexp   ; 6B - Future Expansion
                dw      offset p_instr_jmp_in   ; 6C - JMP (Indirect)
                dw      offset p_instr_adc_ab   ; 6D - ADC (Absolute)
                dw      offset p_instr_ror_ab   ; 6E - ROR (Absolute)
                dw      offset p_instr_futexp   ; 6F - Future Expansion
                dw      offset p_instr_bvs      ; 70 - BVS
                dw      offset p_instr_adc_iny  ; 71 - ADC (Indirect, Y)
                dw      offset p_instr_futexp   ; 72 - Future Expansion
                dw      offset p_instr_futexp   ; 73 - Future Expansion
                dw      offset p_instr_futexp   ; 74 - Future Expansion
                dw      offset p_instr_adc_zpx  ; 75 - ADC (Zero page, X)
                dw      offset p_instr_ror_zpx  ; 76 - ROR (Zero page, X)
                dw      offset p_instr_futexp   ; 77 - Future Expansion
                dw      offset p_instr_sei      ; 78 - SEI
                dw      offset p_instr_adc_aby  ; 79 - ADC (Absolute, Y)
                dw      offset p_instr_futexp   ; 7A - Future Expansion
                dw      offset p_instr_futexp   ; 7B - Future Expansion
                dw      offset p_instr_futexp   ; 7C - Future Expansion
                dw      offset p_instr_adc_abx  ; 7D - ADC (Absolute, X)
                dw      offset p_instr_ror_abx  ; 7E - ROR (Absolute, X)
                dw      offset p_instr_futexp   ; 7F - Future Expansion
                dw      offset p_instr_futexp   ; 80 - Future Expansion
                dw      offset p_instr_sta_inx  ; 81 - STA (Indirect, X)
                dw      offset p_instr_futexp   ; 82 - Future Expansion
                dw      offset p_instr_futexp   ; 83 - Future Expansion
                dw      offset p_instr_sty_zp   ; 84 - STY (Zero page)
                dw      offset p_instr_sta_zp   ; 85 - STA (Zero page)
                dw      offset p_instr_stx_zp   ; 86 - STX (Zero page)
                dw      offset p_instr_futexp   ; 87 - Future Expansion
                dw      offset p_instr_dey      ; 88 - DEY
                dw      offset p_instr_futexp   ; 89 - Future Expansion
                dw      offset p_instr_txa      ; 8A - TXA
                dw      offset p_instr_futexp   ; 8B - Future Expansion
                dw      offset p_instr_sty_ab   ; 8C - STY (Absolute)
                dw      offset p_instr_sta_ab   ; 8D - STA (Absolute)
                dw      offset p_instr_stx_ab   ; 8E - STX (Absolute)
                dw      offset p_instr_futexp   ; 8F - Future Expansion
                dw      offset p_instr_bcc      ; 90 - BCC
                dw      offset p_instr_sta_iny  ; 91 - STA (Indirect, Y)
                dw      offset p_instr_futexp   ; 92 - Future Expansion
                dw      offset p_instr_futexp   ; 93 - Future Expansion
                dw      offset p_instr_sty_zpx  ; 94 - STY (Zero page, X)
                dw      offset p_instr_sta_zpx  ; 95 - STA (Zero page, X)
                dw      offset p_instr_stx_zpy  ; 96 - STX (Zero page, Y)
                dw      offset p_instr_futexp   ; 97 - Future Expansion
                dw      offset p_instr_tya      ; 98 - TYA
                dw      offset p_instr_sta_aby  ; 99 - STA (Absolute, Y)
                dw      offset p_instr_txs      ; 9A - TXS
                dw      offset p_instr_futexp   ; 9B - Future Expansion
                dw      offset p_instr_futexp   ; 9C - Future Expansion
                dw      offset p_instr_sta_abx  ; 9D - STA (Absolute, X)
                dw      offset p_instr_futexp   ; 9E - Future Expansion
                dw      offset p_instr_futexp   ; 9F - Future Expansion
                dw      offset p_instr_ldy_im   ; A0 - LDY (Immediate)
                dw      offset p_instr_lda_inx  ; A1 - LDA (Indirect, X)
                dw      offset p_instr_ldx_im   ; A2 - LDX (Immediate)
                dw      offset p_instr_futexp   ; A3 - Future Expansion
                dw      offset p_instr_ldy_zp   ; A4 - LDY (Zero page)
                dw      offset p_instr_lda_zp   ; A5 - LDA (Zero page)
                dw      offset p_instr_ldx_zp   ; A6 - LDX (Zero page)
                dw      offset p_instr_futexp   ; A7 - Future Expansion
                dw      offset p_instr_tay      ; A8 - TAY
                dw      offset p_instr_lda_im   ; A9 - LDA (Immediate)
                dw      offset p_instr_tax      ; AA - TAX
                dw      offset p_instr_futexp   ; AB - Future Expansion
                dw      offset p_instr_ldy_ab   ; AC - LDY (Absolute)
                dw      offset p_instr_lda_ab   ; AD - LDA (Absolute)
                dw      offset p_instr_ldx_ab   ; AE - LDX (Absolute)
                dw      offset p_instr_futexp   ; AF - Future Expansion
                dw      offset p_instr_bcs      ; B0 - BCS
                dw      offset p_instr_lda_iny  ; B1 - LDA (Indirect, Y)
                dw      offset p_instr_futexp   ; B2 - Future Expansion
                dw      offset p_instr_futexp   ; B3 - Future Expansion
                dw      offset p_instr_ldy_zpx  ; B4 - LDY (Zero page, X)
                dw      offset p_instr_lda_zpx  ; B5 - LDA (Zero page, X)
                dw      offset p_instr_ldx_zpy  ; B6 - LDX (Zero page, Y)
                dw      offset p_instr_futexp   ; B7 - Future Expansion
                dw      offset p_instr_clv      ; B8 - CLV
                dw      offset p_instr_lda_aby  ; B9 - LDA (Absolute, Y)
                dw      offset p_instr_tsx      ; BA - TSX
                dw      offset p_instr_futexp   ; BB - Future Expansion
                dw      offset p_instr_ldy_abx  ; BC - LDY (Absolute, X)
                dw      offset p_instr_lda_abx  ; BD - LDA (Absolute, X)
                dw      offset p_instr_ldx_aby  ; BE - LDX (Absolute, Y)
                dw      offset p_instr_futexp   ; BF - Future Expansion
                dw      offset p_instr_cpy_im   ; C0 - CPY (Immediate)
                dw      offset p_instr_cmp_inx  ; C1 - CMP (Indirect, X)
                dw      offset p_instr_futexp   ; C2 - Future Expansion
                dw      offset p_instr_futexp   ; C3 - Future Expansion
                dw      offset p_instr_cpy_zp   ; C4 - CPY (Zero page)
                dw      offset p_instr_cmp_zp   ; C5 - CMP (Zero page)
                dw      offset p_instr_dec_zp   ; C6 - DEC (Zero page)
                dw      offset p_instr_futexp   ; C7 - Future Expansion
                dw      offset p_instr_iny      ; C8 - INY
                dw      offset p_instr_cmp_im   ; C9 - CMP (Immediate)
                dw      offset p_instr_dex      ; CA - DEX
                dw      offset p_instr_futexp   ; CB - Future Expansion
                dw      offset p_instr_cpy_ab   ; CC - CPY (Absolute)
                dw      offset p_instr_cmp_ab   ; CD - CMP (Absolute)
                dw      offset p_instr_dec_ab   ; CE - DEC (Absolute)
                dw      offset p_instr_futexp   ; CF - Future Expansion
                dw      offset p_instr_bne      ; D0 - BNE
                dw      offset p_instr_cmp_iny  ; D1 - CMP (Indirect, Y)
                dw      offset p_instr_futexp   ; D2 - Future Expansion
                dw      offset p_instr_futexp   ; D3 - Future Expansion
                dw      offset p_instr_futexp   ; D4 - Future Expansion
                dw      offset p_instr_cmp_zpx  ; D5 - CMP (Zero page, X)
                dw      offset p_instr_dec_zpx  ; D6 - DEC (Zero page, X)
                dw      offset p_instr_futexp   ; D7 - Future Expansion
                dw      offset p_instr_cld      ; D8 - CLD
                dw      offset p_instr_cmp_aby  ; D9 - CMP (Absolute, Y)
                dw      offset p_instr_futexp   ; DA - Future Expansion
                dw      offset p_instr_futexp   ; DB - Future Expansion
                dw      offset p_instr_futexp   ; DC - Future Expansion
                dw      offset p_instr_cmp_abx  ; DD - CMP (Absolute, X)
                dw      offset p_instr_dec_abx  ; DE - DEC (Absolute, X)
                dw      offset p_instr_futexp   ; DF - Future Expansion
                dw      offset p_instr_cpx_im   ; E0 - CPX (Immediate)
                dw      offset p_instr_sbc_inx  ; E1 - SBC (Indirect, X)
                dw      offset p_instr_futexp   ; E2 - Future Expansion
                dw      offset p_instr_futexp   ; E3 - Future Expansion
                dw      offset p_instr_cpx_zp   ; E4 - CPX (Zero page)
                dw      offset p_instr_sbc_zp   ; E5 - SBC (Zero page)
                dw      offset p_instr_inc_zp   ; E6 - INC (Zero page)
                dw      offset p_instr_futexp   ; E7 - Future Expansion
                dw      offset p_instr_inx      ; E8 - INX
                dw      offset p_instr_sbc_im   ; E9 - SBC (Immediate)
                dw      offset p_instr_nop      ; EA - NOP
                dw      offset p_instr_futexp   ; EB - Future Expansion
                dw      offset p_instr_cpx_ab   ; EC - CPX (Absolute)
                dw      offset p_instr_sbc_ab   ; ED - SBC (Absolute)
                dw      offset p_instr_inc_ab   ; EE - INC (Absolute)
                dw      offset p_instr_futexp   ; EF - Future Expansion
                dw      offset p_instr_beq      ; F0 - BEQ
                dw      offset p_instr_sbc_iny  ; F1 - SBC (Indirect, Y)
                dw      offset p_instr_futexp   ; F2 - Future Expansion
                dw      offset p_instr_futexp   ; F3 - Future Expansion
                dw      offset p_instr_futexp   ; F4 - Future Expansion
                dw      offset p_instr_sbc_zpx  ; F5 - SBC (Zero page, X)
                dw      offset p_instr_inc_zpx  ; F6 - INC (Zero page, X)
                dw      offset p_instr_futexp   ; F7 - Future Expansion
                dw      offset p_instr_sed      ; F8 - SED
                dw      offset p_instr_sbc_aby  ; F9 - SBC (Absolute, Y)
                dw      offset p_instr_futexp   ; FA - Future Expansion
                dw      offset p_instr_futexp   ; FB - Future Expansion
                dw      offset p_instr_futexp   ; FC - Future Expansion
                dw      offset p_instr_sbc_abx  ; FD - SBC (Absolute, X)
                dw      offset p_instr_inc_abx  ; FE - INC (Absolute, X)
                dw      offset p_instr_futexp   ; FF - Future Expansion

extrn           v_base:word

.code
public          p_init,p_done,p_run,p_showpc,p_reg_pc
public          p_updatevic,p_updateviccnt,p_update6510,p_update6510cnt,p_irq
public          p_showint

public          p_reg_a,p_reg_x,p_reg_y,p_reg_flags,p_reg_pc,p_reg_sp

extrn           v_memchange:proc
extrn           i_memchange:proc

;
; p_loadrom
;
; this will load the rom which filename is at ds:[dx]. It will quit on any
; error. The rom will be loaded to [c64mem]:[di]. It will load [cx] bytes
; of the rom.
;
p_loadrom       proc
                mov     ax,3d00h                ; dos: open file r/o
                int     21h
                jnc     p_loadrom_ok1

                ; unable to open rom file
p_loadrom_err:  mov     dx,offset err_loaderr
                jmp     p_init_err

p_loadrom_ok1:  ; rom file opened
                mov     bx,ax                   ; bx = handle

                push    ds
                push    [c64_mem]
                pop     ds
                mov     dx,di                   ; ds:dx = c64 memory

                mov     ah,3fh                  ; dos: read file
                int     21h
                pop     ds
                jc      p_loadrom_err

                ; rom has been loaded
                mov     ah,3eh                  ; dos: close handle
                int     21h

                ret
p_loadrom       endp


;
; p_init
;
; this will initialize the 6510 microprocessor emulator
;
p_init          proc
                mov     ah,48h                  ; dos: allocate memory
                mov     bx,MEMSIZE/16+1         ; number of paragraphs
                int     21h
                jnc     p_init_ok1

                ; the memory allocation failed. die
                mov     dx,offset err_outofmem
                jmp     p_init_err

p_init_ok1:     ; memory allocation ok
                mov     [c64_mem],ax            ; c64 memory

                mov     es,ax
                xor     di,di                   ; es:di = c64 memory

                ; initialize the c64 memory to some weird pattern
                xor     cx,cx

p_init_1:       ; fill 64 bytes with 0h
                xor     al,al
                mov     al,0ffh                 ; XXX
                xor     dl,dl
p_init_2:       stosb
                inc     dl
                cmp     dl,64
                jne     p_init_2

                ; fill 64 bytes with 0ffh
                mov     al,0ffh
                xor     dl,dl
p_init_3:       stosb
                inc     dl
                cmp     dl,64
                jne     p_init_3

                inc     cx
                cmp     cx,512
                jne     p_init_1

                ; load the KERNEL rom
                mov     dx,offset p_kromfile    ; kernel rom
                mov     cx,KERNEL_ROMSIZE       ; rom size
                mov     di,KERNEL_MEMOFS        ; offset
                call    p_loadrom

                ; load the BASIC rom
                mov     dx,offset p_bromfile    ; basic rom
                mov     cx,BASIC_ROMSIZE        ; rom size
                mov     di,BASIC_MEMOFS         ; offset
                call    p_loadrom

                ; load the CHARACTER rom
                mov     dx,offset p_cromfile    ; kernel rom
                mov     cx,CHAR_ROMSIZE         ; rom size
                mov     di,CHAR_MEMOFS          ; offset
                call    p_loadrom

                ; now initialize our virtual 6510
                ; set the stack pointer
                mov     [p_reg_sp],STACK_END

                ; set [fs] to the commodore memory
                push    [c64_mem]
                pop     fs                      ; fs = c64 memory

                ; start executing the boot code
                mov     si,BOOTPTR
                mov     si,fs:[si]
                mov     [p_reg_pc],si

                ; clean special registers
                mov     word ptr fs:[0],0

                ret

p_init_err:     mov     ah,9                    ; dos: print message
                int     21h

                mov     ax,4c01h                ; dos: exit, error code 1
                int     21h
p_init          endp

;
; p_done
;
; this will deinitialize the 6510 microprocessor emulator
;
p_done          proc
                mov     ah,49h                  ; dos: free memory
                push    [c64_mem]
                pop     es                      ; es = c64 memory
                int     21h
                ret
p_done          endp

;
; p_get_carry
;
; this will set the carry flag if the 6510 carry flag is set, otherwise it'll
; clear it
;
p_get_carry     proc
                push    dx
                mov     dl,[p_reg_flags]
                and     dl,FLAG_C
                jz      p_get_carry1

                stc
                jmp     p_get_carry2

p_get_carry1:   clc
p_get_carry2:   pop     dx
                ret
p_get_carry     endp

;
; p_set_carry
;
; this will copy the x86 carry flag to the 6510 one
;
p_set_carry     proc
                pushf
                mov     dl,[p_reg_flags]
                and     dl,not FLAG_C
                popf
                adc     dl,0
                mov     [p_reg_flags],dl
                ret
p_set_carry     endp

;
; p_get_byte
;
; this will return the next instruction. it will also increment the program
; counter. the instruction will be returned into [al].
;
; changes: si
;
p_get_byte      proc
                mov     si,[p_reg_pc]

                mov     al,byte ptr fs:[si]
                inc     [p_reg_pc]
                ret
p_get_byte      endp

;
; p_get_word
;
; this will return the next word in [ax]. the program counter will also
; be incremented.
;
p_get_word      proc
                call    p_get_byte
                xchg    ah,al                   ; ah = low byte
                call    p_get_byte              ; al = hi byte
                xchg    al,ah                   ; ax = ok now
                ret
p_get_word      endp

;
; p_push
;
; this will put [al] on the c64 stack and decrement [sp].
;
; modifies: si
;
p_push          proc
                movzx   si,[p_reg_sp]           ; si = sp
                add     si,STACK_BASE           ; si = sp + STACK_BASE
                mov     fs:[si],al

                dec     [p_reg_sp]
                ret
p_push          endp

;
; p_push_word
;
; this will put [ax] on the 6510 stack and decrement [sp].
;
; modifies: ax, si
;
p_push_word     proc
                call    p_push
                xchg    al,ah
                call    p_push
                ret
p_push_word     endp



;
; p_run
;
; this will run the next instruction
;
p_run           proc
                mov     ax,[p_reg_pc]
                cmp     ax,BREAKPOINT
                jne     p_run_x

                int     3

p_run_x:        cmp     [p_irq],0               ; irq?
                jz      p_run_1                 ; no, visit p_run_1

                mov     [p_irq],0               ; no irq now

		mov     dl,[p_reg_flags]        ; irq flag set?
		and     dl,FLAG_I
                jnz     p_run_1                 ; yeah, visit p_run_1

		mov     ax,[p_reg_pc]
		call    p_push_word             ; push pc

		mov     al,[p_reg_flags]        ; push flags
		call    p_push

		mov     ax,fs:[IRQ_PTR]
		mov     [p_reg_pc],ax           ; visit IRQ code

		mov     dl,[p_reg_flags]        ; set I flag
		or	dl,FLAG_I
		mov	[p_reg_flags],dl

p_run_1:        call    p_get_byte
                movzx   bx,al                   ; bx = (word)al

                shl     bx,1                    ; bx = 2 * opcode
                add     bx,offset p_calltab     ; bx = 2 * opcode + calltab

                ; call it!
                call    [bx]

                cmp     word ptr fs:[0c1h],0a000h
                jne     p_run_3

;                call    p_showpc                ; XXX

p_run_3:        inc     [p_irqcount]
                mov     eax,[p_irqcount]
                cmp     eax,[p_irqcountcnt]
                jne     p_run_4

                mov     [p_irqcount],0

                mov     [p_irq],1

p_run_4:
                ret
p_run           endp

;
; p_pull
;
; this will retrieve a byte from the stack and return it in [al].
;
; modifies: al, si
;
p_pull          proc
                inc     [p_reg_sp]              ; sp++

                movzx   si,[p_reg_sp]           ; si = sp
                add     si,STACK_BASE           ; si = sp + STACK_BASE
                mov     al,fs:[si]
                ret
p_pull          endp

;
; p_pullword
;
; this will retrieve a word from the stack in return it in [ax].
;
; modifies: ax, si
;
p_pull_word     proc
                call    p_pull
                xchg    ah,al                   ; ah = low byte
                call    p_pull
                ret
p_pull_word     endp

;
; p_setflags
;
; this will set the correct 6510 flags according to the value in [al].
;
; changes: dl.
;
p_setflags      proc
                push    ax bx
                mov     bl,al                   ; bl = al
                mov     dl,[p_reg_flags]        ; dl = 6510 flags

                and     dl,not FLAG_N
                and     dl,not FLAG_Z

                and     bl,80h                  ; bl = al & 80h
                or      bl,bl
                jz      p_setflags_1

                or      dl,FLAG_N

p_setflags_1:   or      al,al
                jnz     p_setflags_2

                or      dl,FLAG_Z

p_setflags_2:   mov     [p_reg_flags],dl        ; active flags

                pop     bx ax
                ret
p_setflags      endp

;
; p_branch
;
; this will branch as [al] tells us to.
;
; modifies: ah, bx, dx
;
p_branch        proc
                xor     ah,ah
                mov     bl,al
                sub     bl,80h                  ; if (bl>0x7xf) -> p_branch_1
                jnc     p_branch_1

                ; branch forward
                mov     bx,[p_reg_pc]
                add     bx,ax
                mov     [p_reg_pc],bx
                ret

p_branch_1:     ; branch backward
                mov     bx,0feh
                sub     bx,ax                   ; bl = number of bytes back

                mov     dx,[p_reg_pc]
                dec     dx
                dec     dx
                sub     dx,bx
                mov     [p_reg_pc],dx

                ret
p_branch        endp

;
; p_valtoasc
;
; this will convert [dl] from a number between 0-15 to [0-9][A-F]
;
p_valtoasc      proc
                cmp     dl,9h
                jg      p_valtoasc_1

                add     dl,30h
                ret

p_valtoasc_1:   add     dl,37h
                ret
p_valtoasc      endp

;
; p_intoasc
;
; this will convert [dl] to a printable number in [bx].
;
p_intoasc       proc
                push    dx
                and     dl,0fh
                call    p_valtoasc
                mov     bl,dl
                pop     dx

                shr     dl,4
                call    p_valtoasc
                mov     bh,dl
                ret
p_intoasc       endp

;
; p_showint
;
; this will show in [ax].
;
p_showint       proc
                mov     dl,ah
                call    p_intoasc
                mov     cx,bx                   ; cx = part one

                mov     dl,al
                call    p_intoasc
                mov     dx,bx                   ; dx = part two

;                mov     ah,2
                mov     ah,0eh

;                mov     dl,ch

                mov     al,ch
                int     10h

;                int     21h
;                mov     dl,cl

                mov     al,cl

;                int     21h

                int     10h

                mov     bx,dx

;                mov     dl,bh
                mov     al,bh
;                int     21h
                int     10h

;                mov     dl,bl
                mov     al,bl
                int     10h
;                int     21h
                ret
p_showint       endp

;
; p_showpc
;
; this will show the program counter register
;
p_showpc        proc
                mov     ax,[p_reg_pc]           ; ax = pc
                call    p_showint

                mov     al,20h
                int     10h

                ret
p_showpc        endp

;
; p_memchange
;
; this will handle memory changes. it expects to have a changed address in
; fs:[si].
;
p_memchange     proc
                cmp     si,VIDEO_OFFSET         ; below video io?
                jl      p_memchange1            ; yeah, visit p_memchange1

                cmp     si,VIDEO_END            ; above video io?
                jg      p_memchange1            ; yeah, visit p_memchange1

                ; let the video memory change do it
                call    v_memchange

                ret

p_memchange1:   mov     ax,[v_base]
                cmp     si,ax                   ; below video memory?
                jl      p_memchange2            ; yeah, visit p_memchange2

                add     ax,(40*25)              ; above video memory?
                cmp     si,ax
                jg      p_memchange2            ; yeah, visit p_memchange2

                ; let the video change do it
                call    v_memchange

p_memchange2:   cmp     si,IO_OFFSET            ; below io memory?
                jl      p_memchange3            ; yeah, visit p_memchange3

                cmp     si,IO_END               ; above io memory?
                jg      p_memchange3            ; yeah, visit p_memchange3

                call    i_memchange
                ret

p_memchange3:   ret
p_memchange     endp

;
; p_get_zeropage
;
; this will handle zero page requests. it will read the appropiate bytes and
; return change [si] so fs:[si] points to the memory.
;
; changes: al, si
;
p_get_zeropage  proc
                call    p_get_byte
                movzx   si,al                   ; si = (word)next byte

                ret
p_get_zeropage  endp

;
; p_get_zeropagex
;
; this will handle zero page X requests. it will read the appropiate bytes
; and return change [si] so fs:[si] points to the memory.
;
; changes: al, si, bx
;
p_get_zeropagex proc
                call    p_get_zeropage
                movzx   bx,[p_reg_x]
                add     si,bx                   ; si = (word)next byte + X

                ret
p_get_zeropagex endp

;
; p_get_zeropagey
;
; this will handle zero page Y requests. it will read the appropiate bytes
; and return change [si] so fs:[si] points to the memory.
;
; changes: al, si, bx
;
p_get_zeropagey proc
                call    p_get_zeropage
                movzx   bx,[p_reg_y]
                add     si,bx                   ; si = (word)next byte + Y

                ret
p_get_zeropagey endp

;
; p_get_absolute
;
; this will handle absolute requests. it will read the appropiate bytes and
; return change [si] so fs:[si] points to the memory.
;
; changes: al, si
;
p_get_absolute  proc
                call    p_get_word
                mov     si,ax                   ; si = next word

                ret
p_get_absolute  endp

;
; p_get_absolutex
;
; this will handle absolute X requests. it will read the appropiate bytes and
; return change [si] so fs:[si] points to the memory.
;
; changes: al, si
;
p_get_absolutex proc
                call    p_get_absolute
                movzx   bx,[p_reg_x]
                add     si,bx                   ; si = next word + X

                ret
p_get_absolutex endp

;
; p_get_absolutey
;
; this will handle absolute Y requests. it will read the appropiate bytes and
; return change [si] so fs:[si] points to the memory.
;
; changes: al, si
;
p_get_absolutey proc
                call    p_get_absolute
                movzx   bx,[p_reg_y]
                add     si,bx                   ; si = next word + X

                ret
p_get_absolutey endp

;
; p_get_immediate
;
; this will handle immediate requests. it will read the appropiate bytes and
; return the immediate value in al.
;
; changes: al, si
;
p_get_immediate proc
                jmp     p_get_byte
p_get_immediate endp

;
; p_get_indirectx
;
; this will handle indirect x requests. it will read the appropiate bytes and
; change [si] so fs:[si] points to the memory.
;
; changes: al, bx, si
;
p_get_indirectx proc
                call    p_get_byte
                movzx   si,al                   ; si = (word)next byte

                movzx   bx,[p_reg_x]            ; bx = (word)x
                add     si,bx                   ; si = (word)next byte + x

                mov     si,fs:[si]
                ret
p_get_indirectx endp

;
; p_get_indirecty
;
; this will handle indirect y requests. it will read the appropiate bytes and
; change [si] so fs:[si] points to the memory.
;
; changes: al, bx, si
;
p_get_indirecty proc
                call    p_get_byte
                movzx   si,al                   ; si = (word)next byte

                mov     si,fs:[si]
                movzx   bx,[p_reg_y]            ; bx = (word)y
                add     si,bx

                ret
p_get_indirecty endp

;
; p_generic_or
;
; this will handle generic or's. it will OR A with [bl] and put the result
; in A. it will also update the flags.
;
p_generic_or    proc
                mov     al,[p_reg_a]            ; al = A
                or      al,bl
                mov     [p_reg_a],al            ; A = A || [bl]

                jmp     p_setflags              ; set flags
p_generic_or    endp

;
; p_generic_and
;
; this will handle generic and's. it will AND A with [bl] and put the result
; in A. it will also update the flags.
;
p_generic_and   proc
                mov     al,[p_reg_a]            ; al = A
                and     al,bl
                mov     [p_reg_a],al            ; A = A && [bl]

                call    p_setflags              ; set flags
                ret
p_generic_and   endp

;
; p_generic_bit
;
; this will handle generic bit's. it will bit A with [bl] and put the result
; in [al]. it'll also update the flags.
;
; changes: al, bl, cl, dl
;
p_generic_bit   proc
                mov     al,[p_reg_a]
                and     al,bl
                call    p_setflags

                and     dl,not FLAG_N
                and     dl,not FLAG_V

                mov     cl,al
                and     cl,80h
                jz      p_generic_bit1

                or      dl,FLAG_N

p_generic_bit1: and     al,40h
                jz      p_generic_bit2

                or      dl,FLAG_V

p_generic_bit2: mov     [p_reg_flags],dl
                ret
p_generic_bit   endp

;
; p_generic_eor
;
; this will handle generic eor's. it will XOR A with [bl] and put the result
; in A. it will also update the flags.
;
p_generic_eor   proc
                mov     al,[p_reg_a]            ; al = A
                xor     al,bl
                mov     [p_reg_a],al            ; A = A || [bl]

                jmp     p_setflags              ; set flags
p_generic_eor   endp

;
; p_generic_adc
;
; this will handle generic adc's. it will add A with C and [bl] and put the
; result in A and C. It will also update the flags.
;
p_generic_adc   proc
                mov     al,[p_reg_a]
                call    p_get_carry
                adc     al,bl

                pushf
                call    p_set_carry
                and     dl,not FLAG_V                   ; clear overflow
                mov     [p_reg_flags],dl
                popf

                jno     p_generic_adc2

                or      dl,FLAG_V                       ; set overflow
                mov     [p_reg_flags],dl

p_generic_adc2: mov     [p_reg_a],al
                call    p_setflags
                ret
p_generic_adc   endp

;
; p_generic_sbc
;
; this will handle generic sbc's. it will sub A from C and [bl] and put the
; result in A and C. It will also update the flags.
;
p_generic_sbc   proc
                call    p_get_carry
                cmc
                sbb     al,bl

                pushf
                cmc
                call    p_set_carry
                and     dl,not FLAG_V
                mov     [p_reg_flags],dl
                popf

                jno     p_generic_sbc1

                or      dl,FLAG_V

p_generic_sbc1: mov     [p_reg_flags],dl
                jmp     p_setflags
p_generic_sbc   endp

;
; p_generic_asl
;
; this will handle generic ASL's. it will asl [al]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_asl   proc
                xor     ah,ah
                add     ax,ax

                clc
                call    p_set_carry

                test    ah,1
                jz      p_generic_asl1

                stc
                call    p_set_carry

p_generic_asl1: jmp     p_setflags
p_generic_asl   endp

;
; p_generic_rol
;
; this will handle generic ROL's. it will rol [al]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_rol   proc
                xor     ah,ah
                call    p_get_carry
                adc     ax,ax

                clc
                call    p_set_carry

                test    ah,1
                jz      p_generic_rol1

                stc
                call    p_set_carry

p_generic_rol1: jmp     p_setflags
p_generic_rol   endp

;
; p_generic_ror
;
; this will handle generic ROR's. it will ror [al]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_ror   proc
                xor     ah,ah
                call    p_get_carry
                jnc     p_generic_ror1

                inc     ah

p_generic_ror1: clc
                call    p_set_carry

                test    al,1
                jz      p_generic_ror2

                stc
                call    p_set_carry

p_generic_ror2: shr     ax,1

                jmp     p_setflags
p_generic_ror   endp


;
; p_generic_lsr
;
; this will handle generic LSR's. it will lsr [al]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_lsr   proc
                clc
                call    p_set_carry

                push    ax
                and     al,1
                pop     ax
                jz      p_generic_lsr1

                stc
                call    p_set_carry

p_generic_lsr1: shr     al,1
                call    p_setflags
                ret
p_generic_lsr   endp

;
; p_generic_cpy
;
; this will handle generic CPY's. it will cpy [bl]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_cpy   proc
                stc
                call    p_set_carry

                mov     al,[p_reg_y]
                jmp     p_generic_sbc
p_generic_cpy   endp

;
; p_generic_cpx
;
; this will handle generic CPX's. it will cpx [bl]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_cpx   proc
                stc
                call    p_set_carry

                mov     al,[p_reg_x]
                jmp     p_generic_sbc
p_generic_cpx   endp

;
; p_generic_cmp
;
; this will handle generic CMP's. it will cmp [bl]. the result will be put in
; [al]. It will also update the flags.
;
p_generic_cmp   proc
                stc
                call    p_set_carry

                mov     al,[p_reg_a]
                jmp     p_generic_sbc
p_generic_cmp   endp

;
; p_isrom
;
; this will set the carry if the memory at fs:[si] is ROM, otherwise the
; carry flag will be cleared.
;
p_isrom         proc
                cmp     si,BASIC_MEMOFS
                jl      p_isrom1

                cmp     si,BASIC_ROMSIZE+BASIC_MEMOFS
                jg      p_isrom1

                ; it's in the BASIC rom. No way we write there.
                stc
                ret

p_isrom1:       cmp     si,KERNEL_MEMOFS
                jl      p_isrom2

                cmp     si,KERNEL_MEMOFS+KERNEL_ROMSIZE-1
                jg      p_isrom2

                ; it's in the KERNEL rom. No way we write there.
                stc
                ret

p_isrom2:       ; it's just normal ram. we can write there.
                clc
                ret
p_isrom         endp

;
; p_poke
;
; this will poke byte [al] to locate [si] in the 6510 memory
;
p_poke          proc
                push    ax
                call    p_isrom
                jc      p_poke1

                mov     fs:[si],al
                call    p_memchange

p_poke1:        pop     ax
                ret
p_poke          endp

;
; p_peek
;
; this will peek a byte from [si] in the 6510 memory. it'll return the byte
; in [al].
;
p_peek          proc
                mov     al,fs:[si]
                ret
p_peek          endp

;
; p_peekaddr
;
; this will peek an adress from [si] in the 6510 memory. the address will be
; returned in [si].
;
p_peekaddr      proc
                call    p_peek
                mov     ah,al
                call    p_peek

                mov     si,ax
                mov     si,[si]
                ret
p_peekaddr      endp

; ==========================================================================
;
;                         6510 INSTRUCTIONS
;
; ==========================================================================
; Future expansion
p_instr_futexp  proc
                ret
p_instr_futexp  endp

; >> BRK
p_instr_brk     proc
                mov     al,[p_reg_flags]
                or      al,FLAG_B
                or      al,FLAG_I
                mov     [p_reg_flags],al        ; set BREAK and INT flags

                mov     ax,[p_reg_pc]
                inc     ax
                inc     ax
                call    p_push_word             ; transfer PC + 2 to stack

                mov     al,[p_reg_flags]
                call    p_push

                mov     si,0fffeh
                call    p_peekaddr
                mov     [p_reg_pc],si           ; si = peek(0xfffe)
                ret
p_instr_brk     endp

; >> ORA - Indirect, X
p_instr_ora_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_inx endp

; >> ORA - Zero page
p_instr_ora_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_zp  endp

; >> ASL - Zero page
p_instr_asl_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                call    p_generic_asl
                jmp     p_poke                  ; M = M << 1
p_instr_asl_zp  endp

; >> PHP
p_instr_php     proc
                mov     al,[p_reg_flags]
                jmp     p_push                  ; push it
p_instr_php     endp

; >> ORA - Immediate
p_instr_ora_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al

                jmp     p_generic_or            ; A = A | [imm]
p_instr_ora_im  endp

; >> ASL - Accumulator
p_instr_asl_ac  proc
                mov     al,[p_reg_a]
                call    p_generic_asl
                mov     [p_reg_a],al            ; A = A << 1

                ret
p_instr_asl_ac  endp

; >> ORA - Absolute
p_instr_ora_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_ab  endp

; >> ASL - Absolute
p_instr_asl_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                call    p_generic_asl
                jmp     p_poke                  ; M = M << 1
p_instr_asl_ab  endp

; >> BPL
p_instr_bpl     proc
                call    p_get_byte

                mov     bl,[p_reg_flags]
                and     bl,FLAG_N               ; N flag set?
                jnz     p_instr_bpl1            ; nope, visit p_instr_bpl1

                call    p_branch                ; branch!

p_instr_bpl1:   ret
p_instr_bpl     endp

; >> ORA - Indirect, Y
p_instr_ora_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_iny endp

; >> ORA - Zero page, X
p_instr_ora_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_zpx endp

; >> ASL - Zero page, X
p_instr_asl_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                call    p_generic_asl
                jmp     p_poke                  ; M = M << 1
p_instr_asl_zpx endp

; >> CLC
p_instr_clc     proc
                clc
                jmp     p_set_carry
p_instr_clc     endp

; >> ORA - Absolute, Y
p_instr_ora_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_aby endp

; >> ORA - Absolute, X
p_instr_ora_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_or            ; A = A | M
p_instr_ora_abx endp

; >> ASL - Absolute, X
p_instr_asl_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                call    p_generic_asl
                jmp     p_poke                  ; M = M << 1
p_instr_asl_abx endp

; >> JSR
p_instr_jsr     proc
                mov     ax,[p_reg_pc]
                inc     ax
                inc     ax                      ; ax = PC + 2
                call    p_push_word             ; push it

                call    p_get_word
                mov     [p_reg_pc],ax           ; jump!
                ret
p_instr_jsr     endp

; >> AND - Indirect, X
p_instr_and_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_and           ; A = A & M
                ret
p_instr_and_inx endp

; >> BIT - Zero page
p_instr_bit_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_bit
                ret
p_instr_bit_zp  endp

; >> AND - Zero page
p_instr_and_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_and           ; A = A & M
                ret
p_instr_and_zp  endp

; >> ROL - Zero page
p_instr_rol_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                call    p_generic_rol
                jmp     p_poke                  ; M = M rol 1
p_instr_rol_zp  endp

; >> PLP
p_instr_plp     proc
                call    p_pull
                mov     [p_reg_flags],al        ; new flags

                ret
p_instr_plp     endp

; >> AND - Immediate
p_instr_and_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al

                jmp     p_generic_and           ; A = A & [imm]
p_instr_and_im  endp

; >> ROL - Accumulator
p_instr_rol_ac  proc
                mov     al,[p_reg_a]
                call    p_generic_rol
                mov     [p_reg_a],1             ; A = A ROL 1

                ret
p_instr_rol_ac  endp

; >> BIT - Absolute
p_instr_bit_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_bit
                ret
p_instr_bit_ab  endp

; >> AND - Absolute
p_instr_and_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_and           ; A = A & M
                ret
p_instr_and_ab  endp

; >> ROL - Absolute
p_instr_rol_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                call    p_generic_rol
                jmp     p_poke                  ; M = M rol 1
p_instr_rol_ab  endp

; >> BMI
p_instr_bmi     proc
                call    p_get_byte

                mov     bl,[p_reg_flags]
                and     bl,FLAG_N               ; N flag clear?
                jz      p_instr_bmi1            ; nope, visit p_instr_bpl1

                call    p_branch                ; branch!

p_instr_bmi1:   ret
p_instr_bmi     endp

; >> AND - Indirect, Y
p_instr_and_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_and           ; A = A & M
                ret
p_instr_and_iny endp

; >> AND - Zero page, X
p_instr_and_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_and           ; A = A & M
                ret
p_instr_and_zpx endp

; >> ROL - Zero page, X
p_instr_rol_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                call    p_generic_rol
                jmp     p_poke                  ; A = A rol 1
p_instr_rol_zpx endp

; >> SEC
p_instr_sec     proc
                stc
                jmp     p_set_carry
p_instr_sec     endp

; >> AND - Absolute, Y
p_instr_and_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_and           ; A = A & M
p_instr_and_aby endp

; >> AND - Absolute, X
p_instr_and_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_and           ; A = A & M
                ret
p_instr_and_abx endp

; >> ROL - Absolute, X
p_instr_rol_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                call    p_generic_rol
                jmp     p_poke                  ; M = M rol 1
p_instr_rol_abx endp

; >> RTI
p_instr_rti     proc
                call    p_pull
                mov     [p_reg_flags],al        ; new flags

                call    p_pull_word
                mov     [p_reg_pc],ax           ; new pc
                ret
p_instr_rti     endp

; >> EOR - Indirect, X
p_instr_eor_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_inx endp

; >> EOR - Zero page
p_instr_eor_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_zp  endp

; >> LSR - Zero page
p_instr_lsr_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                call    p_generic_lsr
                jmp     p_poke                  ; A = A >> 1
p_instr_lsr_zp  endp

; >> PHA
p_instr_pha     proc
                mov     al,[p_reg_a]
                jmp     p_push
p_instr_pha     endp

; >> EOR - Immediate
p_instr_eor_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al                   ; bl = operand

                jmp     p_generic_eor           ; A = A ^ [imm]
p_instr_eor_im  endp

; >> LSR - Accumulator
p_instr_lsr_ac  proc
                mov     al,[p_reg_a]            ; al = A
                call    p_generic_lsr
                mov     [p_reg_a],al            ; A = A << 1

                ret
p_instr_lsr_ac  endp

; >> JMP - Absolute
p_instr_jmp_ab  proc
                call    p_get_word              ; ax = next word

                mov     [p_reg_pc],ax           ; jump!
                ret
p_instr_jmp_ab  endp

; >> EOR - Absolute
p_instr_eor_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_ab  endp

; >> LSR - Absolute
p_instr_lsr_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                call    p_generic_lsr
                jmp     p_poke                  ; M = M >> 1
p_instr_lsr_ab  endp

; >> BVC
p_instr_bvc     proc
                call    p_get_byte

                mov     bl,[p_reg_flags]
                and     bl,FLAG_V
                jnz     p_instr_bvc1            ; nope, visit p_instr_bvc1

                call    p_branch                ; branch!

p_instr_bvc1:   ret
p_instr_bvc     endp

; >> EOR - Indirect, Y
p_instr_eor_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_iny endp

; >> EOR - Zero page, X
p_instr_eor_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_zpx endp

; >> LSR - Zero page, X
p_instr_lsr_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                call    p_generic_lsr
                jmp     p_poke                  ; M = M >> 1
p_instr_lsr_zpx endp

; >> CLI
p_instr_cli     proc
                mov     al,[p_reg_flags]
                and     al,not FLAG_I
                mov     [p_reg_flags],al        ; FLAGS |= ~FLAG_I

                ret
p_instr_cli     endp

; >> EOR - Absolute, Y
p_instr_eor_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_aby endp

; >> EOR - Absolute, X
p_instr_eor_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_eor           ; A = A ^ M
p_instr_eor_abx endp

; >> LSR - Absolute, X
p_instr_lsr_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                call    p_generic_lsr
                jmp     p_poke                  ; M = M << 1
p_instr_lsr_abx endp

; >> RTS
p_instr_rts     proc
                call    p_pull_word
                mov     [p_reg_pc],ax           ; new pc

                ret
p_instr_rts     endp

; >>  DC - Indirect, X
p_instr_adc_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_inx endp

; >> ADC - Zero page
p_instr_adc_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_zp  endp

; >> ROR - Zero page
p_instr_ror_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                call    p_generic_ror
                jmp     p_poke                  ; M = M ror 1
p_instr_ror_zp  endp

; >> PLA
p_instr_pla     proc
                call    p_pull
                mov     [p_reg_a],al            ; new A

                jmp     p_setflags              ; set flags
p_instr_pla     endp

; >> ADC - Immediate
p_instr_adc_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_im  endp

; >> ROR - Accumulator
p_instr_ror_ac  proc
                mov     al,[p_reg_a]
                call    p_generic_ror
                mov     [p_reg_a],al            ; A = A ROR 1

                ret
p_instr_ror_ac  endp

; >> JMP - Indirect
p_instr_jmp_in  proc
                call    p_get_word
                mov     si,ax                   ; si = next word

                call    p_peek
                xchg    ah,al
                inc     si
                call    p_peek
                xchg    ah,al

                mov     [p_reg_pc],ax           ; jump!
                ret
p_instr_jmp_in  endp

; >> ADC - Absolute
p_instr_adc_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_ab  endp

; >> ROR - Absolute
p_instr_ror_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                call    p_generic_ror
                jmp     p_poke                  ; M = M ror 1
p_instr_ror_ab  endp

; >> BVS
p_instr_bvs     proc
                call    p_get_byte

                mov     bl,[p_reg_flags]
                and     bl,FLAG_V               ; V flag set?
                or      bl,bl
                jnz     p_instr_bvs1            ; nope, visit p_instr_bvs1

                call    p_branch                ; branch!
                ret

p_instr_bvs1:   ret
p_instr_bvs     endp

; >> ADC - Indirect, Y
p_instr_adc_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_iny endp

; >> ADC - Zero page, X
p_instr_adc_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_zpx endp

; >> ROR - Zero Page, X
p_instr_ror_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                call    p_generic_ror
                jmp     p_poke                  ; M = M ror 1
p_instr_ror_zpx endp

; >> SEI
p_instr_sei     proc
                mov     al,[p_reg_flags]
                or      al,FLAG_I
                mov     [p_reg_flags],al        ; FLAGS &= FLAG_I
                ret
p_instr_sei     endp

; >> ADC - Absolute, Y
p_instr_adc_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_aby endp

; >> ADC - Absolute, X
p_instr_adc_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_adc
                ret
p_instr_adc_abx endp

; >> ROR - Absolute, X
p_instr_ror_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                call    p_generic_ror
                jmp     p_poke                  ; M = M ror 1
p_instr_ror_abx endp

; >> STA - Indirect, X
p_instr_sta_inx proc
                call    p_get_indirectx         ; get memory address in si

                mov     al,[p_reg_a]
                jmp     p_poke                  ; M = A
p_instr_sta_inx endp

; >> STY - Zero page
p_instr_sty_zp  proc
                call    p_get_zeropage          ; get memory address in si

                mov     al,[p_reg_y]
                call    p_poke                  ; M = Y
                ret
p_instr_sty_zp  endp

; >> STA - Zero page
p_instr_sta_zp  proc
                call    p_get_zeropage          ; get memory address in si

                mov     al,[p_reg_a]
                call    p_poke                  ; M = A
                ret
p_instr_sta_zp  endp

; >> STX - Zero page
p_instr_stx_zp  proc
                call    p_get_zeropage          ; get memory address in si

                mov     al,[p_reg_x]
                jmp     p_poke                  ; M = X
p_instr_stx_zp  endp

; >> DEY
p_instr_dey     proc
                mov     al,[p_reg_y]
                dec     al
                mov     [p_reg_y],al

                call    p_setflags              ; Set N and Z flags
                ret
p_instr_dey     endp

; >> TXA
p_instr_txa     proc
                mov     al,[p_reg_x]            ; X -> al
                mov     [p_reg_a],al            ; al -> A

                jmp     p_setflags
p_instr_txa     endp

; >> STY - Absolute
p_instr_sty_ab  proc
                call    p_get_absolute          ; get memory address in si

                mov     al,[p_reg_y]
                jmp     p_poke                  ; M = Y
p_instr_sty_ab  endp

; >> STA - Absolute
p_instr_sta_ab  proc
                call    p_get_absolute          ; get memory address in si

                mov     al,[p_reg_a]
                call    p_poke                  ; M = A
                ret
p_instr_sta_ab  endp

; >> STX - Absolute
p_instr_stx_ab  proc
                call    p_get_absolute          ; get memory address in si

                mov     al,[p_reg_x]
                call    p_poke                  ; M = X
                ret
p_instr_stx_ab  endp

; >> BCC
p_instr_bcc     proc
                call    p_get_byte

                call    p_get_carry
                jc      p_instr_bcc1

                call    p_branch                ; branch!

p_instr_bcc1:   ret
p_instr_bcc     endp

; >> STA - Indirect, Y
p_instr_sta_iny proc
                call    p_get_indirecty         ; get memory address in si

                mov     al,[p_reg_a]
                call    p_poke                  ; M = A
                ret
p_instr_sta_iny endp

; >> STY - Zero page, X
p_instr_sty_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                mov     al,[p_reg_y]
                call    p_poke                  ; M = Y
                ret
p_instr_sty_zpx endp

; >> STA - Zero page, X
p_instr_sta_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                mov     al,[p_reg_a]
                call    p_poke                  ; M = A
                ret
p_instr_sta_zpx endp

; >> STX - Zero page, Y
p_instr_stx_zpy proc
                call    p_get_zeropagey         ; get memory address in si

                mov     al,[p_reg_x]
                call    p_poke                  ; M = X
                ret
p_instr_stx_zpy endp

; >> TYA
p_instr_tya     proc
                mov     al,[p_reg_y]            ; Y -> al
                mov     [p_reg_a],al            ; al -> A

                jmp     p_setflags
p_instr_tya     endp

; >> STA - Absolute, Y
p_instr_sta_aby proc
                call    p_get_absolutey         ; get memory address in si

                mov     al,[p_reg_a]
                call    p_poke                  ; M = A
                ret
p_instr_sta_aby endp

; >> TXS
p_instr_txs     proc
                mov     al,[p_reg_x]
                mov     [p_reg_sp],al           ; SP = X
                ret
p_instr_txs     endp

; >> STA - Absolute, X
p_instr_sta_abx proc
                call    p_get_absolutex         ; get memory address in si

                mov     al,[p_reg_a]
                call    p_poke                  ; M = A
                ret
p_instr_sta_abx endp

; >> LDY - Immediate
p_instr_ldy_im  proc
                call    p_get_immediate         ; get immediate value
                mov     [p_reg_y],al            ; Y = M

                jmp     p_setflags
p_instr_ldy_im  endp

; >> LDA - Indirect, X
p_instr_lda_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; A = M

                jmp     p_setflags
p_instr_lda_inx endp

; >> LDX - Immediate
p_instr_ldx_im  proc
                call    p_get_immediate         ; get immediate value
                mov     [p_reg_x],al            ; X = M

                jmp     p_setflags
p_instr_ldx_im  endp

; >> LDY - Zero page
p_instr_ldy_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     [p_reg_y],al            ; Y = M

                jmp     p_setflags
p_instr_ldy_zp  endp

; >> LDA - Zero page
p_instr_lda_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; Y = M

                jmp     p_setflags
p_instr_lda_zp  endp

; >> LDX - Zero page
p_instr_ldx_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     [p_reg_x],al            ; X = M

                jmp     p_setflags
p_instr_ldx_zp  endp

; >> TAY
p_instr_tay     proc
                mov     al,[p_reg_a]
                mov     [p_reg_y],al            ; Y = A

                jmp     p_setflags
p_instr_tay     endp

; >> LDA - Immediate
p_instr_lda_im  proc
                call    p_get_immediate         ; get immediate value
                mov     [p_reg_a],al            ; A = [imm]

                jmp     p_setflags
p_instr_lda_im  endp

; >> TAX
p_instr_tax     proc
                mov     al,[p_reg_a]
                mov     [p_reg_x],al            ; X = A

                jmp     p_setflags
p_instr_tax     endp

; >> LDY - Absolute
p_instr_ldy_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     [p_reg_y],al            ; Y = M

                jmp     p_setflags
p_instr_ldy_ab  endp

; >> LDA - Absolute
p_instr_lda_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; A = M

                jmp     p_setflags
p_instr_lda_ab  endp

; >> LDX - Absolute
p_instr_ldx_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     [p_reg_x],al            ; X = M

                jmp     p_setflags
p_instr_ldx_ab  endp

; >> BCS
p_instr_bcs     proc
                call    p_get_byte

                call    p_get_carry             ; carry set?
                jnc     p_instr_bcs1            ; nope, visit p_instr_bcs1

                call    p_branch                ; branch!

p_instr_bcs1:   ret
p_instr_bcs     endp

; >> LDA - Indirect, Y
p_instr_lda_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; A = M

                jmp    p_setflags
p_instr_lda_iny endp

; >> LDY - Zero page, X
p_instr_ldy_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     [p_reg_y],al            ; Y = M

                jmp     p_setflags
p_instr_ldy_zpx endp

; >> LDA - Zero page, X
p_instr_lda_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; A = M

                jmp     p_setflags
p_instr_lda_zpx endp

; >> LDX - Zero page, Y
p_instr_ldx_zpy proc
                call    p_get_zeropagey         ; get memory address in si

                call    p_peek
                mov     [p_reg_x],al            ; X = M

                jmp     p_setflags
p_instr_ldx_zpy endp

; >> CLV
p_instr_clv     proc
                mov     al,[p_reg_flags]
                and     al,not FLAG_V
                mov     [p_reg_flags],al
                ret
p_instr_clv     endp

; >> LDA - Absolute, Y
p_instr_lda_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; A = M

                jmp     p_setflags
p_instr_lda_aby endp

; >> TSX
p_instr_tsx     proc
                mov     al,[p_reg_sp]
                mov     [p_reg_x],al            ; X = S

                jmp     p_setflags
p_instr_tsx     endp

; >> LDY - Absolute, X
p_instr_ldy_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     [p_reg_y],al            ; Y = M

                jmp     p_setflags
p_instr_ldy_abx endp

; >> LDA - Absolute, X
p_instr_lda_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     [p_reg_a],al            ; A = M

                jmp     p_setflags
p_instr_lda_abx endp

; >> LDX - Absolute, Y
p_instr_ldx_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     [p_reg_x],al            ; X = M

                jmp     p_setflags
p_instr_ldx_aby endp

; >> CPY - Immediate
p_instr_cpy_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al

                jmp     p_generic_cpy
p_instr_cpy_im  endp

; >> CMP - Indirect, X
p_instr_cmp_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_inx endp

; >> CPY - Zero Page
p_instr_cpy_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_cpy
p_instr_cpy_zp  endp

; >> CMP - Zero Page
p_instr_cmp_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_cmp
p_instr_cmp_zp  endp

; >> DEC - Zero Page
p_instr_dec_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                dec     al
                call    p_poke                  ; M = M - 1

                jmp     p_setflags
p_instr_dec_zp  endp

; >> INY
p_instr_iny     proc
                mov     al,[p_reg_y]
                inc     al
                mov     [p_reg_y],al            ; Y = Y + 1

                jmp     p_setflags              ; set flags
p_instr_iny     endp

; >> CMP - Immediate
p_instr_cmp_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_im  endp

; >> DEX
p_instr_dex     proc
                mov     al,[p_reg_x]
                dec     al
                mov     [p_reg_x],al            ; X = X - 1

                call    p_setflags
                ret
p_instr_dex     endp

; >> CPY - Absolute
p_instr_cpy_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_cpy
p_instr_cpy_ab  endp

; >> CMP - Absolute
p_instr_cmp_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_ab  endp

; >> DEC - Absolute
p_instr_dec_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                dec     al
                call    p_peek                  ; M = M - 1

                jmp     p_setflags
p_instr_dec_ab  endp

; >> BNE
p_instr_bne     proc
                call    p_get_byte

                mov     bl,[p_reg_flags]
                and     bl,FLAG_Z               ; Z flag clear?
                jnz     p_instr_bne1            ; nope, visit p_instr_bne1

                call    p_branch                ; branch!

p_instr_bne1:   ret
p_instr_bne     endp

; >> CMP - Indirect, Y
p_instr_cmp_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_iny endp

; >> CMP - Zero page, X
p_instr_cmp_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_zpx endp

; >> DEC - Zero page, X
p_instr_dec_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                dec     al
                call    p_poke                  ; M = M - 1

                jmp     p_setflags
p_instr_dec_zpx endp

; >> CLD
p_instr_cld     proc
                mov     al,[p_reg_flags]
                and     al,not FLAG_D
                mov     [p_reg_flags],al
                ret
p_instr_cld     endp

; >> CMP - Absolute, Y
p_instr_cmp_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_aby endp

; >> CMP - Absolute, X
p_instr_cmp_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     bl,al

                call    p_generic_cmp
                ret
p_instr_cmp_abx endp

; >> DEC - Absolute, X
p_instr_dec_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                dec     al
                call    p_poke                  ; M = M - 1

                jmp     p_setflags
p_instr_dec_abx endp

; >> CPX - Immediate
p_instr_cpx_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al
                
                jmp     p_generic_cpx
p_instr_cpx_im  endp

; >> SBC - Indirect, X
p_instr_sbc_inx proc
                call    p_get_indirectx         ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_inx endp

; >> CPX - Zero page
p_instr_cpx_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_cpx
p_instr_cpx_zp  endp

; >> SBC - Zero page
p_instr_sbc_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_zp  endp

; >> INC - Zero page
p_instr_inc_zp  proc
                call    p_get_zeropage          ; get memory address in si

                call    p_peek
                inc     al
                call    p_poke                  ; M = M + 1

                jmp     p_setflags
p_instr_inc_zp  endp

; >> INX
p_instr_inx     proc
                mov     al,[p_reg_x]
                inc     al
                mov     [p_reg_x],al            ; X = X + 1

                jmp     p_setflags              ; set flags
p_instr_inx     endp

; >> SBC - Immediate
p_instr_sbc_im  proc
                call    p_get_immediate         ; get immediate value
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_im  endp

; >> NOP
p_instr_nop     proc
                ret
p_instr_nop     endp

; >> CPX - Absolute
p_instr_cpx_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                jmp     p_generic_cpx
p_instr_cpx_ab  endp

; >> SBC - Absolute
p_instr_sbc_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_ab  endp

; >> INC - Absolute
p_instr_inc_ab  proc
                call    p_get_absolute          ; get memory address in si

                call    p_peek
                inc     al
                call    p_poke                  ; M = M + 1

                jmp     p_setflags
p_instr_inc_ab  endp

; >> BEQ
p_instr_beq     proc
                call    p_get_byte              ; al = jump value

                mov     bl,[p_reg_flags]
                and     bl,FLAG_Z               ; Z flag set?
                jz      p_instr_beq1            ; nope, visit p_instr_beq1

                call    p_branch                ; branch!

p_instr_beq1:   ret
p_instr_beq     endp

; >> SBC - Indirect, Y
p_instr_sbc_iny proc
                call    p_get_indirecty         ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_iny endp

; >> SBC - Zero page, X
p_instr_sbc_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_zpx endp

; >> INC - Zero page, X
p_instr_inc_zpx proc
                call    p_get_zeropagex         ; get memory address in si

                call    p_peek
                inc     al
                call    p_poke                  ; M = M + 1

                jmp     p_setflags
p_instr_inc_zpx endp

; >> SED
p_instr_sed     proc
                mov     al,[p_reg_flags]
                or      al,FLAG_D
                mov     [p_reg_flags],al
                ret
p_instr_sed     endp

; >> SBC - Absolute, Y
p_instr_sbc_aby proc
                call    p_get_absolutey         ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_aby endp

; >> SBC - Absolute, X
p_instr_sbc_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                mov     bl,al

                mov     al,[p_reg_a]
                call    p_generic_sbc
                mov     [p_reg_a],al
                ret
p_instr_sbc_abx endp

; >> INC - Absolute, X
p_instr_inc_abx proc
                call    p_get_absolutex         ; get memory address in si

                call    p_peek
                inc     al
                call    p_poke                  ; M = M + 1

                jmp     p_setflags
p_instr_inc_abx endp

end
