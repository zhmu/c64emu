;
; c64debug.asm
;
; this is the c64 debugger code
;
.model		small
.386
.data
extrn		p_reg_a:byte
extrn		p_reg_x:byte
extrn		p_reg_y:byte
extrn		p_reg_flags:byte
extrn		p_reg_pc:word
extrn		p_reg_sp:byte

extrn		in_keydown

d_videoseg	dw	0b000h

d_reg_a 	db	'A: $',0
d_reg_x 	db	'X: $',0
d_reg_y 	db	'Y: $',0
d_reg_flags	db	'F: ',0
d_reg_pc	db	'PC: $',0
d_reg_sp	db	'SP: $',0

d_lcase 	db	1

FLAG_N		equ	80h			; negative flag
FLAG_V		equ	40h			; overflow flasg
FLAG_B		equ	10h			; break flag
FLAG_D		equ	08h			; decimal flag
FLAG_I		equ	04h			; interrupt flag
FLAG_Z		equ	02h			; zero flag
FLAG_C		equ	01h			; carry flag

PARAM_NONE	equ	0			; no params
PARAM_IND_X	equ	1			; (indirect, x)
PARAM_ZP	equ	2			; zero page
PARAM_IMM	equ	3			; immediate
PARAM_ACC	equ	4			; accumulator
PARAM_ABS	equ	5			; absolute
PARAM_IND_Y	equ	6			; (indirect), y
PARAM_ZP_X	equ	7			; zero page, x
PARAM_ABS_Y	equ	8			; absolute, y
PARAM_ABS_X	equ	9			; absolute, x
PARAM_BRANCH	equ	0ah			; branch
PARAM_UNDEF	equ	0bh			; undefined opcode
PARAM_JUMP	equ	0ch			; jump
PARAM_IND	equ	0dh			; indirect
PARAM_ZP_Y	equ	0eh			; zero page, y

d_paramtab	dw	offset d_param_none
		dw	offset d_param_ind_x
		dw	offset d_param_zp
		dw	offset d_param_imm
		dw	offset d_param_acc
		dw	offset d_param_abs
		dw	offset d_param_ind_y
		dw	offset d_param_zp_x
		dw	offset d_param_abs_y
		dw	offset d_param_abs_x
		dw	offset d_param_branch
		dw	offset d_param_undef
		dw	offset d_param_jump
		dw	offset d_param_ind
		dw	offset d_param_zp_y

d_opcodetab	db	'BRK'                   ; 00
		db	PARAM_NONE
		db	'ORA'                   ; 01
		db	PARAM_IND_X
		db	'???'                   ; 02
		db	PARAM_UNDEF
		db	'???'                   ; 03
		db	PARAM_UNDEF
		db	'???'                   ; 04
		db	PARAM_UNDEF
		db	'ORA'                   ; 05
		db	PARAM_ZP
		db	'ASL'                   ; 06
		db	PARAM_ZP
		db	'???'                   ; 07
		db	PARAM_UNDEF
		db	'PHP'                   ; 08
		db	PARAM_NONE
		db	'ORA'                   ; 09
		db	PARAM_IMM
		db	'ASL'                   ; 0A
		db	PARAM_ACC
		db	'???'                   ; 0B
		db	PARAM_UNDEF
		db	'???'                   ; 0C
		db	PARAM_UNDEF
		db	'ORA'                   ; 0D
		db	PARAM_ABS
		db	'ASL'                   ; 0E
		db	PARAM_ABS
		db	'???'                   ; 0F
		db	PARAM_UNDEF
		db	'BPL'                   ; 10
		db	PARAM_BRANCH
		db	'ORA'                   ; 11
		db	PARAM_NONE
		db	'???'                   ; 12
		db	PARAM_UNDEF
		db	'???'                   ; 13
		db	PARAM_UNDEF
		db	'???'                   ; 14
		db	PARAM_UNDEF
		db	'ORA'                   ; 15
		db	PARAM_ZP_X
		db	'ASL'                   ; 16
		db	PARAM_ZP_X
		db	'???'                   ; 17
		db	PARAM_UNDEF
		db	'CLC'                   ; 18
		db	PARAM_NONE
		db	'ORA'                   ; 19
		db	PARAM_ABS_Y
		db	'???'                   ; 1A
		db	PARAM_UNDEF
		db	'???'                   ; 1B
		db	PARAM_UNDEF
		db	'???'                   ; 1C
		db	PARAM_UNDEF
		db	'ORA'                   ; 1D
		db	PARAM_ABS_X
		db	'ASL'                   ; 1E
		db	PARAM_ABS_X
		db	'???'                   ; 1F
		db	PARAM_UNDEF
		db	'JSR'                   ; 20
		db	PARAM_JUMP
		db	'AND'                   ; 21
		db	PARAM_IND_X
		db	'???'                   ; 22
		db	PARAM_UNDEF
		db	'???'                   ; 23
		db	PARAM_UNDEF
		db	'BIT'                   ; 24
		db	PARAM_ZP
		db	'AND'                   ; 25
		db	PARAM_ZP
		db	'ROL'                   ; 26
		db	PARAM_ZP
		db	'???'                   ; 27
		db	PARAM_UNDEF
		db	'PLP'                   ; 28
		db	PARAM_NONE
		db	'AND'                   ; 29
		db	PARAM_IMM
		db	'ROL'                   ; 2A
		db	PARAM_ACC
		db	'???'                   ; 2B
		db	PARAM_UNDEF
		db	'BIT'                   ; 2C
		db	PARAM_ABS
		db	'AND'                   ; 2D
		db	PARAM_ABS
		db	'ROL'                   ; 2E
		db	PARAM_ABS
		db	'???'                   ; 2F
		db	PARAM_UNDEF
		db	'BMI'                   ; 30
		db	PARAM_BRANCH
		db	'AND'                   ; 31
		db	PARAM_IND_Y
		db	'???'                   ; 32
		db	PARAM_UNDEF
		db	'???'                   ; 33
		db	PARAM_UNDEF
		db	'???'                   ; 34
		db	PARAM_UNDEF
		db	'AND'                   ; 35
		db	PARAM_ZP_X
		db	'ROL'                   ; 36
		db	PARAM_ZP_X
		db	'???'                   ; 37
		db	PARAM_UNDEF
		db	'SEC'                   ; 38
		db	PARAM_NONE
		db	'AND'                   ; 39
		db	PARAM_ABS_Y
		db	'???'                   ; 3A
		db	PARAM_UNDEF
		db	'???'                   ; 3B
		db	PARAM_UNDEF
		db	'???'                   ; 3C
		db	PARAM_UNDEF
		db	'AND'                   ; 3D
		db	PARAM_ABS_X
		db	'ROL'                   ; 3E
		db	PARAM_ABS_X
		db	'???'                   ; 3F
		db	PARAM_UNDEF
		db	'RTI'                   ; 40
		db	PARAM_NONE
		db	'EOR'                   ; 41
		db	PARAM_IND_X
		db	'???'                   ; 42
		db	PARAM_UNDEF
		db	'???'                   ; 43
		db	PARAM_UNDEF
		db	'???'                   ; 44
		db	PARAM_UNDEF
		db	'EOR'                   ; 45
		db	PARAM_ZP
		db	'LSR'                   ; 46
		db	PARAM_ZP
		db	'???'                   ; 47
		db	PARAM_UNDEF
		db	'PHA'                   ; 48
		db	PARAM_NONE
		db	'EOR'                   ; 49
		db	PARAM_IMM
		db	'LSR'                   ; 4A
		db	PARAM_ACC
		db	'???'                   ; 4B
		db	PARAM_UNDEF
		db	'JMP'                   ; 4C
		db	PARAM_ABS
		db	'EOR'                   ; 4D
		db	PARAM_ABS
		db	'LSR'                   ; 4E
		db	PARAM_ABS
		db	'???'                   ; 4F
		db	PARAM_UNDEF
		db	'BVC'                   ; 50
		db	PARAM_BRANCH
		db	'EOR'                   ; 51
		db	PARAM_IND_Y
		db	'???'                   ; 52
		db	PARAM_UNDEF
		db	'???'                   ; 53
		db	PARAM_UNDEF
		db	'???'                   ; 54
		db	PARAM_UNDEF
		db	'EOR'                   ; 55
		db	PARAM_ZP_X
		db	'LSR'                   ; 56
		db	PARAM_ZP_X
		db	'???'                   ; 57
		db	PARAM_UNDEF
		db	'CLI'                   ; 58
		db	PARAM_NONE
		db	'EOR'                   ; 59
		db	PARAM_ABS_Y
		db	'???'                   ; 5A
		db	PARAM_UNDEF
		db	'???'                   ; 5B
		db	PARAM_UNDEF
		db	'???'                   ; 5C
		db	PARAM_UNDEF
		db	'EOR'                   ; 5D
		db	PARAM_ABS_X
		db	'LSR'                   ; 5E
		db	PARAM_ABS_X
		db	'???'                   ; 5F
		db	PARAM_UNDEF
		db	'RTS'                   ; 60
		db	PARAM_NONE
		db	'ADC'                   ; 61
		db	PARAM_IND_X
		db	'???'                   ; 62
		db	PARAM_UNDEF
		db	'???'                   ; 63
		db	PARAM_UNDEF
		db	'???'                   ; 64
		db	PARAM_UNDEF
		db	'ADC'                   ; 65
		db	PARAM_ZP
		db	'ROR'                   ; 66
		db	PARAM_ZP
		db	'???'                   ; 67
		db	PARAM_UNDEF
		db	'PLA'                   ; 68
		db	PARAM_NONE
		db	'ADC'                   ; 69
		db	PARAM_IMM
		db	'ROR'                   ; 6A
		db	PARAM_ACC
		db	'???'                   ; 6B
		db	PARAM_UNDEF
		db	'JMP'                   ; 6C
		db	PARAM_IND
		db	'ADC'                   ; 6D
		db	PARAM_ABS
		db	'ROR'                   ; 6E
		db	PARAM_ABS
		db	'???'                   ; 6F
		db	PARAM_UNDEF
		db	'BVS'                   ; 70
		db	PARAM_BRANCH
		db	'ADC'                   ; 71
		db	PARAM_IND_Y
		db	'???'                   ; 72
		db	PARAM_UNDEF
		db	'???'                   ; 73
		db	PARAM_UNDEF
		db	'???'                   ; 74
		db	PARAM_UNDEF
		db	'ADC'                   ; 75
		db	PARAM_ZP_X
		db	'ROR'                   ; 76
		db	PARAM_ZP_X
		db	'???'                   ; 77
		db	PARAM_UNDEF
		db	'SEI'                   ; 78
		db	PARAM_NONE
		db	'ADC'                   ; 79
		db	PARAM_ABS_Y
		db	'???'                   ; 7A
		db	PARAM_UNDEF
		db	'???'                   ; 7B
		db	PARAM_UNDEF
		db	'???'                   ; 7C
		db	PARAM_UNDEF
		db	'ADC'                   ; 7D
		db	PARAM_ABS_Y
		db	'ROR'                   ; 7E
		db	PARAM_ABS_X
		db	'???'                   ; 7F
		db	PARAM_UNDEF
		db	'???'                   ; 80
		db	PARAM_UNDEF
		db	'STA'                   ; 81
		db	PARAM_IND_X
		db	'???'                   ; 82
		db	PARAM_UNDEF
		db	'???'                   ; 83
		db	PARAM_UNDEF
		db	'STY'                   ; 84
		db	PARAM_ZP
		db	'STA'                   ; 85
		db	PARAM_ZP
		db	'STX'                   ; 86
		db	PARAM_ZP
		db	'???'                   ; 87
		db	PARAM_UNDEF
		db	'DEY'                   ; 88
		db	PARAM_NONE
		db	'???'                   ; 89
		db	PARAM_UNDEF
		db	'TXA'                   ; 8A
		db	PARAM_NONE
		db	'???'                   ; 8B
		db	PARAM_UNDEF
		db	'STY'                   ; 8C
		db	PARAM_ABS
		db	'STA'                   ; 8D
		db	PARAM_ABS
		db	'STX'                   ; 8E
		db	PARAM_ABS
		db	'???'                   ; 8F
		db	PARAM_UNDEF
		db	'BCC'                   ; 90
		db	PARAM_BRANCH
		db	'STA'                   ; 91
		db	PARAM_IND_X
		db	'???'                   ; 92
		db	PARAM_UNDEF
		db	'???'                   ; 93
		db	PARAM_UNDEF
		db	'STY'                   ; 94
		db	PARAM_ZP_X
		db	'STA'                   ; 95
		db	PARAM_ZP_X
		db	'STX'                   ; 96
		db	PARAM_ZP_Y
		db	'???'                   ; 97
		db	PARAM_UNDEF
		db	'TYA'                   ; 98
		db	PARAM_NONE
		db	'STA'                   ; 99
		db	PARAM_ABS_Y
		db	'TXS'                   ; 9A
		db	PARAM_NONE
		db	'???'                   ; 9B
		db	PARAM_UNDEF
		db	'???'                   ; 9C
		db	PARAM_UNDEF
		db	'STA'                   ; 9D
		db	PARAM_ABS_X
		db	'???'                   ; 9E
		db	PARAM_UNDEF
		db	'???'                   ; 9F
		db	PARAM_UNDEF
		db	'LDY'                   ; A0
		db	PARAM_IMM
		db	'LDA'                   ; A1
		db	PARAM_IND_X
		db	'LDX'                   ; A2
		db	PARAM_IMM
		db	'???'                   ; A3
		db	PARAM_UNDEF
		db	'LDY'                   ; A4
		db	PARAM_ZP
		db	'LDA'                   ; A5
		db	PARAM_ZP
		db	'LDX'                   ; A6
		db	PARAM_ZP
		db	'???'                   ; A7
		db	PARAM_UNDEF
		db	'TAY'                   ; A8
		db	PARAM_NONE
		db	'LDA'                   ; A9
		db	PARAM_IMM
		db	'TAX'                   ; AA
		db	PARAM_NONE
		db	'???'                   ; AB
		db	PARAM_UNDEF
		db	'LDY'                   ; AC
		db	PARAM_ABS
		db	'LDA'                   ; AD
		db	PARAM_ABS
		db	'LDX'                   ; AE
		db	PARAM_ABS
		db	'???'                   ; AF
		db	PARAM_UNDEF
		db	'BCS'                   ; B0
		db	PARAM_BRANCH
		db	'LDA'                   ; B1
		db	PARAM_IND_Y
		db	'???'                   ; B2
		db	PARAM_UNDEF
		db	'???'                   ; B3
		db	PARAM_UNDEF
		db	'LDY'                   ; B4
		db	PARAM_ZP_X
		db	'LDA'                   ; B5
		db	PARAM_ZP_X
		db	'LDX'                   ; B6
		db	PARAM_ZP_Y
		db	'???'                   ; B7
		db	PARAM_UNDEF
		db	'CLV'                   ; B8
		db	PARAM_NONE
		db	'LDA'                   ; B9
		db	PARAM_ABS_Y
		db	'TSX'                   ; BA
		db	PARAM_NONE
		db	'???'                   ; BB
		db	PARAM_UNDEF
		db	'LDY'                   ; BC
		db	PARAM_ABS_X
		db	'LDA'                   ; BD
		db	PARAM_ABS_X
		db	'LDX'                   ; BE
		db	PARAM_ABS_Y
		db	'???'                   ; BF
		db	PARAM_UNDEF
		db	'CPY'                   ; C0
		db	PARAM_IMM
		db	'CMP'                   ; C1
		db	PARAM_IND_X
		db	'???'                   ; C2
		db	PARAM_UNDEF
		db	'???'                   ; C3
		db	PARAM_UNDEF
		db	'CPY'                   ; C4
		db	PARAM_ZP
		db	'CMP'                   ; C5
		db	PARAM_ZP
		db	'DEC'                   ; C6
		db	PARAM_ZP
		db	'???'                   ; C7
		db	PARAM_UNDEF
		db	'INY'                   ; C8
		db	PARAM_NONE
		db	'CMP'                   ; C9
		db	PARAM_IMM
		db	'DEX'                   ; CA
		db	PARAM_NONE
		db	'???'                   ; CB
		db	PARAM_UNDEF
		db	'CPY'                   ; CC
		db	PARAM_ABS
		db	'CMP'                   ; CD
		db	PARAM_ABS
		db	'DEC'                   ; CE
		db	PARAM_ABS
		db	'???'                   ; CF
		db	PARAM_UNDEF
		db	'BNE'                   ; D0
		db	PARAM_BRANCH
		db	'CMP'                   ; D1
		db	PARAM_IND_Y
		db	'???'                   ; D2
		db	PARAM_UNDEF
		db	'???'                   ; D3
		db	PARAM_UNDEF
		db	'???'                   ; D4
		db	PARAM_UNDEF
		db	'CMP'                   ; D5
		db	PARAM_ZP_X
		db	'DEC'                   ; D6
		db	PARAM_ZP_X
		db	'???'                   ; D7
		db	PARAM_UNDEF
		db	'CLD'                   ; D8
		db	PARAM_NONE
		db	'CMP'                   ; D9
		db	PARAM_ABS_Y
		db	'???'                   ; DA
		db	PARAM_UNDEF
		db	'???'                   ; DB
		db	PARAM_UNDEF
		db	'???'                   ; DC
		db	PARAM_UNDEF
		db	'CMP'                   ; DD
		db	PARAM_ABS_X
		db	'DEC'                   ; DE
		db	PARAM_ABS_X
		db	'???'                   ; DF
		db	PARAM_UNDEF
		db	'CPX'                   ; E0
		db	PARAM_IMM
		db	'SBC'                   ; E1
		db	PARAM_IND_X
		db	'???'                   ; E2
		db	PARAM_UNDEF
		db	'???'                   ; E3
		db	PARAM_UNDEF
		db	'CPX'                   ; E4
		db	PARAM_ZP
		db	'SBC'                   ; E5
		db	PARAM_ZP
		db	'INC'                   ; E6
		db	PARAM_ZP
		db	'???'                   ; E7
		db	PARAM_UNDEF
		db	'INX'                   ; E8
		db	PARAM_NONE
		db	'SBC'                   ; E9
		db	PARAM_IMM
		db	'NOP'                   ; EA
		db	PARAM_NONE
		db	'???'                   ; EB
		db	PARAM_UNDEF
		db	'CPX'                   ; EC
		db	PARAM_ABS
		db	'SBC'                   ; ED
		db	PARAM_ABS
		db	'INC'                   ; EE
		db	PARAM_ABS
		db	'???'                   ; EF
		db	PARAM_UNDEF
		db	'BEQ'                   ; F0
		db	PARAM_BRANCH
		db	'SBC'                   ; F1
		db	PARAM_IND_Y
		db	'???'                   ; F2
		db	PARAM_UNDEF
		db	'???'                   ; F3
		db	PARAM_UNDEF
		db	'???'                   ; F4
		db	PARAM_UNDEF
		db	'SBC'                   ; F5
		db	PARAM_ZP_X
		db	'INC'                   ; F6
		db	PARAM_ZP_X
		db	'???'                   ; F7
		db	PARAM_UNDEF
		db	'SED'                   ; F8
		db	PARAM_NONE
		db	'SBC'                   ; F9
		db	PARAM_ABS_Y
		db	'???'                   ; FA
		db	PARAM_UNDEF
		db	'???'                   ; FB
		db	PARAM_UNDEF
		db	'???'                   ; FC
		db	PARAM_UNDEF
		db	'SBC'                   ; FD
		db	PARAM_ABS_X
		db	'INC'                   ; FE
		db	PARAM_ABS_X
		db	'???'                   ; FF
		db	PARAM_UNDEF

d_disasm_base   dw      0
d_disasm_sel    dw      0

.code
public          d_init,d_done,d_invoke,d_disasm_base

extrn		p_run:proc
extrn           in_waitrelease:proc

;
; d_init
;
; this will initialize the c64 debugger
;
d_init		proc
		mov	ah,0fh			; video: get current video mode
		int	10h

		cmp	al,3			; colour?
		jne	d_init1 		; no, visit d_init1

		add	[d_videoseg],800h	; yup. set [d_videoseg] to the
						; correct value

d_init1:	push	[d_videoseg]
		pop	gs			; gs = videoseg

		mov	ax,[p_reg_pc]
		mov	[d_disasm_base],ax
		ret
d_init		endp

;
; d_box
;
; this will draw a box from (ch,cl) to (bh,bl) in color (dl). assumes gs is the
; segment of the colour ram
;
; destroys: ax, bx, cx, di, bp
;
d_box		proc
		push	dx es gs
		pop	es			; es = gs

		mov	dh,bh
		sub	dh,ch			; dh = box width

		mov	ax,80
		mul	ch			; ax = ch * 80
		xor	ch,ch
		add	ax,cx			; ax = ch * 80 + x
		shl	ax,1
		mov	di,ax			; di = ch * 160 + 2 * x

		mov	ah,dl			; set color

		dec	dh
		movzx	dx,dh			; dx = (word)width

		push	bx
		sub	bl,cl
		xor	bh,bh
		mov	cx,bx			; cx = (word)height
		pop	bx

		push	dx
		shl	dx,1
		mov	bp,158
		sub	bp,dx			; bp = bytes to skip after a line
		pop	dx

		mov	al,0c9h
		stosw				; draw top notch (É)

		push	cx
		mov	cx,dx
		dec	cx
		mov	al,0cdh 		; draw line (Í)
		rep	stosw
		pop	cx

		mov	al,0bbh
		stosw

		dec	cx
		jz	d_box2

d_box1: 	add	di,bp
		mov	al,0bah
		stosw

		push	cx dx
		pop	cx
		dec	cx
		mov	al,20h
		rep	stosw
		pop	cx

		mov	al,0bah
		stosw

		loop	d_box1

d_box2: 	add	di,bp

		mov	al,0c8h
		stosw				; draw top notch (É)

		mov	cx,dx
		dec	cx
		mov	al,0cdh 		; draw line (Í)
		rep	stosw

		mov	al,0bch
		stosw

		pop	es dx
		ret
d_box		endp

;
; d_text: this will show the asciiz string at ds:[si] at (ch,cl) in color (dl).
;
; destroys: ax, cx, si, di
;
d_text		proc
		push	es gs
		pop	es			; es = gs

		mov	ax,80
		mul	ch			; ax = ch * 80
		xor	ch,ch
		add	ax,cx			; ax = ch * 80 + x
		shl	ax,1
		mov	di,ax			; di = ch * 160 + 2 * x

		mov	ah,dl			; set color

d_text1:	lodsb				; al = next char
		or	al,al			; nul?
		jz	d_text2 		; yup, get outta here

		stosw
		jmp	d_text1

d_text2:	pop	es
		ret
d_text		endp

;
; d_int2hasc: this will convert integer [al] to hex ascii (range 0-f)
;
d_int2hasc	proc
		cmp	al,9			; >9?
		jg	d_int2hasc1		; yup, use chars

		add	al,48
		ret

d_int2hasc1:	add	al,55
		ret
d_int2hasc	endp

;
; d_hexx
;
; this will show the byte (dh) at es:[di] in color (ah).
;
d_hexx		proc
		mov	bh,dh
		mov	bl,dh
		shr	bh,4
		and	bl,0fh

		mov	al,bh
		call	d_int2hasc
		stosw

		mov	al,bl
		call	d_int2hasc
		stosw
		ret
d_hexx		endp

;
; d_hexb
;
; this will show the byte (dh) in hex at (ch,cl) in color (dl).
;
d_hexb		proc
		push	es gs
		pop	es			; es = gs

		mov	ax,80
		mul	ch			; ax = ch * 80
		xor	ch,ch
		add	ax,cx			; ax = ch * 80 + x
		shl	ax,1
		mov	di,ax			; di = ch * 160 + 2 * x

		mov	ah,dl			; set color

		call	d_hexx

		pop	es
		ret
d_hexb		endp

;
; d_hexw
;
; this will show word (bx) in hex at (ch,cl) in color (dl)
;
d_hexw		proc
		push	bx cx
		mov	dh,bh
		call	d_hexb
		pop	cx bx

		add	cl,2

		mov	dh,bl
		jmp	d_hexb
d_hexw		endp

;
; d_done
;
; this will deinitialize the c64 debugger
;
d_done		proc
		ret
d_done		endp

;
; d_showregs
;
; this will show the register contents
;
d_showregs	proc
		push	es gs
		pop	es

		mov	cx,0243h
		mov	dh,[p_reg_a]
		call	d_hexb

		mov	cx,0343h
		mov	dh,[p_reg_x]
		call	d_hexb

		mov	cx,0443h
		mov	dh,[p_reg_y]
		call	d_hexb

		mov	di,(05h*160)+(42h*2)

		mov	al,'N'

		mov	bl,[p_reg_flags]
		push	bx
		and	bl,FLAG_N
		pop	bx
		jnz	d_showregs1

		or	al,20h

d_showregs1:	stosw

		mov	al,'V'

		push	bx
		and	bl,FLAG_V
		pop	bx
		jnz	d_showregs2

		or	al,20h

d_showregs2:	stosw

		mov	al,'-'
		stosw

		mov	al,'B'

		push	bx
		and	bl,FLAG_B
		pop	bx
		jnz	d_showregs3

		or	al,20h

d_showregs3:	stosw

		mov	al,'D'

		push	bx
		and	bl,FLAG_D
		pop	bx
		jnz	d_showregs4

		or	al,20h

d_showregs4:	stosw

		mov	al,'I'

		push	bx
		and	bl,FLAG_I
		pop	bx
		jnz	d_showregs5

		or	al,20h

d_showregs5:	stosw

		mov	al,'Z'

		push	bx
		and	bl,FLAG_Z
		pop	bx
		jnz	d_showregs6

		or	al,20h

d_showregs6:	stosw

		mov	al,'C'

		push	bx
		and	bl,FLAG_C
		pop	bx
		jnz	d_showregs7

		or	al,20h

d_showregs7:	stosw

		mov	cx,0643h
		mov	bx,[p_reg_pc]
		call	d_hexw

		mov	cx,0743h
		mov	dh,[p_reg_sp]
		call	d_hexb

		pop	es
		ret
d_showregs	endp

;
; d_param_none
;
; will handle no parameters
;
d_param_none	proc
		ret
d_param_none	endp

;
; d_param_ind_x
;
; will handle (indirect, x) parameters
;
d_param_ind_x   proc
                mov     al,'('
                stosw
                mov     al,'$'
		stosw

                mov     dh,byte ptr fs:[si]
                inc     si

		call	d_hexx

                mov     al,','
                stosw

                mov     al,'x'
                stosw

                mov     al,')'
                stosw

                ret
d_param_ind_x	endp

;
; d_param_zp
;
; will handle $zeropage parameters.
;
d_param_zp	proc
                mov     al,'$'
                stosw

                mov     dh,fs:[si]
                inc     si

                jmp     d_hexx
d_param_zp	endp

;
; d_param_imm
;
; will handle #$immediate parameters.
;
d_param_imm	proc
		mov	al,'#'
		stosw
                jmp     d_param_zp
d_param_imm	endp

;
; d_param_acc
;
; will handle Accumulator parameter.
;
d_param_acc	proc
		mov	al,'a'
		stosw
		ret
d_param_acc	endp

;
; d_param_abs
;
; will handle absolute parameters.
;
d_param_abs	proc
		jmp	d_param_jump
d_param_abs	endp

;
; d_param_zp_x
;
; will handle $zeropage, x parameters.
;
d_param_zp_x	proc
                call    d_param_zp

                mov     al,','
                stosw

                mov     al,'x'
                stosw
		ret
d_param_zp_x	endp

;
; d_param_abs_y
;
; will handle absolute, y parameters.
;
d_param_abs_y	proc
		call	d_param_abs

		mov	al,','
		stosw
		mov	al,'y'
		stosw
		ret
d_param_abs_y	endp

;
; d_param_abs_x
;
; will handle absolute, x parameters.
;
d_param_abs_x	proc
		call	d_param_abs

		mov	al,','
		stosw
		mov	al,'x'
		stosw
		ret
d_param_abs_x	endp

;
; d_param_branch
;
; will handle branch parameters.
;
d_param_branch	proc
		push	cx
		mov	cx,si

		movzx	bx,fs:[si]
		inc	si

		push	bx
		sub	bl,7fh
		pop	bx
		jnc	d_param_branch1

		add	cl,bl
		adc	ch,0
		inc	cx

		jmp	d_param_branch2

d_param_branch1:not	bl

		sub	cl,bl
		sbb	ch,0

d_param_branch2:mov	al,'$'
		stosw

		mov	dh,ch
		call	d_hexx
		mov	dh,cl
		call	d_hexx

		pop	cx
		ret
d_param_branch	endp

;
; d_param_undef
;
; will handle undefined parameters.
;
d_param_undef	proc
		ret
d_param_undef	endp

;
; d_param_jump
;
; will handle jump parameters.
;
d_param_jump	proc
		mov	al,'$'
		stosw

		push	dx
		mov	dx,word ptr fs:[si]
		add	si,2
		call	d_hexx
		mov	dh,dl
		call	d_hexx
		pop	dx

		ret
d_param_jump	endp

;
; d_param_ind
;
; will handle (indirect) parameters.
;
d_param_ind	proc
		mov	al,'('
		stosw
		mov	al,'$'
		stosw

		push	dx
		mov	dx,word ptr fs:[si]
		add	si,2
		call	d_hexx
		mov	dh,dl
		call	d_hexx
		pop	dx

		mov	al,')'
		stosw
		ret
d_param_ind	endp

;
; d_param_ind_y
;
; will handle (indirect), y parameters.
;
d_param_ind_y	proc
                mov     al,'('
		stosw
		mov	al,'$'
		stosw

                mov     dh,byte ptr fs:[si]
                inc     si
                call    d_hexx

                mov     al,')'
                stosw

		mov	al,','
		stosw

		mov	al,'y'
		stosw
		ret
d_param_ind_y	endp


;
; d_param_zp_y
;
; will handle $zeropage, y parameters.
;
d_param_zp_y	proc
                call    d_param_zp

                mov     al,','
                stosw

                mov     al,'x'
                stosw
		ret
d_param_zp_y	endp

;
; d_disasm
;
; this will disassemble the code at fs:[si] and print it at (ch, cl) in colour
; (dl). [cx] will be the number of bytes needed to disassemble. it assumes
; es = 0b000h or 0b800h, depending whether the user has mono/colour graphics.
;
d_disasm	proc
		push	bp si
		mov	bp,si

		mov	ax,80
		mul	ch			; ax = ch * 80
		xor	ch,ch
		add	ax,cx			; ax = ch * 80 + x
		shl	ax,1
		mov	di,ax			; di = ch * 160 + 2 * x

		movzx	si,byte ptr fs:[si]
		shl	si,2			; si = (byte) * 4
		add	si,offset d_opcodetab

		mov	ah,dl

		lodsb
		cmp	[d_lcase],0
		jz	d_disasm1

		or	al,20h

d_disasm1:	stosw
		lodsb
		cmp	[d_lcase],0
		jz	d_disasm2

		or	al,20h

d_disasm2:	stosw
		lodsb
		cmp	[d_lcase],0
		jz	d_disasm3

		or	al,20h

d_disasm3:	stosw

		mov	cx,4
		mov	al,20h
		rep	stosw

		push	ax
		lodsb

		xor	ah,ah			; ax = (word)parameter type
		shl	ax,1
		add	ax,offset d_paramtab
		mov	bx,ax
		pop	ax

		pop	si
		inc	si

		call	[bx]			; handle the parameters

		sub	si,bp
		mov	cx,si
		pop	bp
		ret
d_disasm	endp

;
; d_showdisasm
;
; this will show the disassembly.
;
d_showdisasm	proc
		push	es gs
		pop	es

		mov	cx,0203h
		mov	bx,[d_disasm_base]
		mov	bp,21

a_showdisasm1:  mov     dl,03fh
		cmp	bx,[p_reg_pc]		; is this our current line?
		jne	a_showdisasm2		; no. visit a_showdisasm2

		mov	dl,04eh

a_showdisasm2:	push	cx
		mov	ax,80
		mul	ch			; ax = ch * 80
		xor	ch,ch
		add	ax,2
		shl	ax,1
		mov	di,ax			; di = ch * 160

		mov	cx,60
		mov	ah,dl
		mov	al,20h
		rep	stosw
		pop	cx

		push	bx cx
		call	d_hexw
		pop	cx bx

		push	bx cx

		mov	al,':'
		stosw

		mov	cl,13h
		mov	si,bx
		mov	di,si
		push	di
		call	d_disasm
		pop	di

		pop	ax bx
		push	cx
		add	bx,cx
		mov	cx,ax
		pop	ax

		push	cx
		mov	cl,9

a_showdisasm3:	push	ax bx cx di
		mov	dh,fs:[di]
		call	d_hexb
		pop	di cx bx ax

		add	cl,3
		inc	di

		dec	ax
		jnz	a_showdisasm3
		pop	cx

		inc	ch

		dec	bp
		jnz	a_showdisasm1

		pop	es
		ret
d_showdisasm	endp

;
; d_invoke
;
; this will invoke the c64 debugger
;
d_invoke	proc
		mov	cx,0101h
		mov	bx,4e17h		; bx = (80,24)
                mov     dl,03fh
		call	d_box

		mov	si,offset d_reg_a
		mov	cx,023fh
		call	d_text

		mov	si,offset d_reg_x
		mov	cx,033fh
		call	d_text

		mov	si,offset d_reg_y
		mov	cx,043fh
		call	d_text

		mov	si,offset d_reg_flags
		mov	cx,053fh
		call	d_text

		mov	si,offset d_reg_pc
		mov	cx,063eh
		call	d_text

		mov	si,offset d_reg_sp
		mov	cx,073eh
		call	d_text

		call	d_showregs

		call	d_showdisasm

d_invoke1:      cmp     byte ptr in_keydown[66],0      ; f8
		jz	d_invoke2

                mov     bl,66
                call    in_waitrelease

                push    bx
		call	p_run
                pop     bx
                dec     bx

                mov     ax,[p_reg_pc]
                cmp     ax,bx                 ; pc > what we can see?
                jle     d_invoke4             ; no, it's ok

                mov     [d_disasm_base],ax

d_invoke4:      mov     bx,[d_disasm_base]
                cmp     ax,bx
                jge     d_invoke5

                mov     [d_disasm_base],ax

d_invoke5:      call    d_showdisasm
                push    bx

                mov     dl,03fh
		call	d_showregs
                pop     bx

d_invoke2:	cmp	in_keydown[1],0		; escape
		jz	d_invoke1

		ret
d_invoke	endp

end
