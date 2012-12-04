BaseOfStack	equ	0100h
;--------------------------------------

%include	"pm.inc"	;

org	0100h
	jmp	LABEL_START
;-------------------------------------------------
[SECTION .gdt]
; GDT
;                                base,            limit,         attr
LABEL_GDT:	   Descriptor       0,                0, 0           	; empty descriptor
LABEL_DESC_CODE32: Descriptor       0, SegCode32Len - 1, DA_C + DA_32	; 
LABEL_DESC_VIDEO:  Descriptor 0B8000h,           0ffffh, DA_DRW	     	; 
; GDT end	
;-------------------------------------------------	
	GdtLen		equ	$ - LABEL_GDT	; GDT lenght
	GdtPtr		dw	GdtLen - 1	; GDT limit
			dd	0		; GDT base
;-------------------------------------------------
	; GDT selector
	SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
	SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT
;-------------------------------------------------

LABEL_START:			
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, BaseOfStack           
	
	mov	si, MsgLoader 
	call	DisplayMessage  	
	
	; Initialize 32-bits code segment descriptor
	xor	eax, eax                        
	mov	ax, cs                          
	shl	eax, 4                          	
	add	eax, LABEL_SEG_CODE32           	
	mov	word [LABEL_DESC_CODE32 + 2], ax
	shr	eax, 16                         	
	mov	byte [LABEL_DESC_CODE32 + 4], al	
	mov	byte [LABEL_DESC_CODE32 + 7], ah

	; Prepared for loading GDTR
	xor	eax, eax
	mov	ax, ds
	shl	eax, 4
	add	eax, LABEL_GDT		; eax <- gdt base
	mov	dword [GdtPtr + 2], eax	; [GdtPtr + 2] <- gdt base
	
	; Load GDTR(Global Descriptor Table Register)
	lgdt	[GdtPtr]
	
	; Clear Interrupt Flags
	cli
	
	; open A20 line
	in	al, 0x92
	or	al, 00000010b
	out	92h, al
	
	; Enable protect mode, PE bit of CR0
	mov	eax, cr0
	or	eax, 1
	mov	cr0, eax
	
	; jump to 32bit mode
	jmp	dword SelectorCode32:0

;------------------------------------------------------
DisplayMessage:
	lodsb			; load next character
	or	al, al		; test for NUL character
	jz	.DONE
	mov	ah, 0x0E	; BIOS teletype
	mov	bh, 0x00	; display page 0
	mov	bl, 0x07	; text attribute
	int	0x10		; invoke BIOS
	jmp	DisplayMessage
.DONE:
	ret
;------------------------------------------------------

;------------------------------------------------------------------	
	MsgLoader	db	"Here is Loader! :)", 0x0D, 0x0A, 0x00
	MsgProtect	db	"HERE is protect mode :)", 0x0D, 0x0A, 0x00
;------------------------------------------------------------------

[SECTION .s32]; 32 bit code
[BITS	32]

LABEL_SEG_CODE32:
	mov	ax, SelectorVideo
	mov	gs, ax			; Video segment selector(dest)

	mov	edi, (80 * 11 + 0) * 2	; 11:0
	mov	ah, 0Ch			; 0000:black back    1100: red front
	mov	al, 'P'
	mov	[gs:edi], ax
	jmp	$
	mov	esi, MsgProtect
	call	DisplayMessage

	; Stop here, infinite loop
	jmp	$

SegCode32Len	equ	$ - LABEL_SEG_CODE32
