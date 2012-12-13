;kernel.asm
;;;;;;;;;;[section .text]	;

;;;;;;;;;;global _start	;export _start

;;;;;;;;;;_start:	;gs -> video ram

org 0
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	
	mov	si, MsgKernel
	call	DisplayMessage	
	jmp	$


;---------------------------------------
MsgKernel		db	"Here is Kernel!!! :)", 0x0D, 0x0A, 0x00
;---------------------------------------
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
	
;---------------------------------------
