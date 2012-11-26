
BaseOfStack	equ	0100h
;--------------------------------------

org	0100h
	jmp	LABEL_START		; Start
	
	
	
LABEL_START:			
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, BaseOfStack

	mov	si, MsgNoLoader
	call	DisplayMessage

	jmp	$		; Start
	



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
	MsgNoLoader		db	"Here is Loader! :)", 0x0D, 0x0A, 0x00
