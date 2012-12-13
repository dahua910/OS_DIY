BaseOfStack	equ	0100h
BaseOfKernelFile	equ	 08000h	; 
OffsetOfKernelFile	equ	     0h	;
;--------------------------------------

%include	"pm.inc"	;
%include	"boot.inc"

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
	
;floppy reset
	xor	ah, ah	; 
	xor	dl, dl	; 
	int	0x13	; 

	mov	word [wSectorNo], SectorNoOfRootDirectory
	
; Find Kernel.BIN in root directory of driver 0
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0	; If searching in root dir can find Kernel.BIN
	jz	LABEL_NO_KERNELBIN		
	dec	word [wRootDirSizeForLoop]	;
	mov	ax, BaseOfKernelFile
	mov	es, ax			; es <- BaseOfKernel
	mov	bx, OffsetOfKernelFile	; bx <- OffsetOfKernel
	mov	ax, [wSectorNo]		; ax <- sector_num in Root Directory  
	mov	cl, 1
	call	ReadSector

	mov	si, KernelFileName	; ds:si -> "Kernel  BIN"
	mov	di, OffsetOfKernelFile	; es:di -> BaseOfKernel:0100
	cld
	mov	dx, 0x10
LABEL_SEARCH_FOR_KERNELBIN:
	cmp	dx, 0				   ; loop time
	jz	LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR ; if a sector read is done
	dec	dx				   ; next sector
	mov	cx, 11
LABEL_CMP_FILENAME:
	cmp	cx, 0
	jz	LABEL_FILENAME_FOUND	; If 11 chars are all identical
	dec	cx
	lodsb				; ds:si -> al
	cmp	al, byte [es:di]
	jz	LABEL_GO_ON
	jmp	LABEL_DIFFERENT		; different

LABEL_GO_ON:
	inc	di
	jmp	LABEL_CMP_FILENAME	; go on loop

LABEL_DIFFERENT:
	and	di, 0xFFE0		; Go to head of this entry 
	add	di, 0x20		;
	mov	si, KernelFileName	; next entry, di += 20h  
	jmp	LABEL_SEARCH_FOR_KERNELBIN; 

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:
	add	word [wSectorNo], 1
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_KERNELBIN:
	mov	si, MsgNoKernel
	call	DisplayMessage		; display string
	jmp	$			; no Kernel.BIN, infinite loop

LABEL_FILENAME_FOUND:			; Kernel.BIN was found
	mov	ax, RootDirSectors
	and	di, 0xFFF0		; Start of current entry, 32 bytes per entry

	push	eax
	mov	eax, [es : di + 01Ch]		; 
	mov	dword [dwKernelSize], eax	;  save the size of kernel.bin
	pop	eax

	
	add	di, 0x1A		; First sector of this file
	mov	cx, word [es:di]
	push	cx			; Save index of this sector in FAT
	add	cx, ax
	add	cx, DeltaSectorNo	; Kernel.BIN's start sector saved in cl
	mov	ax, BaseOfKernelFile
	mov	es, ax			; es <- BaseOfKernel
	mov	bx, OffsetOfKernelFile	; bx <- OffsetOfKernel	es:bx = BaseOfKernel:OffsetOfKernel = BaseOfKernel * 10h + OffsetOfKernel
	mov	ax, cx			; ax <- Sector number

LABEL_GOON_LOADING_FILE:
	push	ax			; 
	push	bx			; 
	mov	ah, 0xE			;  
	mov	al, '.'			; 
	mov	bl, 0xF			;  Booting ......
	int	0x10			; 
	pop	bx			; 
	pop	ax			; 

	mov	cl, 1
	call	ReadSector
	pop	ax			; Got index of this sector in FAT
	call	GetFATEntry
	cmp	ax, 0xFFF
	jz	LABEL_FILE_LOADED
	push	ax			; Save index of this sector in FAT
	mov	dx, RootDirSectors
	add	ax, dx
	add	ax, DeltaSectorNo
	add	bx, [BPB_BytsPerSec]
	jmp	LABEL_GOON_LOADING_FILE
LABEL_FILE_LOADED:

	mov	si, MsgReady
	call	DisplayMessage			

; *****************************************************************************************************
	jmp	BaseOfKernelFile:OffsetOfKernelFile	; Jump to Kernel.BIN's start address in memory.

; *****************************************************************************************************




	
;-------------------------------------------------------	
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
ReadSector:
	push	bp
	mov	bp, sp
	sub	esp, 2			; Reserve space for saving: byte [bp-2]

	mov	byte [bp-2], cl
	push	bx			; save bx
	mov	bl, [BPB_SecPerTrk]	; bl: divider
	div	bl			; al=y, ah=z
	inc	ah			; z ++
	mov	cl, ah			; cl <- start sector_num
	mov	dh, al			; dh <- y
	shr	al, 1			; y >> 1 (y/BPB_NumHeads)
	mov	ch, al			; ch <- Cylinder number(y>>1)
	and	dh, 1			; dh & 1 = Magnetic header(y&1)
	pop	bx			; restore bx
	;Now, we got cylinder number in ch, start sector number in cl, magnetic header in dh 
	mov	dl, [BS_DrvNum]		; driver 0
.GoOnReading:
	mov	ah, 2			; read
	mov	al, byte [bp-2]		; read al sector
	int	13h
	jc	.GoOnReading		; If CF set 1, mean read error, reread

	add	esp, 2
	pop	bp

	ret

;--------------------------------------------------------
;Routine: GetFATEntry Action: Find ax sector's index in FAT, save result in ax
GetFATEntry:
	push	es
	push	bx
	push	ax
	mov	ax, BaseOfKernelFile	; 
	sub	ax, 0x100		;  Left 4K bytes for FAT after BaseOfLoader
	mov	es, ax			; 
	pop	ax
	mov	byte [bOdd], 0
	mov	bx, 3
	mul	bx			; dx:ax = ax * 3
	mov	bx, 2
	div	bx			; dx:ax / 2  
	cmp	dx, 0
	jz	LABEL_EVEN
	mov	byte [bOdd], 1
LABEL_EVEN:
	xor	dx, dx			; now ax is the offset of FATEntry in FAT
	mov	bx, [BPB_BytsPerSec]
	div	bx			; dx:ax / BPB_BytsPerSec 

	push	dx
	mov	bx, 0			; bx <- 0	, es:bx = (BaseOfLoader - 100):00 = (BaseOfLoader - 100) * 10h
	add	ax, SectorNoOfFAT1	; ax <- FATEntry's sector
	mov	cl, 2
	call	ReadSector		; Read 2 sectors in 1 time, because FATEntry
	pop	dx
	add	bx, dx
	mov	ax, [es:bx]
	cmp	byte [bOdd], 1
	jnz	LABEL_EVEN_2
	shr	ax, 4
LABEL_EVEN_2:
	and	ax, 0xFFF

LABEL_GET_FAT_ENRY_OK:

	pop	bx
	pop	es
	ret

;--------------------------------------------------------
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
;--------------------------------------------------------
BPB_BytsPerSec:     	DW	512             ; Bytes per sector 
BPB_SecPerTrk:      	DW	18              ; Sector per track 
BS_DrvNum:          	DB	0               ; Driver number of interrupt 13 

wRootDirSizeForLoop	dw	RootDirSectors	; Root Directory number, dec to 0
wSectorNo		dw	0		; sector number to read
bOdd			db	0		; odd or even
dwKernelSize		dd	0		; KERNEL.BIN file size
;------------------------------------------------------
KernelFileName	db	"KERNEL  BIN", 0	;
MsgLoader	db	"Here is Loader! :)", 0x0D, 0x0A, 0x00
MsgProtect	db	"HERE is protect mode :)", 0x0D, 0x0A, 0x00
MsgNoKernel	db	"No Kernel.bin!!!  :(", 0x0D, 0x0A, 0x00
MsgReady		db	"Ready! ", 0x0D, 0x0A, 0x00
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
