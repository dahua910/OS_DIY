	Base0fStack		equ	0x7c00
	BaseOfLoader		equ	0x9000
	OffsetOfLoader		equ	0x100
	RootDirSectors		equ	14
	DeltaSectorNo		equ	17	; DeltaSectorNo = BPB_RsvdSecCnt + (BPB_NumFATs * FATSz) - 2
	SectorNoOfFAT1		equ	1	; first sector of FAT1= BPB_RsvdSecCnt
		
	SectorNoOfRootDirectory	equ	19	; first sector of root dir
;------------------------------------------------------

[BITS 16]
ORG 0x7c00
	jmp short LABEL_START
	nop					; nop required 

	BS_OEMName:         DB	'dongkang'      ; OEM String, 8 bytes required 
	BPB_BytsPerSec:     DW	512             ; Bytes per sector 
	BPB_SecPerCluster:  DB	1               ; Sector per cluster 
	BPB_ResvdSecCnt:    DW	1               ; Reserved sector count 
	BPB_NumFATs:        DB	2               ; Number of FATs 
	BPB_RootEntCnt:     DW	224             ; Root entries count 
	BPB_TotSec16:       DW	2880            ; Total sector number 
	BPB_Media:          DB	0xf0            ; Media descriptor 
	BPB_FATSz16:        DW	9               ; FAT size(sectors) 
	BPB_SecPerTrk:      DW	18              ; Sector per track 
	BPB_NumHeads:       DW	2               ; Number of magnetic heads 
	BPB_HiddSec:        DD	0               ; Number of hidden sectors 
	BPB_TotSec32:       DD	0               ; If TotSec16 equal 0, this works 
	BS_DrvNum:          DB	0               ; Driver number of interrupt 13 
	BS_Reserved1:       DB	0               ; Reserved 
	BS_BootSig:         DB	0x29            ; Boot signal 
	BS_VolID:           DD	0               ; Volume ID 
	BS_VolLab:          DB	'dongkangni '   ; Volume label, 11 bytes required 
	BS_FileSysType:     DB	'FAT12   '      ; File system type, 8 bytes required 

LABEL_START:
; code located at 0x0000:7C00, adjust segment registers
	cli
	mov	ax, cs		;cs=0x7c00
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, Base0fStack
	sti
	mov	ax, 0x0002	; set display mode ; 640x200, 16 color, 80x25, 8 pages
	int	0x10		; invoke BIOS
	
	
; post message
	mov	si, MsgHello
	call	DisplayMessage	
	
	mov	si, MsgBooting
	call	DisplayMessage

;floppy reset
	xor	ah, ah	; 
	xor	dl, dl	; 
	int	0x13	; 

	mov	word [wSectorNo], SectorNoOfRootDirectory
	
; Find LOADER.BIN in root directory of driver 0
LABEL_SEARCH_IN_ROOT_DIR_BEGIN:
	cmp	word [wRootDirSizeForLoop], 0	; If searching in root dir can find LOADER.BIN
	jz	LABEL_NO_LOADERBIN		
	dec	word [wRootDirSizeForLoop]	;
	mov	ax, BaseOfLoader
	mov	es, ax			; es <- BaseOfLoader
	mov	bx, OffsetOfLoader	; bx <- OffsetOfLoader
	mov	ax, [wSectorNo]		; ax <- sector_num in Root Directory  
	mov	cl, 1
	call	ReadSector

	mov	si, LoaderFileName	; ds:si -> "LOADER  BIN"
	mov	di, OffsetOfLoader	; es:di -> BaseOfLoader:0100
	cld
	mov	dx, 0x10
LABEL_SEARCH_FOR_LOADERBIN:
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
	mov	si, LoaderFileName	; next entry, di += 20h  
	jmp	LABEL_SEARCH_FOR_LOADERBIN; 

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:
	add	word [wSectorNo], 1
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_LOADERBIN:
	mov	si, MsgNoLoader
	call	DisplayMessage		; display string
	jmp	$			; no LOADER.BIN, infinite loop

LABEL_FILENAME_FOUND:			; LOADER.BIN was found
	mov	ax, RootDirSectors
	and	di, 0xFFE0		; Start of current entry, 32 bytes per entry
	add	di, 0x1A		; First sector of this file
	mov	cx, word [es:di]
	push	cx			; Save index of this sector in FAT
	add	cx, ax
	add	cx, DeltaSectorNo	; LOADER.BIN’s start sector saved in cl
	mov	ax, BaseOfLoader
	mov	es, ax			; es <- BaseOfLoader
	mov	bx, OffsetOfLoader	; bx <- OffsetOfLoader	es:bx = BaseOfLoader:OffsetOfLoader = BaseOfLoader * 10h + OffsetOfLoader
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
	jmp	BaseOfLoader:OffsetOfLoader	; Jump to LOADER.BIN’s start address in memory.

; *****************************************************************************************************



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
	mov	ax, BaseOfLoader	; 
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
	and	ax, 0FFFh

LABEL_GET_FAT_ENRY_OK:

	pop	bx
	pop	es
	ret
;--------------------------------------------------------
wRootDirSizeForLoop	dw	RootDirSectors	; Root Directory number, dec to 0
wSectorNo		dw	0		; sector number to read
bOdd			db	0		; odd or even ?
;------------------------------------------------------
LoaderFileName		db	"LOADER  BIN", 0	; LOADER.BIN 
;------------------------------------------------------
MsgHello		db	"Hello dk", 0x0D, 0x0A, 0x00
MsgBooting		db	"Booting", 0x0D, 0x0A, 0x00
MsgReady		db	"Ready!", 0x0D, 0x0A, 0x00
MsgNoLoader		db	"NO Loader! :(", 0x0D, 0x0A, 0x00
TIMES 510-($-$$) DB 0
DW 0xAA55

