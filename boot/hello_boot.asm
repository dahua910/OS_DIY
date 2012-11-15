
org 0x7c00		;start from 0x7c00 @dkni question mark


;BaseOfStack		equ	0100h	;debug mode
BaseOfStack		equ	07c00h	; Stack base address, inner
BaseOfLoader		equ	09000h	; Section loading address of LOADER.BIN 
OffsetOfLoader		equ	0100h	; Loading offset of LOADER.BIN
RootDirSectors		equ	14	; Root directory sector count
SectorNoOfRootDirectory	equ	19	; 1st sector of root directory


	jmp short LABEL_START
	nop							; nop required 

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
	mov	ax, cs
	mov	ds, ax
	mov	es, ax
	mov	ss, ax
	mov	sp, BaseOfStack
	call Clear_screen
	;call	Print_no_loader
	;call Floopy_reset
	call Print_hello
	call Print_find_loader
	;call Print_no_loader
	;call Floopy_reset	 
	mov	word [wSectorNo], SectorNoOfRootDirectory
	call LABEL_SEARCH_IN_ROOT_DIR_BEGIN
	
	jmp	$


;call bios interrupt            
;display string(int=0x10, ah=10)
;ES:BP=string_addr              
;CX=string_len                  
;DH=line_row                    	
;DL=column                      	

Clear_screen:
	mov ax,0x0600
	mov bx,0x0700
	mov cx,0
	mov dx,0x184f
	int 0x10
	ret
   
Print_hello:   
	mov ax, Hello_srting		
	mov bp,ax         
	mov cx,8					;srting_len
	mov ax,0x1301   
	mov bx,0xe    		;string type
	mov dx,0x0        
	int 0x10
	ret
	
Print_no_loader:   
	mov ax, No_loader_string		
	mov bp,ax         
	mov cx,8					;srting_len
	mov ax,0x1301   
	mov bx,0xe    		;string type
	mov dx,0x0200        
	int 0x10
	ret

Print_find_loader:   
	mov ax, Find_loader_string		
	mov bp,ax         
	mov cx,17					;srting_len
	mov ax,0x1301   
	mov bx,0xe    		;string type
	mov dx,0x0100        
	int 0x10
	ret

Floopy_reset:
	xor ah, ah
	xor dl, dl
	int 0x13
	ret		 

;read cl sectors from ax sector to ES:BX
;Assume sector number is ��x��,
;x/(BPB_SecPerTrk) = y,
;x%(BPB_SecPerTrk) = z.
;The remainder 'z'PLUS 1 is the start sector number;
;The quotient 'y' divide by BPB_NumHeads(RIGHT SHIFT 1 bit)is cylinder number
;AND 'y' by 1 can got magnetic header.

Read_sector:
	push	bp
	mov	bp, sp
	sub	esp, 2 ; Reserve space for saving: byte [bp-2]
	mov	byte [bp-2], cl
	push	bx			; save bx
	mov	bl, [BPB_SecPerTrk]	; bl: divider
	div	bl			;al=y, ah=z
	inc	ah			;z ++
	mov	cl, ah			;cl <- start sector_num
	mov	dh, al			;dh <- y
	shr	al, 1			;y >> 1 (y/BPB_NumHeads)
	mov	ch, al			;ch <- Cylinder number(y>>1)
	and	dh, 1			;dh & 1 = Magnetic header(y&1)
	pop	bx			;restore bx
	;Now, we got cylinder number in %ch, start sector number in %cl, magnetic header in %dh 
	mov	dl, [BS_DrvNum]		;driver 0
GoOnReading:
	mov	ah, 2			; read
	mov	al, byte [bp-2]		;read al sector
	int	13h
	jc	GoOnReading		; If CF set 1, mean read error, reread
	add	esp, 2
	pop	bp
	ret








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
	call Read_sector

	mov	si, LoaderFileName	; ds:si -> "LOADER  BIN"
	mov	di, OffsetOfLoader	; es:di -> BaseOfLoader:0100
	cld
	mov	dx, 10h
LABEL_SEARCH_FOR_LOADERBIN:
	cmp	dx, 0				   ; , loop time
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
	and	di, 0FFE0h		; Go to head of this entry 
	add	di, 20h			;
	mov	si, LoaderFileName	; next entry, di += 20h  
	jmp	LABEL_SEARCH_FOR_LOADERBIN; 

LABEL_GOTO_NEXT_SECTOR_IN_ROOT_DIR:
	add	word [wSectorNo], 1
	jmp	LABEL_SEARCH_IN_ROOT_DIR_BEGIN

LABEL_NO_LOADERBIN:
;	mov	dh, 2			; "No LOADER."
	call	Print_no_loader			; display string
	jmp	$			; no LOADER.BIN, infinite loop

LABEL_FILENAME_FOUND:			; LOADER.BIN was found
	mov ax, 0x2b2b
	jmp	$			; infinite loop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
wRootDirSizeForLoop	dw	RootDirSectors	; Root Directory sectors, decrease to 0 
wSectorNo		dw	0		; sector num to read
bOdd			db	0		; odd or even


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;const string~
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Find_loader_string : db"Loader is here :)"
Hello_srting: db "hello OS"
No_loader_string: db "No loader :("


LoaderFileName		db	"LOADER  BIN", 0 ; LOADER.BIN 

times 510-($-$$) db 0   
dw 0xaa55
