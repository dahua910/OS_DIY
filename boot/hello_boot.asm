
org 0x7c00		;start from 0x7c00 @dkni question mark
	jmp LABEL_START
	nop							; nop required 

	BS_OEMName:		DB	"dongkang"		; OEM String, 8 bytes required 
	BPB_BytsPerSec:		DW	512			; Bytes per sector 
	BPB_SecPerCluster:	DB	1			; Sector per cluster 
	BPB_ResvdSecCnt:	DW	1			; Reserved sector count 
	BPB_NumFATs:		DB	2			; Number of FATs 
	BPB_RootEntCnt:		DW	224			; Root entries count 
	BPB_TotSec16:		DW	2880			; Total sector number 
	BPB_Media:		DB	0xf0			; Media descriptor 
	BPB_FATSz16:		DW	9			; FAT size(sectors) 
	BPB_SecPerTrk:		DW	18			; Sector per track 
	BPB_NumHeads:		DW	2			; Number of magnetic heads 
	BPB_HiddSec:		DD	0			; Number of hidden sectors 
	BPB_TotSec32:		DD	0			; If TotSec16 equal 0, this works 
	BS_DrvNum:		DB	0			; Driver number of interrupt 13 
	BS_Reserved1:		DB	0			; Reserved 
	BS_BootSig:		DB	0x29			; Boot signal 
	BS_VolID:		DD	0			; Volume ID 
	BS_VolLab:		DB	"dongkang ni"		; Volume label, 11 bytes required 
	BS_FileSysType:		DB	"FAT12 "		; File system type, 8 bytes required 

LABEL_START:	
	mov ax,cs   	;
	mov ds,ax   
	mov es,ax
	call PrintStr
	jmp	$

;call bios interrupt            
;display string(int=0x10, ah=10)
;ES:BP=string_addr              
;CX=string_len                  
;DH=line_row                    	
;DL=column                      	

;Clrscr:
;	mov ax,0x0600
;	int 0x10
   
PrintStr:   
	mov ax,Hello		
	mov bp,ax         
	mov cx,18					;srting_len
	mov ax,0x1301   
	mov bx,0xd    		;string type
	mov dl,0        
	int 0x10
	ret		 

;read disk(int=0x13, ah=0x02)
;al = section_total_num      
;ch,cl = dish_num,section_num
;ES:BX = data buffer addr    

;80 cylinders,2 heads,18 sectors, 512byte
;80x2x18x512=1440KB

;AX-->0x0211	al-->sector_num ah=0x02(read disk)
;ES:BX-->0x0820 data buffer addr
;CX-->2
;DX-->0

load_system:                            
	mov ax,0x0820			;after the boot addr
	mov es,ax					;load ram addr
	mov ch,0					;cylinder_num=0
	mov dh,0					;head_num=0 , drive letter
	mov cl,2					;section_num=2 @dkni question mark
	mov ax,es
	mov ah,0x02				;ah=0x02, read disk
	mov al,1					;17 sector				
	mov bx,0					;ES:BX = data buffer addr
	int 0x13

Hello: db "hello dk's DIY_os"  
times 510-($-$$) db 0   
dw 0xaa55
