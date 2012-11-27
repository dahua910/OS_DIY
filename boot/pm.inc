
;----------------------------------------------------------------------------
; GDT Descriptor Attributes
; DA_ : Descriptor Attribute
; D : Data Segment
; C : Code Segment
; S : System Segment
; R : Read-only
; RW : Read/Write
; A : Access 
;----------------------------------------------------------------------------
DA_32       EQU	  0x4000; 32-bit segment
DA_LIMIT_4K EQU	  0x8000; segment limit 4K 

DA_DPL0     EQU	  0x00	; DPL = 0
DA_DPL1     EQU	  0x20	; DPL = 1
DA_DPL2     EQU	  0x40	; DPL = 2
DA_DPL3     EQU	  0x60	; DPL = 3


;----------------------------------------------------------------------------
; GDT Code- and Data-Segment Types
;----------------------------------------------------------------------------
DA_DR     EQU	0x90	;Read-Only                    
DA_DRW    EQU	0x92	;Read/Write
DA_DRWA   EQU	0x93	;Read/Write, accessed 
DA_C      EQU	0x98	;Execute-Only             
DA_CR     EQU	0x9A	;Execute/Read              
DA_CCO    EQU	0x9C	;Execute-Only, conforming  
DA_CCOR   EQU	0x9E	;Execute/Read-Only, conforming

;----------------------------------------------------------------------------
; GDT System-Segment and Gate-Descriptor Types
;----------------------------------------------------------------------------
DA_LDT      EQU  82h	;  LDT               
DA_TaskGate EQU  85h	;  Task Gate         
DA_386TSS   EQU  89h	;  32-bit TSS(Available) 
DA_386CGate EQU  8Ch	;  32-bit Call Gate   
DA_386IGate EQU  8Eh	;  32-bit Interrupt Gate 
DA_386TGate EQU  8Fh	;  32-bit Trap Gate   
;----------------------------------------------------------------------------


;----------------------------------------------------------------------------
; Selector Attributes
;----------------------------------------------------------------------------
SA_RPL0		EQU	0	; 
SA_RPL1		EQU	1	; RPL
SA_RPL2		EQU	2	; 
SA_RPL3		EQU	3	; 

SA_TIG		EQU	0	; TI
SA_TIL		EQU	4	; 
;----------------------------------------------------------------------------


;----------------------------------------------------------------------------
;Segment Descriptor data structure.
; Usage: Descriptor Base, Limit, Attr
; Base: 4byte
; Limit: 4byte (low 20 bits available)
; Attr: 2byte (lower 4 bits of higher byte are always 0
;----------------------------------------------------------------------------
%macro Descriptor 3
	dw	%2 & 0xFFFF				; segment limit 1				(2 byte)
	dw	%1 & 0xFFFF				; base address 1				(2 byte)
	db	(%1 >> 16) & 0xFF			; base address 2				(1 byte)
	dw	((%2 >> 8) & 0xF00) | (%3 & 0F0FFh)	; attribute 1 + segment limit 2 + attribute 2		(2 attribute)
	db	(%1 >> 24) & 0xFF			; base address 3				(1 byte)
%endmacro ; 8 byte
;----------------------------------------------------------------------------