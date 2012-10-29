org 0x7c00
	mov ax,cs   
	mov ds,ax   
	mov es,ax   
call PrintStr   
	jmp $   
PrintStr:   
	mov ax,Hello   
	mov bp,ax   
	mov cx,1000   
	mov ax,01301h   
	mov bx,000ch   
	mov dl,0   
	int 10h   
	ret   

Hello: db "hello dk's DIY_os"  
times 510-($-$$) db 0   
dw 0xaa55 
