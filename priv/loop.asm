	;; Output "CIAO\n" to stdout and exit 0
	ORG	0000H
	;; At reset start with P=X=R(0)=0 and IE=1: disable interrupts
	DIS
	BYTE	00H
	;; Make R(1) point to the string to output
	LDI	HIGH(STRING)
	PHI	1
	LDI	LOW(STRING)
	PLO	1
	;; Start loop at the iteration check
	BR	CHECK
BODY	;; Loop body: putchar(*(R(X)++))
	OUT	6     		; *(R(X)++)
	SEX	0		; X=P
	OUT	7		; .. so this OUT takes the next byte from the code
	BYTE	0E0H		; putchar
CHECK	;; Make R(X) follow our string pointer
	SEX	1
	;; Enter body if *(R(X)) != 0
	LDX
	BNZ	BODY
	;; exit(0)
	SEX	0
	OUT	6
	BYTE	00H		; status=0
	OUT	7
	BYTE	00H		; halt
	;;
STRING	TEXT	"CIAO"
	BYTE	0AH
	BYTE	00H
	;;
	END
