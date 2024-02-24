	;; Output "HELO\n" to stdout and exit 0
	org	0000H
	;; Reset: starts at 0000H with P=X=R(0)=0 and IE=1. Disable interrupts.
	DIS
	BYTE	00H
	;; The OUTs need R(X) to point to the data and command bytes.
	;; Since X=P, those bytes are effectively immediates in the code.
	OUT	6
	BYTE	48H		; 'H'
	OUT	7
	BYTE	01H		; putchar
	OUT	6
	BYTE	45H		; 'E'
	OUT	7
	BYTE	01H		; putchar
	OUT	6
	BYTE	4CH		; 'L'
	OUT	7
	BYTE	01H		; putchar
	OUT	6
	BYTE	4FH		; 'O'
	OUT	7
	BYTE	01H		; putchar
	OUT	6
	BYTE	0AH		; '\n'
	OUT	7
	BYTE	01H		; putchar
	OUT	6
	BYTE	00H		; status=0
	OUT	7
	BYTE	00H		; halt
	;;
	END
