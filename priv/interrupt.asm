;;; Illustrates interrupt handling.

PC	EQU	000H		; PROGRAM COUNTER FOR MAIN CODE
INTPC	EQU	001H		; PROGRAM COUNTER FOR INTERRUPT HANDLER
SP	EQU	002H		; STACK POINTER FOR BOTH

	ORG	0000H
	;; Low-level startup from reset.
	;; P=X=R0=0 and IE=1: disable interrupts
	DIS
	BYTE	000H
	;; Initialize SP and INTPC
	LDI	0
	PHI	SP
	PHI	INTPC
	LDI	255
	PLO	SP		; SP = &(end of page 0)
	LDI	LOW(INTERRUPT)
	PLO	INTPC		; INTPC = &INTERRUPT
	;; Program the timer for 0.1Hz interrupt rate
	SEX	PC		; R(X) = PC so OUT takes parameters as immediates in the code
	OUT	6
	BYTE	000H		; timer mode 0 = disabled
	OUT	7
	BYTE	080H		; TIMER_WRITE_CONTROL, i.e. disable it
	OUT	6
	BYTE	000H
	OUT	7
	BYTE	013H		; INTERRUPT_WRITE_ENABLED, i.e. disable all IRQs
	OUT	6
	BYTE	000H
	OUT	7
	BYTE	014H		; INTERRUPT_WRITE_PENDING, i.e. clear all pending IRQs
	OUT	6
	BYTE	001H		; bit 1 set (IRQ 0, TIMER)
	OUT	7
	BYTE	013H		; INTERRUPT_WRITE_ENABLED, i.e. enable IRQ 0 (TIMER)
	OUT	6
	BYTE	001H		; timer mode 1 = 0.1Hz
	OUT	7
	BYTE	080H		; TIMER_WRITE_CONTROL, i.e. enable 0.1Hz timer interrupts
	;; Enable interrupts
	RET
	BYTE	020H		; fake T=<X:2,P=0>
	;; Wait for interrupt
	IDL
	;; Got an interrupt, output "OK\n" and halt
	SEX	PC
	DIS
	BYTE	000H
	OUT	6
	BYTE	04FH		; 'O'
	OUT	7
	BYTE	0E0H		; putchar
	OUT	6
	BYTE	04BH		; 'K'
	OUT	7
	BYTE	0E0H		; putchar
	OUT	6
	BYTE	00AH		; '\n'
	OUT	7
	BYTE	0E0H		; putchar
	OUT	6
	BYTE	000H
	OUT	7
	BYTE	000H		; halt

	;; Exit from interrupt handler
EXITI	SEX	2
	LDXA			; D = *(R(X)++)
	SHR			; DF = D&1; D >>= 1
	LDXA			; D = *(R(X)++)
	RET
	;; Interrupt handler
INTERRUPT
	;; On entry X=2, P=1, T=<interrupted X, interrupted P>
	DEC	SP		; (R(X))--
	SAV			; *(R(X)) = T
	DEC	SP		; (R(X))--
	STXD			; *(R(X)--) = D
	SHLC			; D = (D << 1) | DF
	STR	SP		; *(R(X)) = D (LSB is DF)
	;; Acknowledge the interrupt
	SEX	INTPC
	OUT	7
	BYTE	010H		; Interrupt Acknowledge
	SEX	SP
	DEC	SP
	INP	6		; *(R(X)) = D = IRQ
	INC	SP
	;; Output '!' if IRQ = 0, '?' otherwise
	SEX	INTPC
	BNZ	BADIRQ
	OUT	6
	BYTE	021H		; '!'
	BR	PUTCHAR
BADIRQ	OUT	6
	BYTE	03FH		; '?'
PUTCHAR	OUT	7
	BYTE	0E0H		; putchar
	;;
	BR	EXITI

	;; Rest of page 0 is stack
STACK
	;;
	END
