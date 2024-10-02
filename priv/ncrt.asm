;;; The New Call and Return Technique (NCRT).
;;;
;;; The old Standard Call and Return Technique (SCRT) has the following problems:
;;; - It keeps the current return address in a register (LINK) but also preserves
;;;   the previous one on the stack during calls.  This makes calls and returns
;;;   just as expensive as if the return address only existed on the stack.
;;; - Keeping the return address in a register means that 7 registers are reserved
;;;   (r0-r2 by HW, r3-r6 by SCRT), leaving only 9 for general-purpose usage.
;;; - Having the return address in a register could be used for passing parameters
;;;   in the code itself, but current preferences are to pass parameters in registers
;;;   or on the stack.
;;;
;;; The NCRT therefore eliminates the LINK register, and as a cleanup specifies that
;;; return addresses are stored in BIG-endian order on the stack.  Apart from that it
;;; is identical to the SCRT.
;;;
;;; Register Usage
;;; --------------
;;; R2		SP: stack pointer for the call stack, grows towards lower,
;;;		should point to first free byte below the used stack
;;; R3		PC: program counter, P=3 except during calls and returns
;;; R4		CALL: address of the routine for making a subroutine call
;;; R5		RETN: address of the routine for returning from a subroutine call
;;;
;;; Stack Usage
;;; -----------
;;; During a subroutine call the caller's return address is pushed on the
;;; stack.
;;;
;;; Calls
;;; -----
;;; To call a named routine CALLEE:
;;;
;;; CALLER
;;;     ;; ...
;;;	;; PRE: P=3
;;;	SEP	R4
;;;	BYTE	HIGH(CALLEE)
;;;	BYTE	LOW(CALLEE)
;;; RETADDR
;;;	;; POST: returns here, P=3, X=2, SP as before "SEP R4", D clobbered
;;;
;;; CALLEE
;;;	;; On entry: P=3, X=2, CALLER's return address (RETADDR) on stack (SP), D clobbered
;;;
;;; Returns
;;; -------
;;; To return from CALLEE to CALLER:
;;;
;;; CALLEE
;;;	;; ...
;;;	;; PRE: SP (R2) and MEM[SP+[1-2]] as on entry at CALLEE
;;;	SEP	R5
;;;	;; POST: continues at RETADDR with CALLER's SP restored

R0	EQU	000H
SP	EQU	002H		; STACK POINTER
PC	EQU	003H		; PROGRAM COUNTER
CALL	EQU	004H		; CALL ROUTINE REGISTER
RETN	EQU	005H		; RETURN ROUTINE REGISTER

	ORG	0000H
	;; Low-level startup from reset.
	;; P=X=R0=0 and IE=1: disable interrupts
	DIS
	BYTE	00H
	;; Initialize SP, PC, CALL, and RETN
	LDI	0
	PHI	RETN		; HIGH(RETN) = 0
	PHI	CALL		; HIGH(CALL) = 0
	PHI	SP		; HIGH(SP) = 0
	LDI	LOW(RETRTN)
	PLO	RETN		; RETN = RETRTN
	LDI	LOW(CALRTN)
	PLO	CALL		; CALL = CALRTN
	LDI	255
	PLO	SP		; SP = (end of page 0)
	LDI	HIGH(START)
	PHI	PC
	LDI	LOW(START)
	PLO	PC		; PC = START
	SEP	PC		; jump to START

EXITC	;; At this point, P=4, X=2, PC=CALLEE, CALLER's RA1 is on stack
	SEP	PC		; jump to CALLEE, leaving CALL=CALRTN
CALRTN	;; CALLER did
	;;	; P=3
	;;	SEP	CALL
	;; RA0:	BYTE	HIGH(CALLEE)
	;;	BYTE	LOW(CALLEE)
	;; RA1:	; wants to return here
	;;
	;; At this point, P=4, CALL=CALRTN, PC=RA0.
	;; First step: increment PC and push RA1 on the stack
	SEX	SP		; X points to stack
	INC	PC
	INC	PC
	GLO	PC
	STXD			; push LOW(RA1) on the stack
	GHI	PC
	STXD			; push HIGH(RA1) on the stack
	;; Second step: decrement PC and load CALLEE into PC
	DEC	PC
	LDN	PC		; D = LOW(CALLEE)
	STR	SP		; MEM[SP] = LOW(CALLEE)
	DEC	PC
	LDN	PC		; D = HIGH(CALLEE)
	PHI	PC		; HIGH(PC) = HIGH(CALLEE)
	LDN	SP
	PLO	PC		; LOW(PC) = LOW(CALLEE)
	BR	EXITC

EXITR	;; At this point, P=5, X=2, PC=RA1, CALLER's SP has been restored
	SEP	PC		; jump to CALLER, leaving RETN=RETRTN
RETRTN	;; CALLEE did:
	;;	; P=3, SP as on entry to CALLEE
	;;	SEP	RETN
	;;
	;; At this point, P=5, CALLER's RA1 is on stack
	;; First step: pop PC off SP
	SEX	SP		; X points to stack
	INC	SP		; STXD decrements R(X) after storing to M(R(X))
	LDA	SP		; same as LDXA here, but fewer dynamic dependencies
	PHI	PC		; Pop HIGH half of saved RA1
	LDN	SP		; same as LDX here, but fewer dynamic dependencies
	PLO	PC		; Pop LOW half of saved RA1
	BR	EXITR

	;; Rest of page 0 is stack
	PAGE			; Advance to next page
	;;
START	;; main()
	SEP	CALL
	BYTE	HIGH(MAIN)
	BYTE	LOW(MAIN)
	;; exit(0)
	SEX	PC		; following OUTs take immediates in the code
	OUT	6
	BYTE	00H		; status=0
	OUT	7
	BYTE	00H		; halt
	;;
MAIN	;; R0 = STRING
	LDI	HIGH(STRING)
	PHI	R0
	LDI	LOW(STRING)
	PLO	R0		; R0 = STRING
	BR	CHECK
BODY	;; putchar(*(R0++))
	OUT	6		; *(R0++)
	SEX	PC		; X=P
	OUT	7		; .. so this OUT takes the next byte from the code
	BYTE	0E0H		; putchar
CHECK	;; Make R(X) follow our string pointer
	SEX	R0
	;; Enter loop body if *R0 != 0
	LDX
	BNZ	BODY
	;; return
	SEP	RETN
	;;
STRING	TEXT	"SALVE"
	BYTE	0AH
	BYTE	00H
	;;
	END
