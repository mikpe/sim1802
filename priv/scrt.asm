;;; Illustrates subroutine calls and returns using SCRT.
;;;
;;; Register Usage
;;; --------------
;;; R2		SP: stack pointer for the call stack, grows towards lower,
;;;		should point to first free byte below the used stack
;;; R3		PC: program counter, P=3 except during calls and returns
;;; R4		CALL: address of the routine for making a subroutine call
;;; R5		RETN: address of the routine for returning from a subroutine call
;;; R6		LINK: link register holding the PC to return to
;;;
;;; Stack Usage
;;; -----------
;;; During a subroutine call the caller's LINK register is pushed on the
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
;;;	;; POST: returns here, P=3, X=2, LINK and SP as before "SEP R4", D clobbered
;;;
;;; CALLEE
;;;	;; On entry: P=3, X=2, LINK=&RETADDR, CALLER's LINK is on stack (SP), D clobbered
;;;
;;; Returns
;;; -------
;;; To return from CALLEE to CALLER:
;;;
;;; CALLEE
;;;	;; ...
;;;	;; PRE: SP (R2) and LINK (R6) as on entry at CALLEE
;;;	SEP	R5
;;;	;; POST: continues at RETADDR with CALLER's LINK and SP restored

R0	EQU	000H
SP	EQU	002H		; STACK POINTER
PC	EQU	003H		; PROGRAM COUNTER
CALL	EQU	004H		; CALL ROUTINE REGISTER
RETN	EQU	005H		; RETURN ROUTINE REGISTER
LINK	EQU	006H		; LINK (RETURN ADDRESS) REGISTER

	ORG	0000H
	;; Low-level startup from reset.
	;; P=X=R0=0 and IE=1: disable interrupts
	DIS
	BYTE	00H
	;; Initialize SP, PC, CALL, RETN, and LINK
	LDI	0
	PHI	LINK
	PLO	LINK		; LINK = 0000H
	PHI	RETN		; HIGH(RETN) = 0
	PHI	CALL		; HIGH(CALL) = 0
	PHI	SP		; HIGH(SP) = 0
	LDI	LOW(RETRTN)
	PLO	RETN		; RETN = &RETRTN
	LDI	LOW(CALRTN)
	PLO	CALL		; CALL = &CALRTN
	LDI	255
	PLO	SP		; SP = &(end of page 0)
	LDI	HIGH(START)
	PHI	PC
	LDI	LOW(START)
	PLO	PC		; PC = &START
	SEP	PC		; jump to START

	;; NOTE: The original software SCRT represents LINK registers on
	;; the stack in LITTLE-endian order, although the CPU is BIG-endian.
	;; The 1804 SCAL/SRET instructions are compatible except for
	;; representing LINK registers on the stack in BIG-endian order.

EXITC	;; At this point, P=4, X=2, PC=&CALLEE, LINK=&RA1, CALLER's LINK is on stack
	SEP	PC		; jump to CALLEE, leaving CALL=&CALRTN and LINK=&RA1
CALRTN	;; CALLER did
	;;	; P=3
	;;	SEP	CALL
	;; RA0:	BYTE	HIGH(CALLEE)
	;;	BYTE	LOW(CALLEE)
	;; RA1:	; wants to return here
	;;
	;; At this point, P=4, CALL=&CALRTN, PC=&RA0.
	;; First step: push LINK on the stack
	SEX	SP		; X points to stack
	GHI	LINK
	STXD			; push HIGH(LINK) on the stack
	GLO	LINK
	STXD			; low LOW(LINK) on the stack
	;; Second step: copy PC (&RA0) to LINK
	GHI	PC
	PHI	LINK		; Copy HIGH half of RA0 to LINK
	GLO	PC
	PLO	LINK		; Copy LOW half of RA0 to LINK
	;; Third step: load &CALLEE from LINK to PC and update LINK to &RA1
	LDA	LINK		; D = M(LINK++)
	PHI	PC		; Copy HIGH half of &CALLEE to PC
	LDA	LINK		; D = M(LINK++)
	PLO	PC		; Copy LOW half of &CALLEE to PC
	BR	EXITC

EXITR	;; At this point, P=5, X=2, PC=&RA1, CALLER's LINK has been restored
	SEP	PC		; jump to CALLER, leaving RETN=&RETRTN
RETRTN	;; CALLEE did:
	;;	; P=3, SP as on entry to CALLEE
	;;	SEP	RETN
	;;
	;; At this point, P=5, LINK=&RA1, CALLER's LINK is on stack
	;; First step: copy LINK to PC
	GHI	LINK
	PHI	PC		; Copy HIGH half of LINK to PC
	GLO	LINK
	PLO	PC		; Copy LOW half of LINK to PC
	;; Second step: pop LINK off SP
	SEX	SP		; X points to stack
	INC	SP		; STXD decrements R(X) after storing to M(R(X))
	LDXA
	PLO	LINK		; Pop LOW half of saved LINK
	LDX
	PHI	LINK		; Pop HIGH half of saved LINK
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
MAIN	;; R0 = &STRING
	LDI	HIGH(STRING)
	PHI	R0
	LDI	LOW(STRING)
	PLO	R0		; R0 = &STRING
	BR	CHECK
BODY	;; putchar(*(R0++))
	OUT	6		; *(R0++)
	SEX	PC		; X=P
	OUT	7		; .. so this OUT takes the next byte from the code
	BYTE	01H		; putchar
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
