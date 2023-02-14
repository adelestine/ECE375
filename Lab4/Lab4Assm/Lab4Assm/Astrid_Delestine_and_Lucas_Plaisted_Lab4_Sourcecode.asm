;***********************************************************
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;*	 Author: Enter your name
;*	   Date: Enter Date
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine

		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND) 
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr
		; TODO

		clr		zero			; Set the zero register to zero, maintain
										; these semantics, meaning, don't
										; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program

		; Call function to load ADD16 operands
		nop ; Check load ADD16 operands (Set Break point here #1)

		; Call ADD16 function to display its results (calculate FCBA + FFFF)
		nop ; Check ADD16 result (Set Break point here #2)


		; Call function to load SUB16 operands
		nop ; Check load SUB16 operands (Set Break point here #3)

		; Call SUB16 function to display its results (calculate FCB9 - E420)
		nop ; Check SUB16 result (Set Break point here #4)


		; Call function to load MUL24 operands
		rcall LOADMUL24
		nop ; Check load MUL24 operands (Set Break point here #5)

		; Call MUL24 function to display its results (calculate FFFFFF * FFFFFF)
		nop ; Check MUL24 result (Set Break point here #6)

		; Setup the COMPOUND function direct test
		nop ; Check load COMPOUND operands (Set Break point here #7)

		; Call the COMPOUND function
		nop ; Check COMPOUND result (Set Break point here #8)

DONE:	rjmp	DONE			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;-----------------------------------------------------------
ADD16:
		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y

		; Load beginning address of result into Z

		; Execute the function

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;       result. Always subtracts from the bigger values.
;-----------------------------------------------------------
SUB16:
		; Execute the function here


		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit
;       result.
;-----------------------------------------------------------
MUL24:
;* - Simply adopting MUL16 ideas to MUL24 will not give you steady results. You should come up with different ideas.
/*
Imagine we are multiplying two 24-bit numbers, ABC and DEF.
A is the highest byte, C is the lowest byte. So A is referring
to only the highest byte of ABC. Imagine that when multiplying
C and F, the result is CFH:CFL where CFH and CFL are the high
and low bytes respectively of the result of multiplying C and F.

If you were to write out
how to multiply these together with individual 8-bit multiplication,
it might look something like this:

				A	B	C	*
				D	E	F
===================================
					CFH	CFL
				CEH	CEL
			CDH	CDL
				BFH	BFL
			BEH	BEL
		BDH	BDL
			AFH	AFL
		AEH	AEL
+	ADH	ADL
	5	4	3	2	1	0
===================================

This result does in fact take up 6 bytes, or 48-bits.
Something to keep in mind here is that when adding each
individual result to the total (such as CDL) there will
be previous results already added to that byte. That means
that we need to keep in mind carries. There will be no 
carry coming out of the last byte, as two 24-bit numbers
multiplied together (FFFFFF x FFFFFF) have a maximum
possible value of FFFFFE000001, which is only 6 bytes!

The way I will handle carries is every time I do addition,
I will check the carry bit to see if it is set. If it is,
I will move up where I am looking at by one byte, then add
the carry. Since this is also addition, I will check the
carry AGAIN! Repeat until no more carries. This means 
I could carry up to 4 times, (the lowest byte will never
carry since it is only added to once) so for reliabilities
sake I will use a loop to check/add caries instead of just
adding the carry to every possibly byte even if there is
no carry.

*/
		; Execute the function here
		push XH		; Push literally everything because
		push XL		; I have no idea what I will need :)
		push YH
		push YL
		push ZH
		push ZL
		push mpr
		push rlo
		push rhi
		push A
		push B
		push iloop
		push oloop

		pop oloop
		pop iloop
		pop B
		pop A
		pop rhi
		pop rlo
		pop mpr
		pop ZL
		pop ZH
		pop YL
		pop YH
		pop XL
		pop XH

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: LOADMUL24
; Desc: Loads the numbers needed for the example MUL24
;-----------------------------------------------------------
LOADMUL24:
		; Execute the function here
		push XH		; Push literally everything because
		push XL		; I have no idea what I will need :)
		push YH
		push YL
		push ZH
		push ZL
		push mpr
		push rlo
		push rhi
		push A
		push B
		push iloop
		push oloop

		; Uses OperandE1, OperandE2, OperandF1, and OperandF2
		; Placing these into MUL24_OP1 and MUL24_OP2 respectively
		ldi ZH, high(OperandE1)	; load OperandE1 location to Z
		ldi ZL, low(OperandE1)
		; Shift Z to prepare for program memory access:
		lsl ZH
		lsl ZL
		adc ZH, zero ; shift carry from lower byte to upper byte
		ldi YH, high(MUL24_OP1)	; Load OP1 location into Y
		ldi YL, low(MUL24_OP1)	; ($0118)
				
		ldi oloop, 3	; load A with 3 to loop 3 times.
mulloadloop1:
		lpm mpr, Z+	;load mpr from Z, inc Z
		st Y+, mpr	; store mpr to Y, inc Y
		dec oloop		; decrement A to run loop 3 times
		brne mulloadloop1
		; since operand E2 is immediately after E1 in the program data
		; we should be able to just increment to it :)
		; Operand E is now loaded to MUL24_OP1
		
		ldi ZH, high(OperandF1)	; load OperandF1 location to Z
		ldi ZL, low(OperandF1)
		; Shift Z to prepare for program memory access:
		lsl ZH
		lsl ZL
		adc ZH, zero ; shift carry from lower byte to upper byte
		ldi YH, high(MUL24_OP2)	; Load OP1 location into Y
		ldi YL, low(MUL24_OP2)	; ($011B)

		ldi oloop, 3	; load A with 3 to loop 3 times.
mulloadloop2:
		lpm mpr, Z+	;load mpr from Z, inc Z
		st Y+, mpr	; store mpr to Y, inc Y
		dec oloop		; decrement A to run loop 3 times
		brne mulloadloop2
		; Both operands should be loaded into program mem now!

		pop oloop
		pop iloop
		pop B
		pop A
		pop rhi
		pop rlo
		pop mpr
		pop ZL
		pop ZH
		pop YL
		pop YH
		pop XL
		pop XH

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((G - H) + I)^2
;       by making use of SUB16, ADD16, and MUL24.
;
;       D, E, and F are declared in program memory, and must
;       be moved into data memory for use as input operands.
;
;       All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:

		; Setup SUB16 with operands G and H
		; Perform subtraction to calculate G - H

		; Setup the ADD16 function with SUB16 result and operand I
		; Perform addition next to calculate (G - H) + I

		; Setup the MUL24 function with ADD16 result as both operands
		; Perform multiplication to calculate ((G - H) + I)^2

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;       A - Operand A is gathered from address $0101:$0100
;       B - Operand B is gathered from address $0103:$0102
;       Res - Result is stored in address
;             $0107:$0106:$0105:$0104
;       You will need to make sure that Res is cleared before
;       calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;       beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;*	Do not  section.
;***********************************************************
; ADD16 operands
OperandA:
	.DW 0xFCBA
OperandB:
	.DW 0xFFFF

; SUB16 operands
OperandC:
	.DW 0XFCB9
OperandD:
	.DW 0XE420

; MUL24 operands
OperandE1:
	.DW	0XFFFF
OperandE2:
	.DW	0X00FF
OperandF1:
	.DW	0XFFFF
OperandF2:
	.DW	0X00FF

; Compoud operands
OperandG:
	.DW	0xFCBA				; test value for operand G
OperandH:
	.DW	0x2022				; test value for operand H
OperandI:
	.DW	0x21BB				; test value for operand I

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.
.org	$0110				; data memory allocation for operands
ADD16_OP1:	;$0110
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:	;$0112
		.byte 2				; allocate two bytes for second operand of ADD16
SUB16_OP1:	;$0114
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:	;$0116
		.byte 2				; allocate two bytes for second operand of SUB16
MUL24_OP1:	;$0118
		.byte 3				; allocate three bytes for first operand of MUL24
MUL24_OP2:	;$011B
		.byte 3				; allocate three bytes for second operand of MUL24


.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$130
.org	$140

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program