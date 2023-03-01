;***********************************************************
;*
;*	This is the skeleton file for Lab 6 of ECE 375
;*
;*	 Author: Astrid Delestine & Lucas Plaisted
;*	   Date: 3/1/23
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	waitcnt = r17			; Wait Loop Counter, 
								; waitcnt*10ms for delay
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter
.def	speeed = r20				; Speed register, max of 15

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; place instructions in interrupt vectors here, if needed

.org	$0056					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

		; Configure I/O ports
			; Initialize Port B for output
			ldi		mpr, $FF		; Set Port B Data Direction Register
			out		DDRB, mpr		; for output
			ldi		mpr, $00		; Initialize Port B Data Register
			out		PORTB, mpr		; so all Port B outputs are low
			; Initialize Port D for input
			ldi		mpr, $00		; Set Port D Data Direction Register
			out		DDRD, mpr		; for input
			ldi		mpr, $FF		; Initialize Port D Data Register
			out		PORTD, mpr		; so all Port D inputs are Tri-State
		; Configure External Interrupts, if needed
			; Should not need any
		; Configure 16-bit Timer/Counter 1A and 1B
			; TCCRIA Bits:
				; 7:6 - Timer/CounterA compare mode, 10 = non-inverting mode
					; On compare match clears port B pin 5
				; 5:4 - Timer/CounterB compare mode, 10 = non-inverting mode
					; On compare match clears port B pin 6
				; 3:2 - Timer/CounterC compare mode, 00 = disabled
				; 1:0 - Wave gen mode low half, 01 for 8-bit fast pwm
			ldi mpr, 0b10_10_00_01	
			sts TCCR1A, mpr
			; TCCRIB Bits:
				; 7:5 - not relevant, 0's
				; 4:3 - Wave gen mode high half, 01 for 8 bit fast pwm
				; 2:0 - Clock selection, 001 = no prescale
			ldi mpr, 0b000_01_001
			sts TCCR1B, mpr
			; Fast PWM, 8-bit mode, no prescaling
				; In inverting Compare Output mode output is cleared on compare match and set at TOP
				;

		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL) on Port B
			ldi mpr, $F0
			out PINB, mpr
		; Set initial speeed, display on Port B pins 3:0
			ldi speeed, $0F
			rcall WRITESPD
		; Enable global interrupts (if any are used)
			; Not used
		ldi waitcnt, 5	; Set wait timer to be 100ms

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; poll Port D pushbuttons (if needed)
		in mpr, PIND
		sbrs mpr, 7	; Run next command if button 7 presed (active low)
		rcall MAXSPD
		sbrs mpr, 5
		rcall DECSPD
		sbrs mpr, 4
		rcall INCSPD

		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	INCSPD
; Desc:	Increases the "speeed" of the motor by increasing
;		the width of the pulse. Has built in debouncing.
;		Prevents going over the max speeed.
;-----------------------------------------------------------
INCSPD:
		; Push to stack
		push mpr

		inc speeed		; increase the speeed
		sbrc speeed, 5	; Skip next command if bit 5 is cleared
						; If bit 5 is set then we are 16+, 15 is max
		ldi speeed, 15	; If we are over 15, set speeed to 15
		rcall WRITESPD
INCHOLD:				; Don't leave until we let go of the button
		rcall Wait		; Wait 50ms, debouncing
		in mpr, PIND	; Grab current button value
		sbrs mpr, 4		; Check if button is still held
		rjmp INCHOLD	; Stay in loop if held
		; Pop from stack
		pop mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	DECSPD
; Desc:	Decreases the "speeed" of the motor by decreasing
;		the width of the pulse. Has built in debouncing
;-----------------------------------------------------------
DECSPD:
		; Push to stack
		push mpr
		cpi speeed, 0	; If speeed is 0
		breq DECSKIP	; Don't decrement
		dec speeed
		rcall WRITESPD
		; Pop from stack
DECHOLD:				; Don't leave until we let go of the button
		rcall Wait		; Wait 50ms, debouncing
		in mpr, PIND	; Grab current button value
		sbrs mpr, 5		; Check if button is still held
		rjmp DECHOLD	; Stay in loop if held
DECSKIP:
		pop mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	MAXSPD
; Desc:	Increases the "speeed" 
;-----------------------------------------------------------
MAXSPD:
		push mpr
		ldi speeed, 15
		rcall WRITESPD
MAXHOLD:				; Don't leave until we let go of the button
		rcall Wait		; Wait 100ms, debouncing
		in mpr, PIND	; Grab current button value
		sbrs mpr, 7		; Check if button is still held
		rjmp MAXHOLD	; Stay in loop if held
		pop mpr
		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	WRITESPD
; Desc:	Sets the timer compares for the current speeed as
;		well as setting the lower nibble of 
;-----------------------------------------------------------
WRITESPD:
		push mpr
		ldi mpr, 17		; 255/15 = 17
		mul speeed, mpr	; speeed*17 = pulse width
		clr mpr			; set mpr to 0
		sts OCR1AH, mpr	; write to high byte of both compares
		mov mpr, R0		; place output into mpr. Max 255 = 1 reg
		sts OCR1AL, mpr	; write to low byte of both compares
		clr mpr
		sts OCR1BH, mpr	; only done because requried
		mov mpr, R0		; place output into mpr. Max 255 = 1 reg
		sts OCR1BL, mpr	; write to low byte of both compares
		ldi mpr, 0b10010000
		add mpr, speeed
		out PINB, mpr
		pop mpr
		ret

		;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly
;		waitcnt*10ms.  Just initialize wait for the specific amount
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			(((((3*ilcnt)-1+4)*olcnt)-1+4)*waitcnt)-1+16
;----------------------------------------------------------------
Wait:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt		; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt		; Decrement wait
		brne	Loop			; Continue Wait loop

		pop		olcnt		; Restore olcnt register
		pop		ilcnt		; Restore ilcnt register
		pop		waitcnt		; Restore wait register
		ret				; Return from subroutine

;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program
