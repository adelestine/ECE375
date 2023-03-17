;***********************************************************
;*
;*	This is the skeleton file forHW4, problem 1 of ECE 375
;*
;*	 Author: Matthew Shuman
;*	   Date: March 1st, 2023
;*	 Modified: Lucas Plaisted
;*	   Date: March 16th, 2023
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register

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
		; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

		; Initialize Port B for output (LEDS)
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low

		; Initialize Port D for inputs
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $F0		; Enable pull-up resistors for PD7 to PD4
		out		PORTD, mpr		;


;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		sbis	PIND, 7		;1 clock if not skipped, 2 if skipped
		rcall	D7PUSHED	;4 clocks, one word instruction
		;Button 7 Pressed: 15 clocks; Not Pressed: 2 clocks

		sbis	PIND, 6		;1 clock if not skipped, 2 if skipped
		rcall	D6PUSHED	;4 clocks
		;Button 6 Pressed: 13 clocks; Not Pressed: 2 clocks

		sbis	PIND, 5		;1 clock if not skipped, 2 if skipped
		rcall	D5PUSHED	;4 clocks
		;Button 5 Pressed: 11 clocks; Not Pressed: 2 clocks

		sbis	PIND, 4		;1 clock if not skipped, 2 if skipped
		rcall	D4PUSHED	;4 clocks
		;Button 4 Pressed: 10 clocks; Not Pressed: 2 clocks

/*
*		Totals for questions:
*			a. No buttons pushed : 2*4+5 = 29 clocks
*			b. Only D4 pushed : 2*3+10+5 = 33 clocks
*			c. Only D5 pushed : 2*3+11+5 = 34 clocks
*			d. All buttons pushed : 15+13+11+10+5 = 54 clocks
*
*
*/
DONECHECK:
		in		mpr, PINB		; read current values for PORTB
							;1 clock
		com		mpr		; invert values
							;1 clock
		out		PORTB, mpr		; flip the LEDs
							;1 clock

		rjmp	MAIN			; return to top of MAIN
							;2 clocks
			;DONECHECK clocks: 5
;***********************************************************
;*	Functions and Subroutines
;***********************************************************


D7PUSHED: ;10 clocks total
		nop					;5x1 clocks	
		nop						;
		nop						;
		nop						;
		nop						;
		ret					;5 clocks

D6PUSHED: ;8 clocks total
		nop					;3x1 clocks
		nop						;
		nop						;
		ret					;5 clocks

D5PUSHED: ;6 clocks total
		nop					;1 clock
		ret					;5 clocks

D4PUSHED: ;5 clocks total
		ret 				;5 clocks
		
;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program


