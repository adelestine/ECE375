;***********************************************************
;*	This is the skeleton file for Lab 3 of ECE 375
;*
;*	 Author: Astrid Delestine & Lucas Plaisted
;*	   Date: 2/3/2023
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is required for LCD Driver
.def	cr = r17				; Carry register :)
.equ	lcdL1 = 0x00			; Make LCD Data Memory locations constants
.equ	lcdH1 = 0x01
.equ	lcdL2 = 0x10
.equ	lcdH2 = 0x01			
;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND) 
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr
		; Initialize LCD Display
		rcall LCDInit
		rcall LCDBacklightOn
		; Initialize ports
		; Initialize Port D for input (from Lab 1)
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State
		; NOTE that there is no RET or RJMP from INIT,
		; this is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;*	Buttons:
;*		d4: clear text
;*		d5: display names
;*		d7: NOT 6!!! marquee-style, scroll between both lines
;*			"display at the beginning of the opposite line"
;***********************************************************
MAIN:							; The Main program
		; Main function design is up to you. Below is an example to brainstorm.
		rcall	BTN2MPR	; place 4 buttons into upper half of mpr
						; ACTIVE LOW!!!!!!
		sbrs	mpr, 7
		rcall	MARQUEE
		sbrs	mpr, 5
		rcall	DISPNAMES
		sbrs	mpr, 4	
		rcall	LCDClr

		; Move strings from Program Memory to Data Memory

		; Display the strings on the LCD Display

		rjmp	MAIN			; jump back to main and create an infinite
								; while loop.  Generally, every main program is an
								; infinite while loop, never let the main program
								; just run off

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variables by pushing them to the stack

		; Execute the function here

		; Restore variables by popping them from the stack,
		; in reverse order

		ret						; End a function with RET



;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; BTN2MPR: Button to MPR
; Desc: Places the 4 button inputs into the higher 4 bits
;		of mpr
;-----------------------------------------------------------
BTN2MPR:
		in		mpr, PIND		; Get input from Port D
		andi	mpr, 0b11110000	; Clear lower 4 mpr bits
		ret


;-----------------------------------------------------------
; Func: Marquee
; Desc: Calls DISPNAMES, shifts letters (bytes) from their
;		current data memory locations to the right, and if
;		going off of the right it will enter the left of
;		the next row, waiting for .25 seconds between each
;		move. This should be carrying bytes from the low 
;		address of the LCD screen and carrying them up to
;		the highest values.
;-----------------------------------------------------------
MARQUEE:

		ret

;-----------------------------------------------------------
; Func: Display Names
; Desc: Cut and paste this and fill in the info at the
;		beginning of your functions
;-----------------------------------------------------------
DISPNAMES:

		ret
;***********************************************************
;*	Stored Program Data
;***********************************************************

;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING_BEG:
.DB		"Astrid Delestine"		; Declaring data in ProgMem
STRING2_BEG:
.DB		" Lucas Plaisted "
STRING_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver
