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
.def	waitcnt = r17			; Counting registers for wait loop
.def	ilcnt = r18				
.def	olcnt = r19
.equ	lcdL1 = 0x00			; Make LCD Data Memory locations constants
.equ	lcdH1 = 0x01
.equ	lcdL2 = 0x10			; lcdL1 means the low part of line 1's location
.equ	lcdH2 = 0x01			; lcdH2 means the high part of line 2's location
.equ	lcdENDH = 0x01			; as it sounds, the last space in data mem
.equ	lcdENDL = 0x1F			; for storing lcd text
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
		rcall LCDClr
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
		rcall	BTN2MPR		; place 4 buttons into upper half of mpr
							; ACTIVE LOW!!!!!!
		sbrs	mpr, 7
		rcall	MARQUEE
		sbrs	mpr, 5
		rcall	DISPNAMES
		sbrs	mpr, 4	
		rcall	LCDClr

		; Move strings from Program Memory to Data Memory

		; Display the strings on the LCD Display

		rjmp	MAIN		; jump back to main and create an infinite
							; while loop.  Generally, every main program is an
							; infinite while loop, never let the main program
							; just run off



;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; BTN2MPR: Button to MPR
; Desc: Places the 4 button inputs into the higher 4 bits
;		of mpr. Don't forget the buttons are active low!
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
;
;		I have made the executive decision to stop this by
;		pressing button 
;-----------------------------------------------------------
MARQUEE:
		rcall DISPNAMES		; make sure the text is on screen
		ret

;-----------------------------------------------------------
; Func: Rotate Characters
; Desc: Rotates all characters through the locations
;		where the LCD pulls from, once.
;-----------------------------------------------------------
/*
The example given in the lab doc:
		Line 1: _____My_Name_is_
		Line 2: _______Jane_Doe_
			delay .25s
		Line 1: ______My_Name_is
		Line 2: ________Jane_Doe
			delay .25s
		Line 1: e______My_Name_i
		Line 2: s________Jane_Do
			delay .25s

We know that the data mem locations look like this:
		Line 1: $0100 : $010F
		Line 2: $0110 : $011F
And the shift is always done to the "right", with the
shifts carrying over into the next line. Each letter is
nicely one byte, making each line 16 bytes long (why the
locations go from 0 to f as well). In terms of shifting
to the right, this means that each letter at M(x) needs 
to be placed into M(x+1), except for the last letter,
which is placed into the first characters location.

As I see it, this can be done in two different ways.
	Start at the begining ($0100) and shift up
	
	Start at the end and shift up working backwards

Starting at the begining has the issue of needing to
hold onto the next value, as otherwise it would be
overwritten when the previous moves forwards. This
issue could maybe be overcome by using two registers
and sort of flip flopping between the two? I.e:
	Reg1 <- M(0)
	Reg2 <- M(1)

	M(1) <- Reg1 (place M(0) into M(1))
	Reg1 <- M(2) 
	M(2) <- Reg2 (place M(1) into M(2))
	Reg2 <- M(3) 
	M(3) <- Reg1 cycle repeats! (place M(2) into M(3))

This has the disadvantage of only working in pairs, 
and in general feels a little silly. Instead I will
work backwards

	stack <- top value
	M(top) <- M(top-1)
	M(top-1) <- M(top-2)
	...
	M(bottom+1) <- M(bottom)
	M(bottom) <- stack
*/

ROTCHAR:
		push YH				; push vars to stack
		push YL
		push mpr			; done

		ldi YH, lcdENDH
		ldi YL, lcdENDL		; Set Y to end of line 2
		ld	mpr, Y			; pull last character
		push mpr			; and stack it
rotloop:
		ld mpr, -Y			; dec Y, mpr <- m(Y)
		std Y+1, mpr		; move letter up 1 in data mem
		cpi YL, $00			; check if just moved first char
		brne rotloop		; if not go again until done

		pop mpr				; pop last character from stack
		st Y, mpr			; place last character at first
							; done with one rotation

		pop mpr				; pop vars from stack
		pop YL
		pop YH				; done
		ret
		
;-----------------------------------------------------------
; Func: Display Names
; Desc: Displayes names of project members by copying from 
;		data memory into program memory
;-----------------------------------------------------------
DISPNAMES:
		push ZL				; Save vars to stack
		push ZH
		push YL
		push YH
		push mpr
		push ilcnt

		ldi  ZL , low(STRING_BEG<<1)	; Sets ZL to the low bits  
			 					; of the first string location
		ldi  ZH , high(STRING_BEG<<1)	; Sets ZH to the first 
								; of the first string location
		ldi  YH , lcdH1
		ldi  YL , lcdL1
		ldi  ilcnt , 16

WCNEZ1: ; While ilcnt != zero 1
		lpm  mpr, Z+
		st   Y+ , mpr
		dec  ilcnt
		brne WCNEZ1

		;z is already pointing at the second string due to how memory is stored
		ldi  YH , lcdH2
		ldi  YL , lcdL2
		ldi  ilcnt , 16

WCNEZ2: ; While ilcnt != zero 2
		lpm  mpr, Z+
		st   Y+ , mpr
		dec  ilcnt
		brne WCNEZ2

		rcall LCDWrite

		pop ilcnt
		pop mpr
		pop YH
		pop YL
		pop ZH
		pop ZL					; Pop vars off of stack

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

;***********************************************************
;*	Functions and Subroutines Template
;***********************************************************
/*
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

		*/
