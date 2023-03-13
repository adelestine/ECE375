
;***********************************************************
;*
;*	This is the TRANSMIT skeleton file for Lab 7 of ECE 375
;*
;*  	Rock Paper Scissors
;* 	Requirement:
;* 	1. USART1 communication
;* 	2. Timer/counter1 Normal mode to create a 1.5-sec delay
;***********************************************************
;*
;*	 Author: Astrid Delestine & Lucas Plaisted
;*	   Date: 3/13/2023
;*
;***********************************************************

.include "m32U4def.inc"         ; Include definition file

;***********************************************************
;*  Internal Register Definitions and Constants
;***********************************************************
.def    mpr = r16               ; Multi-Purpose Register

; Use this signal code between two boards for their game ready
.equ    SendReady = 0b11111111

;***********************************************************
;*  Start of Code Segment
;***********************************************************
.cseg                           ; Beginning of code segment

;***********************************************************
;*  Interrupt Vectors
;***********************************************************
.org    $0000                   ; Beginning of IVs
	    rjmp    INIT            	; Reset interrupt


.org    $0056                   ; End of Interrupt Vectors

;***********************************************************
;*  Program Initialization
;***********************************************************
INIT:

    ; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

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

	;init the LCD
		rcall LCDInit
		rcall LCDBacklightOn
		rcall LCDClr


/*	I/O Ports
	;USART1
		; Need to set USCR1A, B, and C
		; USCR1A is a SREG
		; A: 
		; B: x00xxx00
			
		; C: xxxxxxxx
		; x's are bits that need to be set
		; 0's are status bits, no setting, only reading
*/
		; Set baudrate at 2400bps, double data rate
		; Asynchronous Double Speed mode eq:
/*	UBRR1 = fOSC/(8*BAUD)
		fOSC is just the system clock, so 8MHz
		BAUD is 2400
	UBRR1 = (8*10^6)/(8*2400) = 10^6/2400 = 416.66
	about 417 or 0b1_10100001
*/
			ldi mpr, 0b00000001
			out UBRRH1, mpr
			ldi mpr, 0b10100001
			out UBRRL1, mpr
		;Enable receiver and transmitter
			ldi r16, (1<<RXEN1)|(1<<TXEN1)	; from data sheet
			out UCSR1B,r16					; from data sheet
		;Set frame format: 8 data bits, 2 stop bits
			ldi r16, (1<<USBS1)|(3<<UCSZ10)	; from data sheet
			out UCSRnC,r16					; from data sheet
	;TIMER/COUNTER1
		;Set Normal mode

	;Other


;***********************************************************
;*  Main Program
;***********************************************************
MAIN:

	;TODO: ???

		rjmp	MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;***********************************************************
;*	Stored Program Data
;***********************************************************

;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING_START:
    .DB		"Welcome!"		; Declaring data in ProgMem
STRING_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

