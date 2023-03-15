
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
.def	ilcnt = r18				
.def	olcnt = r19

; Use this signal code between two boards for their game ready
.equ    SendReady = 0b11111111
.equ	lcdL1 = 0x00			; Make LCD Data Memory locations constants
.equ	lcdH1 = 0x01
.equ	lcdL2 = 0x10			; lcdL1 means the low part of line 1's location
.equ	lcdH2 = 0x01			; lcdH2 means the high part of line 2's location
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
		Need to set USCR1B and C
		B: x00xxx00 -> 0b0_00_1_1_0_00
			2:	 USCZ12
			3:	 TXEN1: Transmitter enable
			4:	 RXEN1: Receiver enable
			7:	 RXCIE1: Receive complete interrupt enable flag,
						 enable if using interrupts
		C: xxxxxxxx	-> 0b00_00_1_11_0
			0:	 UPOL1: Clock Polarity
			2-1: USCZ11 and USCZ10
			3:	 USBS1 stop bit select
			5-4: UPM1 parity mode
			7-6: UMSEL1 USART mode select
		x's are bits that need to be set
		0's are status bits, no setting, only reading
	USCZ1:	011 for 8 bit
	UMSEL1:	00 for asynchronous
	UMP1:	00 for disbled
	USBS1:	1 for 2-bit
	USPOL1:	0 for rising edge
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

			ldi mpr, 0b0_00_1_1_0_00
			out UCSR1B, mpr
			ldi mpr, 0b00_00_1_11_0
			out UCSR1C, mpr	
				
	;TIMER/COUNTER1
		;Set Normal mode, WGM13:0 = 0b000
/*
TIMER MATH
	Need 1.5sec delay
	Max count of 2^16-1 = 65,535
	65,535/1.5 = 43690 counts/sec ideal, lower is okay
	CPU @ 8MHz = 8*10^6 counts/sec
	8*10^6/prescale <= 43690
	prescale >= 8*10^6/43690
	prescale >= 183
	prescale should be 256 :)
	WGM1 = 0b100
	at 256 prescale how much we counting?
	x/(8MHz/256) = 1.5s
	x = 1.5s(8Mhz/256) = 46,875
	so we need to load 65535-46875 = 18660
	into the counter in order to have it count for the
	correct amount of time
	
	In two 8-bit numbers, that value is
	High: 0b01001000
	Low:  0b11100100
*/
	; Configure 16-bit Timer/Counter 1A and 1B
			; TCCRIA Bits:
				; 7:6 - Timer/CounterA compare mode, 00 = disabled
				; 5:4 - Timer/CounterB compare mode, 00 = disabled
				; 3:2 - Timer/CounterC compare mode, 00 = disabled
				; 1:0 - Wave gen mode low half, 00 for normal mode
			ldi mpr, 0b00_00_00_00
			sts TCCR1A, mpr
			; TCCRIB Bits:
				; 7:5 - not relevant, 0's
				; 4:3 - Wave gen mode high half, 00 for normal
				; 2:0 - Clock selection, 100 = 256 prescale
			ldi mpr, 0b000_00_100
			sts TCCR1B, mpr

	; Load text data from program mem to data mem for easy access
	ldi YH, high(STRING1)
	ldi YL, low(STRING1)
	lsl YH		; shift for program mem access
	lsl YL
	clr mpr
	adc YH, mpr ; shift carry from lower byte to upper byte
	ldi ZH, high(DATAMEMBEG)
	ldi ZL, low(DATAMEMBEG)
	
	ldi ilcnt, 


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
STRING1:
.DB		"Welcome!        "
STRING2:
.DB		"Please press PD7"
STRING3:
.DB		"Ready. Waiting  "
STRING4:
.DB		"for the opponent"
STRING5:
.DB		"Game start      "
STRING6:
.DB		"Rock            "
STRING7:
.DB		"Paper           "
STRING8:
.DB		"Scissor         "
STRING9:
.DB		"You won!        "
STRING10:
.DB		"You lost        "
STRING11:
.DB		"Draw            "

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
DATAMEMBEG:
welcome:	.byte 2
press:		.byte 2
ready:		.byte 2
for:		.byte 2
start:		.byte 2
rock:		.byte 2
paper		.byte 2
scissor:	.byte 2
win:		.byte 2
lose:		.byte 2
draw:		.byte 2 

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

