
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
;DO NOT USE 20-22
.def    mpr = r16               ; Multi-Purpose Register
.def	ilcnt = r18				
.def	olcnt = r19
.def	zero = r2
.def	userChoice = r17
; Use this signal code between two boards for their game ready
.equ    SendReady = 0b11111111
.equ	lcd1L = 0x00			; Make LCD Data Memory locations constants
.equ	lcd1H = 0x01
.equ	lcd2L = 0x10			; lcdL1 means the low part of line 1's location
.equ	lcd2H = 0x01			; lcdH2 means the high part of line 2's location
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
	; Most important thing possible!!!!!
		clr		zero
		clr		userChoice
			;)
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
			sts UBRR1H, mpr
			ldi mpr, 0b10100001
			sts UBRR1L, mpr

			ldi mpr, 0b0_00_1_1_0_00
			sts UCSR1B, mpr
			ldi mpr, 0b00_00_1_11_0
			sts UCSR1C, mpr	
				
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
	ldi ZH, high(STRING1)
	ldi ZL, low(STRING1)
	lsl ZH		; shift for program mem access
	lsl ZL
	adc ZH, zero ; shift carry from lower byte to upper byte
	ldi YH, high(DATAMEMBEG)
	ldi YL, low(DATAMEMBEG)
		; Z has the loading address, Y the offloading address
		; Need to load 16*number of phrases letters
		;	16*11 = 176
	ldi ilcnt, 176
LOADLOOP:
		lpm mpr, Z+	; load letter into mpr
		st Y+, mpr	; store letter into data meme
		dec ilcnt	; count 1 more done
		cp ilcnt, zero	; are we done yet
		brne LOADLOOP
	
	
	

;***********************************************************
;*  Main Program
;***********************************************************
MAIN:
	
	sbic PIND, 7
	rjmp MAIN
	clr mpr
	;check to see if buffer is empty
	sbic USCR1A, UDRE1 ; skips next instuctuon if no data in USART
	rcall USART_RX;
	cpi mpr, $FF
	brne p1 ;if equal this is p2 if not equal this is p1
p2: 
	ldi mpr, $FF
	rcall USART_TX ; send confirmation
	rcall GAMESTART
	rjmp main
p1:
	;send USART FF
	ldi mpr, $FF
	rcall USART_TX
	;clear rxc1
	sbr UCSR1A, 0b1000_0000
	;wait for reccived signal of FF
	rcall USART_RX
	cpi mpr, $FF
	breq p2

	



	

	rjmp	MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************


USART_TX:
	;sbis USCR1A, UDRE1	;loops as long as there are bits in the 
						;USART register.
	;rjmp USART_TX
	;load data into usart ouptut buffer
	out	UDR1, mpr
	ret

USART_RX:
	sbis USCR1A, RXC1
	rjmp USART_RX
	;get data from usart into mpr
	in	mpr, UDR1
	ret




GAMESTART:
	;clear data in USART reg

	;clear flags for USART
	;start clock for timer






;***********************************************************
;*		Write Screen
;*	Writes two words to the screen, assuming that they are
;*	stored in data memory (done by INIT) and that the low
;*	bytes of their addresses are stored in the stack.
;*	
;*	The first push/second pop is the top line phrase, and
;*	the second push/first pop is the bottom line phrase.
;*
;*	If the value popped from the stack is $FF, that line
;*	will not be written.
;*
;*		EXTREMELY destructive of register contents...
;*			Use with caution.
;***********************************************************
WRITESCREEN:
	rcall LCDClr
	ldi ZH, $01	; load ZH with upper byte of phrase address
	pop mpr		; mpr has lower byte
	cpi mpr, $FF	; if mpr != FF
	breq SKIPWRITE1	
		ldi YH, lcd2H	; load Y with line 2 location
		ldi YL, lcd2L

SKIPWRITE1:
	ldi YH, lcd2H	; load Y with line 2 location
	ldi YL, lcd2L

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
paper:		.byte 2
scissor:	.byte 2
win:		.byte 2
lose:		.byte 2
draw:		.byte 2 

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

