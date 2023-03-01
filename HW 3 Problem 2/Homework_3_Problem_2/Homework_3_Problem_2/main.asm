;***********************************************************
;*
;*	 Author: Lucas Plaisted
;*	   Date: 2/27/2023
;*
;*		Modified from the homework example
;***********************************************************

.include "m32U4def.inc"			; Include definition file

.def mpr = r16 
.def temp = r17 
INIT: 

    ; Initialize the Stack Pointer (VERY IMPORTANT!!!!)
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND


; R = reserved, just write 0's
; CTC mode, WGM02:0 = 0b010
; TCCR0A <- 0bxxxxRR10
; TCCR0B <- 0bxxRR0xxx
; In CTC mode the counter is cleared to zero when the counter value (TCNT0) matches the
;	OCR0A. The OCR0A defines the top value for the counter, hence also its resolution.

; For generating a waveform output in CTC mode, the OC0A output can be set to toggle its logical level on each
;	Compare Match by setting the Compare Output mode bits to toggle mode (COM0A1:0 = 1) 
; TCCR0A <- 0b01xxRR10
; TCCR0B <- 0bxxRR0xxx
; This means that the wave will need to have double the required frequency.

; B7 and B6 are not needed, they force output compares
; TCCR0A <- 0b01xxRR10
; TCCR0B <- 0b00RR0xxx

; We are not working with OC0B, so those related bits are 0's
; TCCR0A <- 0b0100RR10
; TCCR0B <- 0b00RR0xxx

; Last 3 bits are the prescale!
; Lowest frequency is 440Hz
; Highest frequency is 587Hz
; f_OC0A = 8MHz/(2*Prescale*(1+OCR0A))
; From lots of math done in my notes, prescale should be 8, which is 0b010 
; TCCR0A <- 0b01000010
; TCCR0B <- 0b00000010
; is the final number!
ldi mpr, 0b01000010
out TCCR0A, mpr
ldi mpr, 0b00000010
out TCCR0A, mpr

;The OC0A value will not be visible on the port pin unless the data direction for the pin is set to output.
ldi mpr, 0b10000000 ; OC0A is B7
out DDRB, mpr 

; I/O ports


ldi mpr, 0b11110000 ; set pins 7-4 as input 
out DDRD, mpr 
ldi mpr, 0b11110000 ; enable pull-up resistor for pins 7-4
out PORTD, mpr 


MAIN: 
	in mpr, PIND
	sbrc	mpr, 7
	rjmp	NEXT1
	ldi	temp, 141
NEXT1:
	sbrc	mpr, 6
	rjmp	NEXT2
	ldi temp, 126
NEXT2:
	sbrc	mpr, 5
	rjmp	NEXT3
	ldi temp, 118
NEXT3:	
	sbrc	mpr, 4
	rjmp	MAIN
	ldi temp, 105
	out OCR0A, temp

rjmp MAIN
