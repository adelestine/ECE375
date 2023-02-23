;***********************************************************
;*	This is the skeleton file for Lab 5 of ECE 375
;*
;*	 Author: Astrid Delestine & Lucas Plaisted
;*	   Date: 2/23/2023
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;************************************************************
;* Variable and Constant Declarations
;************************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	waitcnt = r17				; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter
.def	hlcnt = r15				; Hit Left Counter
.def	hrcnt = r14				; Hit Right Counter
;.def	count = r20				; needed for LCD binToASCII

.equ	WTime = 50				; Time to wait in wait loop

.equ	WskrR = 4				; Right Whisker Input Bit
.equ	WskrL = 5				; Left Whisker Input Bit
.equ	EngEnR = 5				; Right Engine Enable Bit
.equ	EngEnL = 6				; Left Engine Enable Bit
.equ	EngDirR = 4				; Right Engine Direction Bit
.equ	EngDirL = 7				; Left Engine Direction Bit

;//TAKEN FROM LAB3

.equ	lcdL1 = 0x00	; Make LCD Data Memory locations constants
.equ	lcdH1 = 0x01
.equ	lcdL2 = 0x10	; lcdL1 means the low part of line 1's location
.equ	lcdH2 = 0x01 ; lcdH2 means the high part of line 2's location
.equ	lcdENDH = 0x01	; as it sounds, the last space in data mem
.equ	lcdENDL = 0x1F		; for storing lcd text

;//END TAKEN FROM LAB3

.equ	strSize = 4;


;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used
		

		; This is just an example:
;.org	$002E					; Analog Comparator IV
;		rcall	HandleAC		; Call function to handle interrupt
;		reti					; Return from interrupt
.org	$0002 ;INT0
		rcall	HitRight		;RIGHT WHISKER
		reti
.org	$0004 ;INT1
		rcall	HitLeft		;LEFT WHISKER
		reti
;.org	$0006 ;INT2
.org	$0008 ;INT3
		rcall	ClearCounters		;CLEAR COUNTERS
		reti
;.org	$000E ;INT6

.org	$0056					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
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
		rcall toLCD

		rcall ClearCounters


	; Initialize external interrupts
		; Set the Interrupt Sense Control to falling edge
		ldi mpr, 0b10001010
		sts EICRA, mpr;

	; Configure the External Interrupt Mask
		ldi mpr, 0b0000_1011 ; x0xx_0000 ; all disabled
		out EIMSK, mpr;
	; Turn on interrupts
		; NOTE: This must be the last thing to do in the INIT function
	sei ; Turn on interrupts

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr

		rjmp	MAIN			; Create an infinite while loop to 
								; signify the end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the
;	left whisker interrupt, one to handle the right whisker
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;		beginning of your functions
;-----------------------------------------------------------
ClearCounters:							; Begin a function with a label

		; Save variable by pushing them to the stack

		; Execute the function here
		clr		hrcnt			; sets hlcnt and hrcnt to zero by 
		clr		hlcnt			; doing an xor operation with itself

		push ZL				; Save vars to stack
		push ZH
		push XL
		push XH
		push mpr
		push ilcnt

		ldi  ZL , low(STRING_BEG<<1)	; Sets ZL to the low bits  
			 					; of the first string location
		ldi  ZH , high(STRING_BEG<<1)	; Sets ZH to the first 
								; of the first string location
		ldi  XH , lcdH1
		ldi  XL , lcdL1
		ldi  ilcnt , 16

CCl1: ; While ilcnt != zero 1
		lpm  mpr, Z+
		st   X+ , mpr
		dec  ilcnt
		brne CCl1

		ldi	ZL, low(STRING2_BEG<<1)
		ldi ZH, high(STRING2_BEG<<1)
		;z is already pointing at the second 
		;string due to how memory is stored
		ldi  XH , lcdH2
		ldi  XL , lcdL2
		ldi  ilcnt , 16

CCl2: ; While ilcnt != zero 2
		lpm  mpr, Z+
		st   X+ , mpr
		dec  ilcnt
		brne CCl2

		rcall LCDWrite

		ldi mpr , 0b0000_0111
		out EIFR, mpr

		pop ilcnt
		pop mpr
		pop XH
		pop XL
		pop ZH
		pop ZL					; Pop vars off of stack
		; Restore variable by popping them from the stack
		; in reverse order

		ret						; End a function with RET


;-----------------------------------------------------------
; Func: toLCD
; Desc: Takes various info and pushes it to the LCD
;		*HL#:0 
;		*HR#:0
;-----------------------------------------------------------
toLCD:
		push ZL				; Save vars to stack
		push ZH
		push XL
		push XH
		push mpr
		push ilcnt

		; Sets ZL to the low bits of the first string location
		ldi  ZL , low(STRING_BEG<<1)
		ldi  ZH , high(STRING_BEG<<1)
		;points to the data location where LCD draws from
		ldi  XH , lcdH1
		ldi  XL , lcdL1
		ldi  ilcnt , 4

Line1Loop: ; While ilcnt != zero
		lpm  mpr, Z+
		st   X+ , mpr
		dec  ilcnt
		brne Line1Loop
		//end loop

		mov mpr, hlcnt; copies the counter to mpr

		rcall Bin2ASCII
		; Takes a value in MPR and outputs  
		; the ascii equivilant to XH:XL
		; convineintly X is currently pointing where 
		; I would like this number to go


		ldi	ZL, low(STRING2_BEG<<1)
		ldi ZH, high(STRING2_BEG<<1)

		ldi  XH , lcdH2
		ldi  XL , lcdL2
		ldi  ilcnt , 4

Line2Loop: ; While ilcnt != zero 2
		lpm  mpr, Z+
		st   X+ , mpr
		dec  ilcnt
		brne Line2Loop


		mov mpr, hrcnt;
		rcall Bin2ASCII

		rcall LCDWrite






		pop ilcnt
		pop mpr
		pop XH
		pop XL
		pop ZH
		pop ZL					; Pop vars off of stack


		ret
;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, (WTime<<1)	; Shifted bit back by 1, 
									; making the wait time two seconds
		rcall	Wait			; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr

		inc		hrcnt;
		rcall	toLCD;
		;fix debounce
		ldi mpr , 0b0000_0111
		out EIFR, mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	mpr			; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG	; Save program state
		push	mpr			;

		; Move Backwards for a second
		ldi		mpr, MovBck	; Load Move Backward command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, (WTime<<1)	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR	; Load Turn Left Command
		out		PORTB, mpr	; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	Wait			; Call wait function

		; Move Forward again
		ldi		mpr, MovFwd	; Load Move Forward command
		out		PORTB, mpr	; Send command to port

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr

		inc		hlcnt	;
		rcall	toLCD;
				;fix debounce
		ldi mpr , 0b0000_0111
		out EIFR, mpr
		ret				; Return from subroutine

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



;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label

		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here
;.org 
STRING_BEG:
.DB		"HL#:0           "		; Declaring data in ProgMem
STRING2_BEG:
.DB		"HR#:0           "
STRING_END:


;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

