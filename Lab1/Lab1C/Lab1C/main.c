/*
 * Lab1C.c
 *
 * Created: 1/14/2023 12:51:47 PM
 * Author : Astrid Delestine and Lucas Plaisted
 */ 

/*
This code will cause a TekBot connected to the AVR board to
move forward and when it touches an obstacle, it will reverse
and turn away from the obstacle and resume forward motion.

PORT MAP
Port B, Pin 5 -> Output -> Right Motor Enable
Port B, Pin 4 -> Output -> Right Motor Direction
Port B, Pin 6 -> Output -> Left Motor Enable
Port B, Pin 7 -> Output -> Left Motor Direction
Port D, Pin 5 -> Input -> Left Whisker
Port D, Pin 4 -> Input -> Right Whisker
*/

#define F_CPU 16000000
#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>

const int FORWARD = 0b10010000, HALT = 0b11110000, BACKWARD = 0b00000000, RIGHT = 0b00010000, LEFT = 0b10000000; // Led final integer values

void BotActionL();
void BotActionR();
void goBackwards2Sec();

int main(void)
{
	DDRB = 0b11110000; // set 7-4th bits as outputs
	//PORTB = 0b01100000; // turn on LEDs connected to 5-6th bits
	DDRD = 0b00000000; // set 5th and 4th pins on D as inputs
	PORTD = 0b11110000; //enable pull up resistors for port D pins 7-4
	

	while (1) // loop forever
	{
		uint8_t mpr = PIND & 0b00110000; // read and extract only 4-5 th bit
		mpr = ~mpr; //flip bits since PINDD is active low
		if (mpr & 0b00010000) // check if the right whisker is hit
		{
			BotActionR(); // call BotAction
		}
		else if (mpr & 0b00100000) // check if the left whisker is hit
		{
			BotActionL(); // call BotAction
		}
		PORTB = FORWARD; //resume forward movement
		_delay_ms(50); //delay for 50ms to help prevent switch bouncing
	}
}


void BotActionL(){
	goBackwards2Sec(); //self explanatory
	PORTB = LEFT; //left motor forwards, right motor backwards = turn right
	_delay_ms(1000); //wait 1 second
	return;
}

void BotActionR(){
	goBackwards2Sec(); //self explanatory :)
	PORTB = RIGHT; //right motor forwards, left motor backwards = turn left
	_delay_ms(1000); //wait 1 second
	return;
}

void goBackwards2Sec(){
	PORTB = BACKWARD; //turn both motors to reverse
	_delay_ms(2000); //delay for 2 seconds
	return;
}