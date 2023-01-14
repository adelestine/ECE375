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

int main(void)
{
	DDRB = 0b11110000 // set 7-4th bits as outputs
	PORTB = 0b01100000 // turn on LEDs connected to 5-6th bits

	while (1) // loop forever
	{
		uint8_t mpr = PIND & 0b00110000; // read and extract only 4-5 th bit
		if (mpr == 0b00100000) // check if the right whisker is hit
		{
			BotActionL(); // call BotAction
		}
		if (mpr == 0b00010000) // check if the right whisker is hit
		{
			BotActionR(); // call BotAction
		}
		// Your code goes here
		//more code here
	}
}


int BotActionL(){
	
}

int BotActionR(){
	
}