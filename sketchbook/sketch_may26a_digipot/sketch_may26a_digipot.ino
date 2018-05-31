//#include "MCP4161.h"

#ifndef MCP4161_h
#define MCP4161_h

//#include "WProgram.h"
#include <SPI.h>

#define MCP4161_MIN 0
#define MCP4161_MAX 256

const int ledpin=13;
const int miso=50;
const int datapin=51;
const int clockpin=52;
const int latchpin = 53;

class MCP4161
{
public:
    MCP4161(int csPin);
	int initTCON();
	int readTCON();
	int readStatus();
	int increment();
	int decrement();
	int setTap(int value);
        int getTap();
	
private:
    int _cs;
	void enable();
	void disable();
};

#endif

MCP4161::MCP4161(int csPin)
{
	_cs = csPin;
	pinMode(_cs, OUTPUT);
	disable();

	//Increase the frequency when external pull ups are used
#if 0
	SPI.begin();
	SPI.setBitOrder(MSBFIRST);
	SPI.setDataMode(SPI_MODE0);
	SPI.setClockDivider(SPI_CLOCK_DIV64);
#endif
}

void MCP4161::enable() {
	// take the SS pin low to select the chip:
	digitalWrite(_cs, LOW);
}

void MCP4161::disable() {
	// take the SS pin high to de-select the chip:
	digitalWrite(_cs, HIGH);
}

int MCP4161::increment() {
	enable();
	
	//  send in the address and value via SPI:
	byte ret1 = SPI.transfer(0x04);

	disable();
	return ret1;
}

int MCP4161::decrement() {
	enable();
	
	//  send in the address and value via SPI:
	byte ret1 = SPI.transfer(0x08);
	
	disable();
	return ret1;
}

int MCP4161::readTCON() {
	enable();
	
	//  send in the address and value via SPI:
	byte ret1 = SPI.transfer(0x4C);
	byte ret2 = SPI.transfer(0x00);
	
	disable();
	return ret1;
}

int MCP4161::initTCON() {
	enable();
	
	//  send in the address and value via SPI:
	byte ret1 = SPI.transfer(0x40);
	byte ret2 = SPI.transfer(0x0F);
	//byte ret2 = SPI.transfer(0x00);

	disable();
	return ret1;
}

int MCP4161::readStatus() {
	enable();
	
	//  send in the address and value via SPI:
	byte ret1 = SPI.transfer(0x5C);
        // must write FF here so 4K7 resistor takes effect
	byte ret2 = SPI.transfer(0xFF);
	
	disable();
        Serial.print(ret1,HEX);
        Serial.print(" status ");
        Serial.print(ret2,HEX);
        Serial.println();
	return ret1;
}

int MCP4161::setTap(int value) {
	enable();

	//  send in the address and value via SPI:
	byte h = 0x03 & (value >> 8);
	byte l = 0x00FF & value;
	
	//Serial.print("HIGH: ");
	//Serial.println(h, BIN);
	//Serial.print("LOW: ");
	//Serial.println(l, BIN);
	
	byte ret1 = SPI.transfer(h);
	byte ret2 = SPI.transfer(l);

	disable();	
	return (ret1 << 8) | ret2;
}

int MCP4161::getTap() {
	enable();

	//  send in the address and value via SPI:
	byte h = 0x0C;
	byte l = 0xFF;
	
	//Serial.print("HIGH: ");
	//Serial.println(h, BIN);
	//Serial.print("LOW: ");
	//Serial.println(l, BIN);
	
	byte ret1 = SPI.transfer(h);
	byte ret2 = SPI.transfer(l);

	disable();	
        Serial.print(ret1,HEX);
        Serial.print(" ");
        Serial.print(ret2,HEX);
        Serial.println();
        
        return (ret1 << 8) | ret2;
}

void setup()
{
	SPI.begin();
	SPI.setBitOrder(MSBFIRST);
	SPI.setDataMode(SPI_MODE0);
	SPI.setClockDivider(SPI_CLOCK_DIV128);


    pinMode(ledpin, OUTPUT);
    pinMode(datapin,OUTPUT);
    pinMode(clockpin,OUTPUT);
    pinMode(latchpin,OUTPUT);
    pinMode(miso,INPUT);
    
    digitalWrite(ledpin, LOW);
    
    //SPI.begin();
    Serial.begin(115200);
    Serial.println("begin");
}


void loop()
{
  MCP4161 fred(53);
  fred.initTCON();
  fred.setTap(150);
  fred.getTap();
  fred.readStatus();

  digitalWrite(ledpin, HIGH);

#if 0
  for (int i=0;i<200;i++)
  {
    delay(10);
  //  fred.increment();
  }
#endif

   delay(200); 
      digitalWrite(ledpin, LOW);
   delay(200);
}
