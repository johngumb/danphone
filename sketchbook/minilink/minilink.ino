#include <SPI.h>

#define LATCH 8
#define DATA 11

void setup() {
  // put your setup code here, to run once:
  Serial.begin(115200);
  Serial.println("Minilink");
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  pinMode(LATCH, OUTPUT);
  pinMode(DATA, OUTPUT);
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data

  Serial.println("writing...");
  SPI.transfer(0x01);
  delay(1000);
}
