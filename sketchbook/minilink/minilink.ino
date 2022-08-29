#include <SPI.h>

#define LATCH 8
#define DATA 11

void setup() {
  // put your setup code here, to run once:
#if 0
  Serial.begin(115200);
  Serial.println("Minilink");
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);
#endif

  pinMode(LATCH, OUTPUT);
  pinMode(DATA, OUTPUT);
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red

  digitalWrite(DATA, LOW);

  digitalWrite(LATCH, HIGH);
  delay(2000);
  digitalWrite(LATCH, LOW);
  delay(2000);


}
