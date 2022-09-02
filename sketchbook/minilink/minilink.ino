#include <SPI.h>

#define SRLATCH 8 // orange
#define SROE 7 // yellow 

#define IN1 6 // green
#define IN2 5 // blue
#define DATA 11

#define LED 13

void setup() {
  // put your setup code here, to run once:
  Serial.begin(115200);
  Serial.println("Minilink");

  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  pinMode(SRLATCH, OUTPUT);
  pinMode(SROE, OUTPUT);
  pinMode(IN1, INPUT);
  pinMode(IN2, INPUT);

  pinMode(DATA, OUTPUT);
  pinMode(LED, OUTPUT);

  //digitalWrite(SRLATCH, LOW);
  //digitalWrite(SROE, LOW);

  txlatchselect(~0x01);
}

void txlatchselect(unsigned char device)
{
  SPI.transfer(device);

  digitalWrite(SRLATCH, LOW);
  delay(10);
  digitalWrite(SRLATCH, HIGH);
  delay(10);
  digitalWrite(SRLATCH, LOW); 
}

void txlatch(int level)
{
  Serial.print("tx latch ");
  Serial.println(level);

  if (level)
      digitalWrite(SROE, HIGH);
  else
      digitalWrite(SROE, LOW);
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int v1, v2;

  v1=digitalRead(IN1);
  v2=digitalRead(IN2);
  Serial.print("IN1 ");
  Serial.println(v1);
  Serial.print("IN2 ");
  Serial.println(v2);

  Serial.println("LOW");
  txlatch(LOW);
  delay(5000);

  Serial.println("HIGH");
  txlatch(HIGH);
  delay(5000);
  

  //digitalWrite(LED, digitalRead(IN2));

  //delay(5000);
}
