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

  digitalWrite(SRLATCH, LOW);
  digitalWrite(SROE, HIGH);

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
  if (level)
      digitalWrite(SROE, HIGH);
  else
      digitalWrite(SROE, LOW);
}

void tx_synth_write(unsigned char v1, unsigned char v2, unsigned char v3)
{
   txlatch(LOW);
   SPI.transfer(v1);
   SPI.transfer(v2);
   SPI.transfer(v3);
   txlatch(HIGH);

  delay(10);
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

#if 0
  Serial.println("LOW");
  txlatch(LOW);
  delay(5000);

  Serial.println("HIGH");
  txlatch(HIGH);
  delay(5000);
#endif

  tx_synth_write(0x8D, 0x80, 0x12);
  tx_synth_write(0x00, 0x01, 0xA0);
  tx_synth_write(0x05, 0xD9, 0x31);

  //txlatch(LOW);
  

  //digitalWrite(LED, digitalRead(IN2));

  delay(500);
}
