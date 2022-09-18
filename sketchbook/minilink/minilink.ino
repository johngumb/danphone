#include <SPI.h>

#define SRLATCH 8 // orange
#define SROE 7 // yellow 

#define IN1 6 // green
#define VIOLET 5
#define GREY 4
#define WHITE 3

#define DATA 11
#define DATA_IN 12

#define LED 13

#define TXLATCH (~(1<<0))
#define RXLATCH (~(1<<1))
#define DCC_DRIVER_LATCH (~(1<<2))
#define DCC_PA_LATCH (~(1<<3))

void setup()
{
  // put your setup code here, to run once:
  Serial.begin(115200);
  Serial.println("Minilink");

  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2); // ??? seems to work
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  pinMode(SRLATCH, OUTPUT);
  pinMode(SROE, OUTPUT);
  pinMode(IN1, INPUT);
  //pinMode(IN2, INPUT);

  //pinMode(DATA, OUTPUT);
  pinMode(LED, OUTPUT);

  pinMode(VIOLET, OUTPUT);
  pinMode(GREY, OUTPUT);
  pinMode(WHITE, OUTPUT);

  digitalWrite(SRLATCH, LOW);
  digitalWrite(SROE, HIGH);
}

void latchselect(unsigned char device)
{
  SPI.transfer(device);

  digitalWrite(SRLATCH, LOW);
  delay(10);
  digitalWrite(SRLATCH, HIGH);
  delay(10);
  digitalWrite(SRLATCH, LOW); 
}

void latch(int level)
{
  if (level)
      digitalWrite(SROE, HIGH);
  else
      digitalWrite(SROE, LOW);
}

void write3(unsigned char v1, unsigned char v2, unsigned char v3)
{
   latch(LOW);
   SPI.transfer(v1);
   SPI.transfer(v2);
   SPI.transfer(v3);
   latch(HIGH);

  // TODO is this delay really necessary?
  //delay(10);
}

void readfifo()
{
  unsigned char val1, val2;
  latch(LOW);
  SPI.transfer(0x80);
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(HIGH);
  
  Serial.println(val1,HEX);
  Serial.println(val2,HEX);
}
void init_mesfet_dcc(unsigned char dcc_latch)
{
  unsigned char val1, val2;
  Serial.println("init_mesfet_dcc");
 
  // see MAX11014 p69
  latchselect(dcc_latch);
  write3(0x64, 0x00, 0x00); // Removes the global power-down.
  write3(0x64, 0x00, 0x00); // Powers up all parts of the MAX11014.
  write3(0x74, 0x00, 0x20); // Arms the full reset.
  write3(0x74, 0x00, 0x40); // Completes the full reset.
  write3(0x74, 0x00, 0x20); // Arms the full reset.
  write3(0x74, 0x00, 0x40); // Completes the full reset.
  
  delay(1);

  latch(LOW);
  SPI.transfer(0xF6); // Read of FLAG register to verify reset good. Code should read 0xX042 if reset good.
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(HIGH);
  
  Serial.println(val1,HEX);
  Serial.println(val2,HEX);

  readfifo();
  readfifo();

  write3(0x64, 0x00, 0x00); // Removes the global power-down.
  write3(0x64, 0x00, 0x00); // Powers up all parts of the MAX11014.

  // reverse engineered from Saleae
  write3(0x38, 0x00, 0x40);
  write3(0x3C, 0x00, 0x14);
  write3(0x4E, 0x00, 0x00);
  write3(0x4A, 0x00, 0x00);
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int v1;
#if 1
  v1=digitalRead(IN1);

  // update messages as we update FPGA code
  Serial.print("Rx Lock AND Tx Lock ");
  Serial.println(v1);
//  Serial.print("PA Alarm ");
//  Serial.println(v2);

  latchselect(TXLATCH);
  write3(0x8D, 0x80, 0x12);
  write3(0x00, 0x01, 0xA0);
  write3(0x05, 0xD9, 0x31);

  latchselect(RXLATCH);
  write3(0x8D, 0x80, 0x12);
  write3(0x00, 0x01, 0xA0);
  write3(0x04, 0xA6, 0x01);

  init_mesfet_dcc(DCC_DRIVER_LATCH);
#endif

  //latchselect(RXLATCH);
  //latch(LOW);
  //delay(10);
  //latch(HIGH);
  //delay(100);
  //delay(500);

#if 0
#define TST WHITE
  digitalWrite(TST, LOW);
  delay(1000);
  digitalWrite(TST, HIGH);
#endif

  delay(500);
}
