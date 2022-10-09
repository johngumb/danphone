#include <SPI.h>

#define ORANGE 8
#define YELLOW 7
#define GREEN 6
#define VIOLET 5
#define GREY 4
#define WHITE 3
#define GREENWHITE 2

#define IN1 GREEN

#define SRLATCH ORANGE
#define SROE YELLOW

#define TOPLATCH VIOLET
#define TOPOE GREY 

#define DATA 11
#define DATA_IN 12

#define LED 13

#define ADF4360STAT GREENWHITE

#define TXLATCH (~(1<<0))
#define RXLATCH (~(1<<1))
#define DCC_DRIVER_LATCH (~(1<<2))
#define DCC_PA_LATCH (~(1<<3))

void adf4360stat();
void adf4360();

void setup()
{
  // put your setup code here, to run once:
  Serial.begin(115200);
  Serial.println("Minilink");

  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2); // ??? seems to work
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  /* mid right 74LS595 */
  pinMode(SRLATCH, OUTPUT);
  pinMode(SROE, OUTPUT);

  /* top 74LS595 */
  pinMode(TOPLATCH, OUTPUT);
  pinMode(TOPOE, OUTPUT);

  pinMode(IN1, INPUT);

  pinMode(ADF4360STAT, INPUT);

  //pinMode(DATA, OUTPUT);
  //pinMode(LED, OUTPUT);

  digitalWrite(SRLATCH, HIGH);
  digitalWrite(TOPLATCH, HIGH); // goes through 4049 inverter directly onto top 74HC595 RCLK

  digitalWrite(SROE, LOW);
  digitalWrite(TOPOE, LOW); // goes through 4049 inverter directly onto top 74HC595 RCLK 
}

void latchselect(unsigned char latchid, unsigned char device)
{
  SPI.transfer(device);

  digitalWrite(latchid, HIGH);
  delay(10);
  digitalWrite(latchid, LOW);
  delay(10);
  digitalWrite(latchid, HIGH);
}

void latch(unsigned char oe, int level)
{
  digitalWrite(oe, level);
}

void write3(unsigned char v1, unsigned char v2, unsigned char v3)
{
   latch(SROE, HIGH);
   SPI.transfer(v1);
   SPI.transfer(v2);
   SPI.transfer(v3);
   latch(SROE, LOW);

  // TODO is this delay really necessary?
  delay(10);
}

void readfifo()
{
  unsigned char val1, val2;
  Serial.println("fifo");
  latch(SROE, HIGH);
  SPI.transfer(0x80);
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(SROE, LOW);
  
  Serial.println(val1,HEX);
  Serial.println(val2,HEX);
}

void readalmflag()
{
  unsigned char val1, val2;
  Serial.println("almflag");
  latch(SROE, HIGH);
  SPI.transfer(0xF8);
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(SROE, LOW);
  
  Serial.println(val1,HEX);
  Serial.println(val2,HEX);
}
void init_mesfet_dcc(unsigned char dcc_latch, unsigned char DAC2LSB)
{
  unsigned char val1, val2;
  Serial.print("init_mesfet_dcc 0x0");
  Serial.println((uint8_t)~dcc_latch, HEX);

  // see MAX11014 p69
  latchselect(SRLATCH, dcc_latch);
  write3(0x64, 0x00, 0x00); // Removes the global power-down.
  write3(0x64, 0x00, 0x00); // Powers up all parts of the MAX11014.
  write3(0x74, 0x00, 0x20); // Arms the full reset.
  write3(0x74, 0x00, 0x40); // Completes the full reset.
  write3(0x74, 0x00, 0x20); // Arms the full reset.
  write3(0x74, 0x00, 0x40); // Completes the full reset.
  
  delay(1);

  latch(SROE, HIGH);
  SPI.transfer(0xF6); // Read of FLAG register to verify reset good. Code should read 0xX042 if reset good.
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(SROE, LOW);
  
  Serial.println(val1,HEX);
  Serial.println(val2,HEX);

  readfifo();
  readfifo();
  readalmflag();

  write3(0x64, 0x00, 0x00); // Removes the global power-down.
  write3(0x64, 0x00, 0x00); // Powers up all parts of the MAX11014.

  // reverse engineered from Saleae
  write3(0x38, 0x00, 0x40);
  write3(0x3C, 0x00, 0x14);

  write3(0x4E, 0x00, DAC2LSB); // DAC2 // definitely has an effect.
  write3(0x4A, 0x00, 0x00); // DAC1
}

void adf4360stat()
{
  int v1;

  v1=digitalRead(ADF4360STAT);
  Serial.print("ADF4360 Lock ");
  Serial.println(v1);  
}

void adf4360()
{
  Serial.println("lock");
  // ADF4360
  latchselect(SRLATCH, 0xFF);
  latch(SROE, HIGH);
  delay(1);
  write3(0x81, 0xF1, 0x28);
  write3(0x00, 0x08, 0x21); // stock R value
  //write3(0x08, 0xCA, 0x02); // 1.8GHz 1048.4
  write3(0x07, 0x40, 0x22); // stock A B N // 643.483
  delay(1);
  latch(SROE, LOW);
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int v1;

  v1=digitalRead(IN1);

  // update messages as we update FPGA code
  Serial.print("Rx Lock AND Tx Lock ");
  Serial.println(v1);
//  Serial.print("PA Alarm ");
//  Serial.println(v2);


  latchselect(SRLATCH, TXLATCH);
  write3(0x8D, 0x80, 0x12);
  write3(0x00, 0x01, 0xA0);
  write3(0x05, 0xD9, 0x31); // 5.9895e9Hz
  //write3(0x05, 0xDC, 0x01); // 6.0GHz


  // transmit on 10.0GHz
  

  // Rx IF = 140MHz. Possible range 115 - 170 MHz; 949 MHz to 1004MHz
  latchselect(SRLATCH, RXLATCH);
  write3(0x8D, 0x80, 0x12);
  write3(0x00, 0x01, 0xA0);
  write3(0x04, 0xA6, 0x01); // 4.76e9Hz
  //write3(0x04, 0x8B, 0x21); // 4.653GHz *2 = 9.306GHz -0.14 = 9.166GHz + 0.834 = 10.0GHz

#if 1
  // IF = 140MHz.

  init_mesfet_dcc(DCC_DRIVER_LATCH, 0x00);
  init_mesfet_dcc(DCC_PA_LATCH, 0x00);

  //latchselect(SRLATCH, RXLATCH);
  //latch(SROE, HIGH);
  //delay(10);
  //latch(SROE, LOW);
  //delay(100);
  //delay(500);

#if 0
#define TST WHITE
  digitalWrite(TST, LOW);
  delay(1000);
  digitalWrite(TST, HIGH);
#endif
#endif

  adf4360();

  delay(1000);
  adf4360stat();

  v1=digitalRead(IN1);

  // update messages as we update FPGA code
  Serial.print("Rx Lock AND Tx Lock ");
  Serial.println(v1);

  //test
  //latchselect(TOPLATCH, 0x00);
  //latch(TOPOE, HIGH); // enable output

  delay(1000);
  //adf4360stat();
}
