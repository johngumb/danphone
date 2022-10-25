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
#define AD5318_DAC_LATCH (~(1<<5))

void adf4360stat();
void adf4360();
void ad5318_dac_init();
void ad5318_dac_write(uint8_t dacno, uint16_t val);

void setup()
{
  // put your setup code here, to run once:
  Serial.begin(115200);
  Serial.println("Minilink jag");

  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE2); // ??? MODE2 seems to work
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

  ad5318_dac_init();
}

void latchselect(unsigned char latchid, unsigned char device)
{
  SPI.transfer(device);

  digitalWrite(latchid, HIGH);
  digitalWrite(latchid, LOW);
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
}

void readfifo()
{
  unsigned char val1, val2, chan;
  int result,offset=0;
  Serial.println("fifo");
  latch(SROE, HIGH);
  SPI.transfer(0x80);
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(SROE, LOW);

  chan=(val1&0xF0)>>4;

  switch(chan)
  {
    case 0:
      Serial.print("Internal temp ");
      offset=-273;
      break;
    case 1:
      Serial.print("CH1 ext temp ");
      offset=-273;
      break;
    case 2:
      Serial.print("CH1 sense voltage ");
      break;
    case 3:
      Serial.print("CH1 DAC input voltage ");
      break;
    case 4:
      Serial.print("CH1 gate voltage ");
      break;
    case 5:
      Serial.print("ADCIN1 voltage ");
      break;
    case 6:
      Serial.print("CH2 ext temp ");
      offset=-273;
      break;
    case 7:
      Serial.print("CH2 sense voltage ");
      break;
    case 8:
      Serial.print("CH2 DAC input voltage ");
      break;
    case 9:
      Serial.print("CH2 gate voltage ");
      break;
    case 10:
      Serial.print("ADCIN2 voltage ");
      break;
    case 11:
      Serial.print("Reserved ");
      break;
    case 12:
      Serial.print("LUT Data ");
      break;
    case 14:
      Serial.print("Corrupt ");
      break;
    case 15:
      Serial.print("Empty FIFO ");
      break;
  }

  result=((val1&0x0F)<<8)+val2;
  Serial.println(result+offset);

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


  readalmflag();

  write3(0x64, 0x00, 0x00); // Removes the global power-down.
  write3(0x64, 0x00, 0x00); // Powers up all parts of the MAX11014.

  // reverse engineered from Saleae
  write3(0x38, 0x00, 0x40);
  write3(0x3C, 0x00, 0x14);

  write3(0x4E, 0x03, DAC2LSB); // DAC2 // definitely has an effect.
  write3(0x4A, 0x00, DAC2LSB); // DAC1

#define ADCCON 0x62
  write3(ADCCON, 0xFF, 0xFF);

  delay(1000);
  readfifo();
  delay(1000);
  readfifo();
  delay(1000);
  readfifo();
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
  Serial.println("adf4360");
  // ADF4360
  latchselect(SRLATCH, 0xFF); // when SROE goes high below, ensure no latches are visible

  // HACK re-use SROE as latch for ADF4360. Avoid unwanted transitions by teaming this signal with SRLATCH
  digitalWrite(SRLATCH, LOW);  // SRLATCH must be high in FPGA for SROE to act as adf4360 LE signal
  latch(SROE, HIGH);
  write3(0x81, 0xF1, 0x28);
  write3(0x00, 0x08, 0x21); // stock R value
  write3(0x08, 0xCA, 0x02); // 1.8GHz 1048.4
  //write3(0x07, 0x40, 0x22); // stock A B N // 643.483, 1485MHz

  latch(SROE, LOW);

  latchselect(SRLATCH, 0xFF);
}

void ad5318_dac_init(void)
{
  uint16_t ldac_mode=0;
  latchselect(SRLATCH, AD5318_DAC_LATCH);
  SPI.setDataMode(SPI_MODE3);

  // reset
  latch(SROE, HIGH);
  SPI.transfer16(0xF000);
  latch(SROE, LOW);

  // LDAC mode: continuous update ad5308_5318_5328.pdf Table 8.
  ldac_mode = (0x05 << 13) + (0x3FF << 2);

  Serial.print("LDAC mode ");
  Serial.println(ldac_mode, HEX);

  latch(SROE, HIGH);
  SPI.transfer16(ldac_mode);
  latch(SROE, LOW);

  SPI.setDataMode(SPI_MODE2);
}

void ad5318_dac_write(uint8_t dacno, uint16_t dacval)
{
  uint16_t dacword=0;
  dacword = (dacno << 12) + (dacval << 2);
  Serial.println(dacword,HEX);
  latchselect(SRLATCH, AD5318_DAC_LATCH);

  SPI.setDataMode(SPI_MODE3);
  latch(SROE, HIGH);
  SPI.transfer16(dacword);
  latch(SROE, LOW);
  
  SPI.setDataMode(SPI_MODE2); 
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int v1,j;

  latch(TOPOE, LOW); // disable output
 
  v1=digitalRead(IN1);

  // update messages as we update FPGA code
  Serial.print("Rx Lock AND Tx Lock ");
  Serial.println(v1);
//  Serial.print("PA Alarm ");
//  Serial.println(v2);


  latchselect(SRLATCH, TXLATCH);
  write3(0x8D, 0x80, 0x12);
  write3(0x00, 0x01, 0xA0);
  //write3(0x05, 0xD9, 0x31); // 5.9895e9Hz
  write3(0x05, 0xDC, 0x01); // 6,0GHz

  // transmit on 10.2GHz (12000-1800)
  // appears at 447.5MHz on spec an

  // Rx IF = 140MHz. Possible range 115 - 170 MHz; 949 MHz to 1004MHz
  latchselect(SRLATCH, RXLATCH);
  write3(0x8D, 0x80, 0x12);
  write3(0x00, 0x01, 0xA0);
  //write3(0x04, 0xA6, 0x01); // 4.76e9Hz

 write3(0x04, 0x81, 0x21); //4613MHz receives 10.2GHz at 140MHz down
 //write3(0x04, 0x68, 0x21); //4.609GHz
 //write3(0x05, 0xD9, 0x31); // won't lock

#if 1
  // IF = 140MHz.

  init_mesfet_dcc(DCC_DRIVER_LATCH, 0x90);
  init_mesfet_dcc(DCC_PA_LATCH, 0x90);

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

  // 30 -0.1
  // 60 -0.2
  // 90 -0.4
  // 120 -0.55
  // 300 -1.42
  
#if 1
// 300 is good - 10dB between 300 and 100.
  ad5318_dac_write(0,300); 

  // ch 0 inner atten
  // ch1 1 outer atten
  ad5318_dac_write(1,300);

#define DACDEF 100
#if 1
  //ad5318_dac_write(2,DACDEF);

  //ad5318_dac_write(3,DACDEF);

  //ad5318_dac_write(4,DACDEF);

  //ad5318_dac_write(5,DACDEF);

  //ad5318_dac_write(6,DACDEF);

  //ad5318_dac_write(7,DACDEF);
#endif

  delay(2000);

  ad5318_dac_write(0,100); 

  // ch 0 inner atten
  // ch1 1 outer atten
  ad5318_dac_write(1,100);

#define DACDEF2 1
#if 1
  //ad5318_dac_write(2,DACDEF2);

  //ad5318_dac_write(3,DACDEF2);

  //ad5318_dac_write(4,DACDEF2);

  //ad5318_dac_write(5,DACDEF2);

  //ad5318_dac_write(6,DACDEF2);

  //ad5318_dac_write(7,DACDEF2);
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

#if 0
  latch(TOPOE, HIGH); // enable output
  for (j=0;j<255;j++)
  {
    Serial.println(j);
    latchselect(TOPLATCH, j);
    delay(100);
  }
#endif

 
  delay(3000);
  //adf4360stat();
}
