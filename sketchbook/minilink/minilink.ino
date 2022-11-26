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
#define ADF4360LATCH WHITE

#define LATCHDEF(_x) ((uint8_t)(~(1<<_x)))
#define TXLATCH          LATCHDEF(0)
#define RXLATCH          LATCHDEF(1)
#define DCC_DRIVER_LATCH LATCHDEF(2)
#define DCC_PA_LATCH     LATCHDEF(3)
#define AD5318_DAC_LATCH LATCHDEF(5)
#define MAX147LATCH      LATCHDEF(7)

/* MAX11014 registers */
#define TH1 0x20
#define TH2 0x2C
#define IH1 0x24
#define IH2 0x30
#define VH1 0x28
#define VH2 0x34

#define HCFG 0x38
#define ALMHCFG 0x3C
#define THRUDAC1 0x4A
#define THRUDAC2 0x4E
#define ADCCON 0x62
#define SHUT 0x64
#define SCLR 0x74
#define FIFO 0x80
#define FLAG 0xF6
#define ALMFLAG 0xF8
#define ALMHCFG 0x3C
#define ALMSCFG 0x3E

/* MAX11014 ALMFLAGs */
#define HIGH_V2 (1<<11)
#define LOW_V2  (1<<10)
#define HIGH_I2 (1<<9)
#define LOW_I2  (1<<8)
#define HIGH_T2 (1<<7)
#define LOW_T2  (1<<6)
#define HIGH_V1 (1<<5)
#define LOW_V1  (1<<4)
#define HIGH_I1 (1<<3)
#define LOW_I1  (1<<2)
#define HIGH_T1 (1<<1)
#define LOW_T1  (1<<0)

/* MAX11014 FLAGs */
#define RESTART (1<<6)
#define ALUBUSY (1<<5)
#define PGABUSY (1<<4)
#define ADCBUSY (1<<3)
#define VGBUSY  (1<<2)
#define FIFOEMP (1<<1)
#define FIFOOVR (1<<0)

// about 700mA for now for PA current
#define IMAX 2010
#define VMAX 3900

// 40C (40*8==320)
#define T1LIMIT 320

void adf4360stat();
void adf4360();
void max147_read();
void ad5318_dac_init();
void ad5318_dac_write(uint8_t dacno, uint16_t val);

void decode_flags(uint16_t flags);
void decode_almflags(uint16_t almflags);

static uint8_t counter=0;

void setup()
{
  // put your setup code here, to run once:
  Serial.begin(115200);
  Serial.println("Minilink jag");

  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  /* mid right 74LS595 */
  pinMode(SRLATCH, OUTPUT);
  pinMode(SROE, OUTPUT);

  /* top 74LS595 */
  pinMode(TOPLATCH, OUTPUT);
  pinMode(TOPOE, OUTPUT);

  pinMode(IN1, INPUT);

  pinMode(ADF4360STAT, INPUT);

  pinMode(ADF4360LATCH, OUTPUT);

  //pinMode(DATA, OUTPUT);
  //pinMode(LED, OUTPUT);

  digitalWrite(SRLATCH, HIGH);
  digitalWrite(TOPLATCH, HIGH); // goes through 4049 inverter directly onto top 74HC595 RCLK

  digitalWrite(SROE, LOW);
  digitalWrite(TOPOE, LOW); // goes through 4049 inverter directly onto top 74HC595 RCLK

  ad5318_dac_init();

  init_mesfet_dcc(DCC_DRIVER_LATCH, 0, 0x200); // CH2 0x300 does make a difference
                                                   // CH1 0x300 noisier?

  init_mesfet_dcc(DCC_PA_LATCH, 0, 0x200);

  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, 0x7FF);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, 0x7FF);

  digitalWrite(ADF4360LATCH, HIGH);
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

void write_byte_then_short(unsigned char byteval, uint16_t val)
{
   latch(SROE, HIGH);
   SPI.transfer(byteval);
   SPI.transfer16(val);
   latch(SROE, LOW);
}

char *dcc_latch_tostr(unsigned char dcc_latch)
{
  static char driver[]="driver ";
  static char pa[]="pa ";
  static char unknown[]="unknown ";

  if  (dcc_latch == (DCC_DRIVER_LATCH&0xFF))
    return driver;

  if  (dcc_latch == (DCC_PA_LATCH&0xFF))
    return pa;

  return unknown;
}

int readfifo(unsigned char dcc_latch)
{
  unsigned char chan;
  uint16_t val;
  int result,offset=0;
  int div=1;

  latchselect(SRLATCH, dcc_latch);
  
  latch(SROE, HIGH);
  SPI.transfer(FIFO);
  val=SPI.transfer16(0);
  latch(SROE, LOW);

  chan=(val&0xF000)>>12;

  if (chan != 15)
    Serial.print(dcc_latch_tostr(dcc_latch));
    
  switch(chan)
  {
    case 0:
      Serial.print("Internal temp ");
      offset=0;
      div=8;
      break;
    case 1:
      Serial.print("CH1 ext temp ");
      offset=0;
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
      offset=0;
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
      //Serial.println("Empty");
      break;
  }

  val&=0xFFF;
  if (chan == 15)
  {
    //decode_flags(val);
    //Serial.println();
  }
  else
  {
    Serial.println((val+offset)/div);
  }

  return chan;
}

void decode_almflags(uint16_t almflags)
{
  if (almflags & HIGH_V2)
      Serial.print("HIGH_V2 ");
  if (almflags & LOW_V2 )
      Serial.print("LOW_V2  ");
  if (almflags & HIGH_I2)
      Serial.print("HIGH_I2 ");
  if (almflags & LOW_I2)
      Serial.print("LOW_I2 ");
  if (almflags & HIGH_T2)
      Serial.print("HIGH_T2 ");
  if (almflags & LOW_T2)
      Serial.print("LOW_T2 ");
  if (almflags & HIGH_V1)
      Serial.print("HIGH_V1 ");
  if (almflags & LOW_V1)
      Serial.print("LOW_V1 ");
  if (almflags & HIGH_I1)
      Serial.print("HIGH_I1 ");
  if (almflags & LOW_I1)
      Serial.print("LOW_I1 ");
  if (almflags & HIGH_T1)
      Serial.print("HIGH_T1 ");
  if (almflags & LOW_T1)
      Serial.print("LOW_T1 ");
  if (almflags)
    Serial.println();
}

void decode_flags(uint16_t flags)
{
  if (flags & RESTART)
      Serial.print("RESTART ");
  if (flags & ALUBUSY)
      Serial.print("ALUBUSY ");
  if (flags & PGABUSY)
      Serial.print("PGABUSY ");
  if (flags & ADCBUSY)
      Serial.print("ADCBUSY ");
  if (flags & VGBUSY)
      Serial.print("VGBUSY ");
  if (flags & FIFOEMP)
      Serial.print("FIFOEMP ");
  if (flags & FIFOOVR)
      Serial.print("FIFOOVR ");
  if (flags)
    Serial.println();
}

uint16_t read_flag_reg(unsigned char dcc_latch, uint8_t reg)
{
  uint16_t val;

  latchselect(SRLATCH, dcc_latch);

  latch(SROE, HIGH);
  SPI.transfer(reg);
  val=SPI.transfer16(0);
  latch(SROE, LOW);

  return (val&0xFFF);
}

void drain_mesfet_fifo(unsigned char dcc_latch)
{
  int chan;

  chan = readfifo(dcc_latch);

  while (!((chan==15) || (chan==14)))
  {
    chan=readfifo(dcc_latch);
  }
}

void write_mesfet_dcc(uint8_t dcc_latch, uint8_t reg, uint16_t data)
{
  latchselect(SRLATCH, dcc_latch);

  write_byte_then_short(reg, data);
}

void init_mesfet_dcc(unsigned char dcc_latch, uint16_t chan1dacval, uint16_t chan2dacval)
{
  unsigned char val1, val2;
  Serial.print("init_mesfet_dcc ");
  Serial.println(dcc_latch_tostr(dcc_latch));

  // see MAX11014 p69
  latchselect(SRLATCH, dcc_latch);
  write3(SHUT, 0x00, 0x00); // Removes the global power-down.
  write3(SHUT, 0x00, 0x00); // Powers up all parts of the MAX11014.
  write3(SCLR, 0x00, 0x20); // Arms the full reset.
  write3(SCLR, 0x00, 0x40); // Completes the full reset.
  write3(SCLR, 0x00, 0x20); // Arms the full reset.
  write3(SCLR, 0x00, 0x40); // Completes the full reset.

  delay(1); // necessary

  latch(SROE, HIGH);
  SPI.transfer(FLAG); // Read of FLAG register to verify reset good. Code should read 0x42 if reset good.
  val1=SPI.transfer(0);
  val2=SPI.transfer(0);
  latch(SROE, LOW);

  if (val2 != 0x42)
  {
    Serial.print(dcc_latch_tostr(dcc_latch));
    Serial.println("MAX11014 Reset failed");
    Serial.println(val1,HEX);
    Serial.println(val2,HEX);
    return;
  }

  write3(SHUT, 0x00, 0x00); // Removes the global power-down.
  write3(SHUT, 0x00, 0x00); // Powers up all parts of the MAX11014.

  // reverse engineered from Saleae
  write3(HCFG, 0x00, 0x40); // 0x40 means load ADC results into FIF0

  write_byte_then_short(THRUDAC1, chan1dacval);
  write_byte_then_short(THRUDAC2, chan2dacval);
  
  // request a conversion
  write3(ADCCON, 0x07, 0xFF);

  write_byte_then_short(ALMHCFG, 0xFFF); // FIXME clamping bits
  write_byte_then_short(ALMSCFG, 0xAAA);

  write_byte_then_short(TH1, T1LIMIT); //40C
  write_byte_then_short(IH2, IMAX);
  write_byte_then_short(VH2, VMAX);
}

void adf4360stat()
{
  int v1;

  v1=digitalRead(ADF4360STAT);
  if (v1)
    Serial.println("ADF4360 UNLOCKED!!");
  Serial.print("ADF4360 Lock ");
  Serial.println(v1);  
}

void adf4360()
{
  Serial.println("adf4360");
  // ADF4360

  latchselect(SRLATCH, 0xFF);  // prevents rx synth getting trashed - TODO

  latch(ADF4360LATCH, LOW);

  delay(1);
  write3(0x81, 0xF1, 0x28);
  latch(ADF4360LATCH, HIGH);
  latch(ADF4360LATCH, LOW);
  write3(0x00, 0x08, 0x21); // stock R value
  latch(ADF4360LATCH, HIGH);
  latch(ADF4360LATCH, LOW);
  write3(0x08, 0xCA, 0x02); // 1.8GHz 1048.4
  //write3(0x07, 0x40, 0x22); // stock A B N // 643.483, 1485MHz

  delay(1);

  latch(ADF4360LATCH, HIGH);
}

void ad5318_dac_init(void)
{
  uint16_t ldac_mode=0;
  latchselect(SRLATCH, AD5318_DAC_LATCH);
  SPI.setDataMode(SPI_MODE1);

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

  SPI.setDataMode(SPI_MODE0);
}

void ad5318_dac_write(uint8_t dacno, uint16_t dacval)
{
  uint16_t dacword=0;
  dacword = (dacno << 12) + (dacval << 2);
  //Serial.println(dacword,HEX);
  latchselect(SRLATCH, AD5318_DAC_LATCH);

  SPI.setDataMode(SPI_MODE1);
  latch(SROE, HIGH);

  SPI.transfer16(dacword);
  
  latch(SROE, LOW);
  SPI.setDataMode(SPI_MODE0);
}

void max147_read(void)
{
  uint8_t tb1=0;
  uint16_t result;
  uint8_t chan;

  latchselect(SRLATCH, MAX147LATCH);

  for (chan=0; (chan<8); chan++)
  {
  tb1 = 0x80 + (chan << 4) + 0x0B;
  latch(SROE, HIGH);
  SPI.transfer(tb1);
  result=SPI.transfer16(0);
  result>>=3;

  latch(SROE, LOW);

  Serial.print("max147 chan ");
  Serial.print(chan);
  Serial.print(" ");
  Serial.println(result);
  }
}

void max147_read_onboard(void)
{
  uint8_t tb1=0;
  uint16_t result;
  uint8_t chan;

  SPI.setDataMode(SPI_MODE0);

  for (chan=0; (chan<8); chan++)
  {
  tb1 = 0x80 + (chan << 4) + 0x0F;
  latch(SRLATCH, HIGH);
  SPI.transfer(tb1);
  result=SPI.transfer16(0);
  result>>=3;

  latch(SRLATCH, LOW);

  Serial.print("max147 chan ");
  Serial.print(chan);
  Serial.print(" ");
  Serial.println(result);
  }
}

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int v1;
  uint8_t j=0;

#if 0
  max147_read_onboard();
#else
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

  // IF = 140MHz.


  // tx input upconverter
  adf4360();
  delay(10);
  adf4360stat();

  //latchselect(SRLATCH, RXLATCH);
  //latch(SROE, HIGH);
  //delay(10);
  //latch(SROE, LOW);
  //delay(100);
  //delay(500);

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

  Serial.println("hipwr");
  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, 0x7FF);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, 0x7FF);
  drain_mesfet_fifo(DCC_PA_LATCH);
  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, 0x7FF);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, 0x7FF);

  max147_read();
#define DACDEF 0

  //ad5318_dac_write(2,DACDEF);

  //ad5318_dac_write(3,DACDEF);

  //ad5318_dac_write(4,DACDEF);

  //ad5318_dac_write(5,DACDEF);

  //ad5318_dac_write(6,DACDEF);  // increasing value causes signal drop?

  //ad5318_dac_write(7,DACDEF);

  delay(4000);

  ad5318_dac_write(0,100); 

  // ch 0 inner atten
  // ch1 1 outer atten
  ad5318_dac_write(1,100);

  Serial.println("lopwr");
  max147_read();

#define DACDEF2 1

  //ad5318_dac_write(2,DACDEF2);

  //ad5318_dac_write(3,DACDEF2);

  //ad5318_dac_write(4,DACDEF2);

  //ad5318_dac_write(5,DACDEF2);

  //ad5318_dac_write(6,DACDEF2); // increasing value causes signal drop?

  //ad5318_dac_write(7,DACDEF2);
  
#endif
  drain_mesfet_fifo(DCC_PA_LATCH);
  delay(5000);

  //test

  //latch(TOPOE, HIGH); // enable output
  //Serial.println(counter);
  //latchselect(TOPLATCH, counter++);

#if 0
  latch(TOPOE, HIGH); // enable output
  for (j=0;j<255;j++)
  {
    Serial.println(j);
    latchselect(TOPLATCH, j);
    delay(100);
  }
#endif

#if 1
  //write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, 0x7FF);
  //write_mesfet_dcc(DCC_PA_LATCH, ADCCON, 0x7FF);

  Serial.println("driverflags");
  drain_mesfet_fifo(DCC_DRIVER_LATCH);
  decode_almflags(read_flag_reg(DCC_DRIVER_LATCH, ALMFLAG));

  Serial.println("paflags");
  drain_mesfet_fifo(DCC_PA_LATCH);
  decode_almflags(read_flag_reg(DCC_PA_LATCH, ALMFLAG));

  //write_mesfet_dcc(DCC_DRIVER_LATCH, SCLR, (1<<4));
  //write_mesfet_dcc(DCC_PA_LATCH, SCLR, (1<<4));

  write_mesfet_dcc(DCC_DRIVER_LATCH, TH1, T1LIMIT);
  write_mesfet_dcc(DCC_PA_LATCH, TH1, T1LIMIT);

  write_mesfet_dcc(DCC_PA_LATCH, IH2, IMAX);
  write_mesfet_dcc(DCC_PA_LATCH, VH2, VMAX);
#endif
  adf4360stat();
#endif
  delay(3000);
}
