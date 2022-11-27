#include <SPI.h>

//#define DBG

#ifdef DBG
#define dbgprint(_x) {Serial.print(#_x); Serial.print(" "); Serial.println(_x);}
#else
#define dbgprint(_x)
#endif

#define ORANGE 8
#define YELLOW 7
#define GREEN 6
#define VIOLET 5
#define GREY 4
#define WHITE 3
#define GREENWHITE 2

#define IN1 GREEN

#define MULTIPLEXER WHITE

#define SRLATCH ORANGE
#define SROE YELLOW

#define TOPLATCH VIOLET
#define TOPOE GREY 

#define DATA 11
#define DATA_IN 12

#define LED 13

#define ADF4360STAT GREENWHITE
#define ADF4360LATCH SRLATCH

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

#define CH0_INTTEMP   (1<<0)
#define CH1_EXTTEMP1  (1<<1)
#define CH2_SENS1     (1<<2)
#define CH3_DACCODE1  (1<<3)
#define CH4_GATE1     (1<<4)
#define CH5_ADCIN1    (1<<5)
#define CH6_EXTTEMP2  (1<<6)
#define CH7_SENS2     (1<<7)
#define CH8_DACCODE2  (1<<8)
#define CH9_GATE2     (1<<9)
#define CH10_ADCIN2   (1<<10)

#define PA_ADCCON1_VAL (CH2_SENS1|CH3_DACCODE1|CH4_GATE1|CH5_ADCIN1)
#define PA_ADCCON2_VAL (CH7_SENS2|CH8_DACCODE2|CH9_GATE2|CH10_ADCIN2)
#define PA_ADCCON_VAL (CH0_INTTEMP|PA_ADCCON1_VAL|PA_ADCCON2_VAL)

#define DRV_ADCCON1_VAL (CH2_SENS1|CH3_DACCODE1|CH4_GATE1|CH5_ADCIN1)
#define DRV_ADCCON2_VAL (CH7_SENS2|CH8_DACCODE2|CH9_GATE2|CH10_ADCIN2)
#define DRV_ADCCON_VAL (CH0_INTTEMP|DRV_ADCCON1_VAL|DRV_ADCCON2_VAL)

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

void pulsebithigh(uint8_t signal, int delay);

#define MAX_SUBSYSTEMS 8
#define LOOPBACK 0xFF
typedef enum
{ SS_RFBOARD=1,
  SS_MAX147,
  SS_ADF4360,
  SS_AD5318,
  SS_LOOPBACK=255} ssentry_t;
  
class Multiplexer
{
public:
  Multiplexer();
  void synchronise();
  void select_subsystem(ssentry_t);
private:
  bool do_synchronise() const;
  uint8_t find_subsystem_idx(ssentry_t);
  ssentry_t m_lines[MAX_SUBSYSTEMS];
  uint8_t m_state=0;
  bool m_synchronised=false;
};   

Multiplexer::Multiplexer()
{
  m_lines[0]=SS_RFBOARD;
  m_lines[1]=SS_MAX147;
  m_lines[2]=SS_ADF4360;
  m_lines[3]=SS_AD5318;
  m_lines[MAX_SUBSYSTEMS-1]=SS_LOOPBACK;
}

uint8_t Multiplexer::find_subsystem_idx(ssentry_t ss)
{
  for (uint8_t i=0; (i<MAX_SUBSYSTEMS); i++)
    if (m_lines[i]==ss)
      return i;
  return MAX_SUBSYSTEMS; // failure
}

void Multiplexer::select_subsystem(ssentry_t ss)
{
  uint8_t entry=find_subsystem_idx(ss);
  uint8_t clicks;

  if (entry==MAX_SUBSYSTEMS)
  {
    Serial.println("NOT FOUND");
    return;
  }

  dbgprint(entry);
  dbgprint(m_state);

  clicks =  ((entry - m_state) + MAX_SUBSYSTEMS) % MAX_SUBSYSTEMS;

  dbgprint(clicks);

  for (int i=0; (i<clicks); i++)
  {
    pulsebithigh(MULTIPLEXER,0);
    m_state += 1;
    m_state %= MAX_SUBSYSTEMS;
  }
}

void Multiplexer::synchronise()
{
  while(!do_synchronise())
  {
    Serial.println("FAILED TO SYNC");
    delay(10000);
  }

  m_synchronised=true;
}

bool Multiplexer::do_synchronise() const
{
  int j;
  uint16_t readback=0;
  const uint8_t maxloop=(MAX_SUBSYSTEMS*2);
  
#define TESTPATTERN 0xBAAD

  for (j=0; (j<maxloop); j++)
  {
    pulsebithigh(MULTIPLEXER, 0);

    if (readback==TESTPATTERN)
      break;

    readback=SPI.transfer16(TESTPATTERN);
  }

  return not (j==maxloop);
}

Multiplexer g_multiplexer;

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

  pinMode(MULTIPLEXER, OUTPUT);

  //pinMode(DATA, OUTPUT);
  //pinMode(LED, OUTPUT);

  digitalWrite(SRLATCH, HIGH);
  digitalWrite(TOPLATCH, HIGH); // goes through 4049 inverter directly onto top 74HC595 RCLK

  digitalWrite(SROE, LOW);
  digitalWrite(TOPOE, LOW); // goes through 4049 inverter directly onto top 74HC595 RCLK

  g_multiplexer.synchronise();

  ad5318_dac_init();

  init_mesfet_dcc(DCC_DRIVER_LATCH, 0, 0x200); // CH2 0x300 does make a difference
                                                   // CH1 0x300 noisier?

  init_mesfet_dcc(DCC_PA_LATCH, 0, 0x200);

  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, DRV_ADCCON_VAL);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, PA_ADCCON_VAL);
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

void pulsebithigh(uint8_t line, int del)
{
  digitalWrite(line, LOW);
  digitalWrite(line, HIGH);
  digitalWrite(line, LOW);
  delay(del);
}

void write3_with_cs(uint8_t v1, uint8_t v2, uint8_t v3, uint8_t chipsel)
{
  // assumes -ve sense latch
   latch(chipsel, HIGH);
   latch(chipsel, LOW);
   SPI.transfer(v1);
   SPI.transfer(v2);
   SPI.transfer(v3);
   latch(chipsel, HIGH);
}

void write3rfboard(unsigned char v1, unsigned char v2, unsigned char v3)
{
   latch(SROE, HIGH);
   SPI.transfer(v1);
   SPI.transfer(v2);
   SPI.transfer(v3);
   latch(SROE, LOW);
}

void write_byte_then_short_rfboard(unsigned char byteval, uint16_t val)
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

  write_byte_then_short_rfboard(reg, data);
}

void init_mesfet_dcc(unsigned char dcc_latch, uint16_t chan1dacval, uint16_t chan2dacval)
{
  unsigned char val1, val2;
  Serial.print("init_mesfet_dcc ");
  Serial.println(dcc_latch_tostr(dcc_latch));

  // see MAX11014 p69
  latchselect(SRLATCH, dcc_latch);
  write3rfboard(SHUT, 0x00, 0x00); // Removes the global power-down.
  write3rfboard(SHUT, 0x00, 0x00); // Powers up all parts of the MAX11014.
  write3rfboard(SCLR, 0x00, 0x20); // Arms the full reset.
  write3rfboard(SCLR, 0x00, 0x40); // Completes the full reset.
  write3rfboard(SCLR, 0x00, 0x20); // Arms the full reset.
  write3rfboard(SCLR, 0x00, 0x40); // Completes the full reset.

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

  write3rfboard(SHUT, 0x00, 0x00); // Removes the global power-down.
  write3rfboard(SHUT, 0x00, 0x00); // Powers up all parts of the MAX11014.

  // reverse engineered from Saleae
  write3rfboard(HCFG, 0x00, 0x40); // 0x40 means load ADC results into FIF0

  write_byte_then_short_rfboard(THRUDAC1, chan1dacval);
  write_byte_then_short_rfboard(THRUDAC2, chan2dacval);
  
  // request a conversion
  write3rfboard(ADCCON, 0x07, 0xFF);

  write_byte_then_short_rfboard(ALMHCFG, 0xFFF); // FIXME clamping bits
  write_byte_then_short_rfboard(ALMSCFG, 0xAAA);

  write_byte_then_short_rfboard(TH1, T1LIMIT); //40C
  write_byte_then_short_rfboard(IH2, IMAX);
  write_byte_then_short_rfboard(VH2, VMAX);
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
  g_multiplexer.select_subsystem(SS_ADF4360);
  
  write3_with_cs(0x81, 0xF1, 0x28, ADF4360LATCH);
  write3_with_cs(0x00, 0x08, 0x21, ADF4360LATCH); // stock R value
  write3_with_cs(0x08, 0xCA, 0x02, ADF4360LATCH); // 1.8GHz 1048.4
  //write3_with_cs(0x07, 0x40, 0x22, ADF4360LATCH); // stock A B N // 643.483, 1485MHz

  g_multiplexer.select_subsystem(SS_RFBOARD);
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

void ad5318_onboard_dac_reset()
{
  g_multiplexer.select_subsystem(SS_AD5318);

  SPI.setDataMode(SPI_MODE1);

#if 1
  latch(SRLATCH, LOW);
  SPI.transfer16(0xF000);
  latch(SRLATCH, HIGH);
  delay(1);
#endif

  latch(SRLATCH, LOW);
  SPI.transfer16(0xA000);
  latch(SRLATCH, HIGH);

  latch(SRLATCH, LOW);
  SPI.transfer16(0x8003);
  latch(SRLATCH, HIGH);

  SPI.setDataMode(SPI_MODE0);

  g_multiplexer.select_subsystem(SS_RFBOARD);
}

void ad5318_onboard_dac_write(uint8_t dacno, uint16_t dacval)
{
  uint16_t dacword;
  dacword = (dacno << 12) + (dacval << 2);
  //Serial.println(dacword,HEX);

  g_multiplexer.select_subsystem(SS_AD5318);

  SPI.setDataMode(SPI_MODE1);

  latch(SRLATCH, LOW);
  //SPI.transfer16(0x8000);
  SPI.transfer16(dacword);
  latch(SRLATCH, HIGH);

  SPI.setDataMode(SPI_MODE0);

  g_multiplexer.select_subsystem(SS_RFBOARD);
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

  g_multiplexer.select_subsystem(SS_MAX147);

  Serial.println("max147_read_onboard");

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

  g_multiplexer.select_subsystem(SS_RFBOARD);
}

int counter;

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int rxtxlockdet;
  uint8_t j=0;
  int dv;

  counter++;
  dv=counter*10;

  dbgprint(dv);

  g_multiplexer.synchronise();

  ad5318_onboard_dac_reset();

  // onboard AD5318
  // ch 0 1.29V (?)
  // ch 1 1.48V (?)
  // ch 2 0.01V
  // ch 3 0.696V rx gain?
  // VREF ABCD 2.5V
  // VREF EFGH 2.5V
  // ch 4 0
  // ch 5 0.151
  // ch 6 0
  // ch 7 0
#if 0
  // ch3 is crucial. gain??
  ad5318_onboard_dac_write(0, 2113);
  ad5318_onboard_dac_write(1, 2425);
  ad5318_onboard_dac_write(2, 0);
  ad5318_onboard_dac_write(3, 280);
  ad5318_onboard_dac_write(4, 0);
  ad5318_onboard_dac_write(5, 247);
  ad5318_onboard_dac_write(6, 0);
  ad5318_onboard_dac_write(7, 0);
#endif
#if 1
  // ch3 is crucial. rx gain?? set at 280 to get 0.696V
  ad5318_onboard_dac_write(0, 2113);
  ad5318_onboard_dac_write(1, 2425);
  ad5318_onboard_dac_write(2, 0);
  ad5318_onboard_dac_write(3, 280); // 0.696V == 280
  ad5318_onboard_dac_write(4, 2000);
  ad5318_onboard_dac_write(5, 58); // 0.151V
  ad5318_onboard_dac_write(6, 2048);
  ad5318_onboard_dac_write(7, 1024);
#endif
  max147_read_onboard();

  g_multiplexer.select_subsystem(SS_RFBOARD);

  latch(TOPOE, LOW); // disable output
 
  rxtxlockdet=digitalRead(IN1);

  // update messages as we update FPGA code
  Serial.print("Rx Lock AND Tx Lock ");
  Serial.println(rxtxlockdet);
  if (!rxtxlockdet)
    Serial.print("Rx Lock AND Tx Lock FAIL UNLOCKED!!");
//  Serial.print("PA Alarm ");
//  Serial.println(v2);


  latchselect(SRLATCH, TXLATCH);
  write3rfboard(0x8D, 0x80, 0x12);
  write3rfboard(0x00, 0x01, 0xA0);
  //write3rfboard(0x05, 0xD9, 0x31); // 5.9895e9Hz
  write3rfboard(0x05, 0xDC, 0x01); // 6,0GHz

  // transmit on 10.2GHz (12000-1800)
  // appears at 447.5MHz on spec an

  // Rx IF = 140MHz. Possible range 115 - 170 MHz; 949 MHz to 1004MHz
  latchselect(SRLATCH, RXLATCH);
  write3rfboard(0x8D, 0x80, 0x12);
  write3rfboard(0x00, 0x01, 0xA0);
  //write3rfboard(0x04, 0xA6, 0x01); // 4.76e9Hz

 write3rfboard(0x04, 0x81, 0x21); //4613MHz receives 10.2GHz at 140MHz down
 //write3rfboard(0x04, 0x68, 0x21); //4.609GHz
 //write3rfboard(0x05, 0xD9, 0x31); // won't lock

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
  
// 300 is good - 10dB between 300 and 100.
  ad5318_dac_write(0,300);

  // ch 0 inner atten
  // ch1 1 outer atten
  ad5318_dac_write(1,300);

  Serial.println("hipwr");
  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, DRV_ADCCON_VAL);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, PA_ADCCON_VAL);
  drain_mesfet_fifo(DCC_PA_LATCH);
  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, DRV_ADCCON_VAL);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, PA_ADCCON_VAL);

  max147_read();
  max147_read_onboard();

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
  max147_read_onboard();

#define DACDEF2 1

  //ad5318_dac_write(2,DACDEF2);

  //ad5318_dac_write(3,DACDEF2);

  //ad5318_dac_write(4,DACDEF2);

  //ad5318_dac_write(5,DACDEF2);

  //ad5318_dac_write(6,DACDEF2); // increasing value causes signal drop?

  //ad5318_dac_write(7,DACDEF2);

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

  //write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, DRV_ADCCON_VAL);
  //write_mesfet_dcc(DCC_PA_LATCH, ADCCON, DRV_ADCCON_VAL);

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

  adf4360stat();

  delay(3000);
}
