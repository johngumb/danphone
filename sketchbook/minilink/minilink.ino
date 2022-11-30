#include <SPI.h>
#include <assert.h>

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

#define CH0_INTTEMP   0
#define CH1_EXTTEMP1  1
#define CH2_SENS1     2
#define CH3_DACCODE1  3
#define CH4_GATE1     4
#define CH5_ADCIN1    5
#define CH6_EXTTEMP2  6
#define CH7_SENS2     7
#define CH8_DACCODE2  8
#define CH9_GATE2     9
#define CH10_ADCIN2   10

#define ADCCON_ITEMS 11

#define P(_x) (1<<_x)

#define PA_ADCCON1_VAL (P(CH1_EXTTEMP1)|P(CH2_SENS1)|P(CH3_DACCODE1)|P(CH4_GATE1)|P(CH5_ADCIN1))
#define PA_ADCCON2_VAL (P(CH6_EXTTEMP2)|P(CH7_SENS2)|P(CH8_DACCODE2)|P(CH9_GATE2)|P(CH10_ADCIN2))
#define PA_ADCCON_VAL  (P(CH0_INTTEMP)|PA_ADCCON1_VAL|PA_ADCCON2_VAL)

#define DRV_ADCCON1_VAL (P(CH1_EXTTEMP1)|P(CH2_SENS1)|P(CH3_DACCODE1)|P(CH4_GATE1)|P(CH5_ADCIN1))
#define DRV_ADCCON2_VAL (P(CH6_EXTTEMP2)|P(CH7_SENS2)|P(CH8_DACCODE2)|P(CH9_GATE2)|P(CH10_ADCIN2))
#define DRV_ADCCON_VAL  (P(CH0_INTTEMP)|DRV_ADCCON1_VAL|DRV_ADCCON2_VAL)

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
void write_mesfet_dcc(uint8_t dcc_latch, uint8_t reg, uint16_t data);
void drain_mesfet_fifo(uint8_t dcc_latch);

void pulsebithigh(uint8_t signal, int delay);

void latch(uint8_t oe, int level);
void latchselect(uint8_t latchid, uint8_t device);
void write3rfboard(uint8_t v1, uint8_t v2, uint8_t v3);


#define MAXSTACK 10

template <class _T> class Stack
{
  public:
  Stack(){initialise();};
  _T pop();
  void push(_T);
  void initialise();
  uint8_t entries() const {return m_ptr;}

private:
  uint8_t m_ptr=0;
  _T m_store[MAXSTACK];
};

template <class _T> void Stack<_T>::initialise()
{
  m_ptr=0;
  memset(m_store, 0, sizeof(m_store));
}

template <class _T> _T Stack<_T>::pop()
{
  return m_store[--m_ptr];
}

template <class _T> void Stack<_T>::push(_T val)
{
  assert(m_ptr<MAXSTACK);
  m_store[++m_ptr]=val;
}

#define MAX_SUBSYSTEMS 8
#define INVALID_SUBSYSTEM_INDEX MAX_SUBSYSTEMS
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
  void select_subsystem_save_current(ssentry_t);
  void restoreprev();
private:
  bool do_synchronise() const;
  uint8_t find_subsystem_idx(ssentry_t);
  ssentry_t m_lines[MAX_SUBSYSTEMS];
  uint8_t m_state=0;
  bool m_synchronised=false;
  Stack<ssentry_t> m_stack;
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

  return INVALID_SUBSYSTEM_INDEX;
}

void Multiplexer::select_subsystem(ssentry_t ss)
{
  uint8_t entry=find_subsystem_idx(ss);
  uint8_t clicks;

  assert(m_synchronised);

  if (entry==INVALID_SUBSYSTEM_INDEX)
  {
    Serial.println("NOT FOUND");
    return;
  }

  dbgprint(entry);
  dbgprint(m_state);

  clicks = ((entry - m_state) + MAX_SUBSYSTEMS) % MAX_SUBSYSTEMS;

  dbgprint(clicks);

  for (int i=0; (i<clicks); i++)
  {
    pulsebithigh(MULTIPLEXER,0);
    m_state += 1;
    m_state %= MAX_SUBSYSTEMS;
  }
}

void Multiplexer::select_subsystem_save_current(ssentry_t ss)
{
  select_subsystem(ss);
  if (m_stack.entries() > 2)
  {
    Serial.print("Requested subsystem ");
    Serial.print(ss);
    Serial.println(" STACK TOO BIG");
    while(1);
  }

  m_stack.push(ss);
}

void Multiplexer::restoreprev()
{
   select_subsystem(m_stack.pop());
}

void Multiplexer::synchronise()
{
  while(!do_synchronise())
  {
    Serial.println("FAILED TO SYNC");
    delay(10000);
  }

  m_synchronised=true;

  m_stack.initialise();
  select_subsystem_save_current(SS_RFBOARD);
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

class DCC
{
public:
  DCC(uint8_t latch, uint8_t adccon);
  uint16_t get(uint8_t);
private:
  uint8_t processitem();
  void processitems();
  bool all_valid() const;
  void invalidate();
  void load();
  uint8_t m_latch;
  uint8_t m_adccon;
  uint16_t m_items[ADCCON_ITEMS];
  bool m_items_valid[ADCCON_ITEMS];
};

DCC::DCC(uint8_t latch, uint8_t adccon)
{
  m_latch=latch;
  m_adccon=adccon;
  invalidate();
}

void DCC::invalidate()
{
  memset(m_items, 0, sizeof(m_items) );
  memset(m_items_valid, 0, sizeof(m_items_valid));
}

uint8_t DCC::processitem()
{
  uint8_t chan;
  uint16_t val;

  latchselect(SRLATCH, m_latch);

  latch(SROE, HIGH);
  SPI.transfer(FIFO);
  val=SPI.transfer16(0);
  latch(SROE, LOW);

  chan=(val&0xF000)>>12;

  if (chan < ADCCON_ITEMS)
  {
    m_items[chan]=val&0xFFF;
    m_items_valid[chan]=true;
  }

  return chan;
}

void DCC::processitems()
{
  uint8_t chan;

  g_multiplexer.select_subsystem_save_current(SS_RFBOARD);

  chan = processitem();

  while (!((chan==15) || (chan==14)))
  {
    chan=processitem();
  }

  g_multiplexer.restoreprev();
}

bool DCC::all_valid() const
{
  bool result=true;

  for (int i=0; (i<ADCCON_ITEMS); i++)
  {
    if (not m_items_valid[i])
    {
      Serial.print("INVITEM: ");
      Serial.println(i);
      result=false;
    }
  }

  return result;
}

void DCC::load()
{
  int i=0;
  g_multiplexer.select_subsystem_save_current(SS_RFBOARD);
  //invalidate();
  write_mesfet_dcc(m_latch, ADCCON, m_adccon);
  for (int i=0; (i<3); i++)
  {
    processitems();
  }

  all_valid();

  g_multiplexer.restoreprev();
}

uint16_t DCC::get(uint8_t val)
{
  load();

  if (!m_items_valid[val])
  {
    Serial.print("INVALID ITEM ");
    Serial.println(val);
    return 0;
  }

  return m_items[val];
}

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

void latchselect(uint8_t latchid, uint8_t device)
{
  SPI.transfer(device);

  digitalWrite(latchid, HIGH);
  digitalWrite(latchid, LOW);
  digitalWrite(latchid, HIGH);
}

void latch(uint8_t oe, int level)
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

void write3rfboard(uint8_t v1, uint8_t v2, uint8_t v3)
{
   latch(SROE, HIGH);
   SPI.transfer(v1);
   SPI.transfer(v2);
   SPI.transfer(v3);
   latch(SROE, LOW);
}

void write_byte_then_short_rfboard(uint8_t byteval, uint16_t val)
{
   latch(SROE, HIGH);
   SPI.transfer(byteval);
   SPI.transfer16(val);
   latch(SROE, LOW);
}

char *dcc_latch_tostr(uint8_t dcc_latch)
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

int readfifo(uint8_t dcc_latch)
{
  uint8_t chan;
  uint16_t val;
  int result;
  int div=1;
  bool skip=false;

  latchselect(SRLATCH, dcc_latch);
  
  latch(SROE, HIGH);
  SPI.transfer(FIFO);
  val=SPI.transfer16(0);
  latch(SROE, LOW);

  chan=(val&0xF000)>>12;

  switch(chan)
  {
    case 1:
    case 6:
      skip=true;
  }

  if ((chan != 15) && (!skip))
    Serial.print(dcc_latch_tostr(dcc_latch));
    
  switch(chan)
  {
    case 0:
      Serial.print("Internal temp ");
      div=8;
      break;
    case 1:
      //Serial.print("CH1 ext temp ");
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
//      Serial.print("CH2 ext temp ");
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
    if (!skip)
      Serial.println(val/div);
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

uint16_t read_flag_reg(uint8_t dcc_latch, uint8_t reg)
{
  uint16_t val;

  latchselect(SRLATCH, dcc_latch);

  latch(SROE, HIGH);
  SPI.transfer(reg);
  val=SPI.transfer16(0);
  latch(SROE, LOW);

  return (val&0xFFF);
}

void drain_mesfet_fifo(uint8_t dcc_latch)
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

void init_mesfet_dcc(uint8_t dcc_latch, uint16_t chan1dacval, uint16_t chan2dacval)
{
  uint8_t val1, val2;
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
  g_multiplexer.select_subsystem_save_current(SS_ADF4360);
  
  write3_with_cs(0x81, 0xF1, 0x28, ADF4360LATCH);
  write3_with_cs(0x00, 0x08, 0x21, ADF4360LATCH); // stock R value
  write3_with_cs(0x08, 0xCA, 0x02, ADF4360LATCH); // 1.8GHz 1048.4
  //write3_with_cs(0x07, 0x40, 0x22, ADF4360LATCH); // stock A B N // 643.483, 1485MHz

  g_multiplexer.restoreprev();
}

void ad5318_dac_init(void)
{
  latchselect(SRLATCH, AD5318_DAC_LATCH);
  SPI.setDataMode(SPI_MODE1);

  // reset
  latch(SROE, HIGH);
  SPI.transfer16(0xF000);
  latch(SROE, LOW);
  delay(1);

  // LDAC mode: continuous update ad5308_5318_5328.pdf Table 8.

  latch(SROE, HIGH);
  SPI.transfer16(0xA000);
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
  g_multiplexer.select_subsystem_save_current(SS_AD5318);

  SPI.setDataMode(SPI_MODE1);

  latch(SRLATCH, LOW);
  SPI.transfer16(0xF000);
  latch(SRLATCH, HIGH);
  delay(1);

  latch(SRLATCH, LOW);
  SPI.transfer16(0xA000);
  latch(SRLATCH, HIGH);

  SPI.setDataMode(SPI_MODE0);

  g_multiplexer.restoreprev();
}

void ad5318_onboard_dac_write(uint8_t dacno, uint16_t dacval)
{
  uint16_t dacword;
  dacword = (dacno << 12) + (dacval << 2);
  //Serial.println(dacword,HEX);

  g_multiplexer.select_subsystem_save_current(SS_AD5318);

  SPI.setDataMode(SPI_MODE1);

  latch(SRLATCH, LOW);
  SPI.transfer16(dacword);
  latch(SRLATCH, HIGH);

  SPI.setDataMode(SPI_MODE0);

  g_multiplexer.restoreprev();
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

uint16_t rssi()
{
  return max147_read_onboard_chan(0);
}

uint16_t max147_read_onboard_chan(uint8_t chan)
{
  uint16_t result;
  uint8_t tb1=0;

  g_multiplexer.select_subsystem_save_current(SS_MAX147);

  tb1 = 0x80 + (chan << 4) + 0x0F;
  latch(SRLATCH, HIGH);
  SPI.transfer(tb1);
  result=SPI.transfer16(0);
  result>>=3;

  latch(SRLATCH, LOW);

  g_multiplexer.restoreprev();

  return result;
}

void max147_read_onboard(void)
{
  uint16_t result;
  uint8_t chan;

  Serial.println("max147_read_onboard");

  for (chan=0; (chan<8); chan++)
  {

  result=max147_read_onboard_chan(chan);
  Serial.print("max147 chan ");
  Serial.print(chan);
  Serial.print(" ");
  Serial.println(result);
  }
}

int counter;
DCC pa_dcc(DCC_PA_LATCH, PA_ADCCON_VAL);

void loop() {
  // put your main code here, to run repeatedly:
  // pin 11 red wire; data
  int rxtxlockdet;
  uint8_t j=0;
  int dv;

  g_multiplexer.synchronise();

  counter++;
  dv=counter*10;

  dbgprint(dv);

  ad5318_onboard_dac_reset();

  // onboard AD5318
  // ch 0 1.295V (?)
  // ch 1 1.486V (?)
  // ch 2 0.01V
  // ch 3 0.696V rx gain?
  // VREF ABCD 2.5V
  // VREF EFGH 2.5V
  // ch 4 0
  // ch 5 0.151
  // ch 6 0
  // ch 7 0

  // ch3 is crucial. rx gain?? set at 280 to get 0.696V
  ad5318_onboard_dac_write(0, 531);
  ad5318_onboard_dac_write(1, 609);
  ad5318_onboard_dac_write(2, 0);
  ad5318_onboard_dac_write(3, 280); // 0.696V == 280
  ad5318_onboard_dac_write(4, 0);
  ad5318_onboard_dac_write(5, 55); // 0.151V
  ad5318_onboard_dac_write(6, 0);
  ad5318_onboard_dac_write(7, 0);

  max147_read_onboard();

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
#if 1
  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, DRV_ADCCON_VAL);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, PA_ADCCON_VAL);
  drain_mesfet_fifo(DCC_PA_LATCH);
  drain_mesfet_fifo(DCC_DRIVER_LATCH);
  write_mesfet_dcc(DCC_DRIVER_LATCH, ADCCON, DRV_ADCCON_VAL);
  write_mesfet_dcc(DCC_PA_LATCH, ADCCON, PA_ADCCON_VAL);
#endif

  max147_read();
  max147_read_onboard();

  Serial.print("RSSI HI ");
  Serial.println(rssi());
  Serial.print("ADCIN1 HI ");
  Serial.println(pa_dcc.get(CH5_ADCIN1));

#define DACDEF 0

  ad5318_dac_write(2,DACDEF);

  ad5318_dac_write(3,DACDEF);

  ad5318_dac_write(4,DACDEF);

  ad5318_dac_write(5,DACDEF);

  ad5318_dac_write(6,DACDEF);  // increasing value causes signal drop?

  ad5318_dac_write(7,DACDEF);

  delay(4000);

  ad5318_dac_write(0,100); 

  // ch 0 inner atten
  // ch1 1 outer atten
  ad5318_dac_write(1,100);

  Serial.println("lopwr");
  max147_read();
  max147_read_onboard();

#define DACDEF2 100

  ad5318_dac_write(2,DACDEF2);

  ad5318_dac_write(3,DACDEF2);

  ad5318_dac_write(4,DACDEF2);

  ad5318_dac_write(5,DACDEF2);

  ad5318_dac_write(6,DACDEF2); // increasing value causes signal drop?

  ad5318_dac_write(7,DACDEF2);

  drain_mesfet_fifo(DCC_PA_LATCH);

  Serial.print("RSSI LO ");
  Serial.println(rssi());
  Serial.print("ADCIN1 LO ");
  Serial.println(pa_dcc.get(CH5_ADCIN1));

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
