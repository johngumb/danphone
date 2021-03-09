// i2c #include <Wire.h>
#include <SPI.h>

//#define AD9862_SIM

typedef enum {
  off = 0,
  on = 1
} led_state_t;

// https://www.renesas.com/us/en/products/clocks-timing/clock-generation/clocks-general-purpose/307-03-serially-programmable-clock-source#tools_support
// 307GI03 synth chip.
//
// https://www.analog.com/media/en/technical-documentation/data-sheets/AD9860_9862.pdf
//

// 132 bit word generated by VersaClock II. NOTE this is padded with a 4 byte nibble of 0 at MSB
// to make delivery via SPI easier. Lead with zeros and start on a byte boundary. The chip will take
// the last 132 bits before chip select is pulsed.
unsigned char progword_versaclock[]={0x00, 0x80, 0x3F, 0x80, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x1F, 0xF2};

// output 1 23.2MHz
// output 2 116 MHz
unsigned char progword2[]={0x00, 0x80, 0x3F, 0xC0, 0x46, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x1F, 0xF2};


// AD9862
#define AD9862_SIGMA_DELTA_DAC_REG_LO 42
#define AD9862_SIGMA_DELTA_DAC_REG_HI 43

unsigned char ad9862reset[]={0x00, 0x20};
unsigned char ad9862dacW1[]={AD9862_SIGMA_DELTA_DAC_REG_LO, 0x00}; // 3:0 116MHz adjusted exactly
unsigned char ad9862dacW2[]={AD9862_SIGMA_DELTA_DAC_REG_HI, 0x56}; // 11:4 116MHz adjusted exactly

#define GREEN_PIN 5
#define RED_PIN 6

/*
 * Arduino Nano Every
 * Pin 13 SCK
 * Pin 12 MISO
 * Pin 11 MOSI
 * Pin 10 ICS307 Slave Select. This IS SS on classic Nano and Uno, but not Nano Every.
 * Pin 9 AD9862 CSEL
 * Pin 4 1PPS in
 * Pin 2 External clock in
 */
#define ICS307_SS_PIN 10
#define AD9862_CSEL_PIN 9
#define PPS_PIN 4

bool g_led_initialised;

void led_init(void) {
   pinMode(GREEN_PIN, OUTPUT);
   pinMode(RED_PIN, OUTPUT);
   g_led_initialised=1;
}

void set_led(const int led, const led_state_t state)
{
#if 0
  if (!g_led_initialised)
    led_init();

  switch(state)
  {
  case off: digitalWrite(led, LOW);
  break;
  case on: digitalWrite(led, HIGH);
  break;
  }
#endif
}

void red_led(const led_state_t state)
{
  set_led(RED_PIN, state);
}

void green_led(const led_state_t state)
{
  set_led(GREEN_PIN, state);
}

void ad9862_write(const unsigned int reg, const unsigned int val)
{
  digitalWrite(AD9862_CSEL_PIN, HIGH);
  SPI.transfer(reg);
  SPI.transfer(val);
  digitalWrite(AD9862_CSEL_PIN, LOW);
}

unsigned int ad9862_read(const unsigned int reg)
{
  unsigned int val;

  digitalWrite(AD9862_CSEL_PIN, HIGH);
  SPI.transfer(0x80|reg);
  val = SPI.transfer(0); // read back
  digitalWrite(AD9862_CSEL_PIN, LOW);

  return val;
}

bool ad9862_write_verified(const unsigned int reg, const unsigned int val)
{
  unsigned int readback_val;
  
  ad9862_write(reg, val);
  readback_val = ad9862_read(reg);

#ifndef AD9862_SIM
  if (val != readback_val)
  {
    Serial.print("AD9862 Register ");
    Serial.print(reg);
    Serial.print(": WRITE FAILED, expected ");
    Serial.print(val);
    Serial.print(" but got ");
    Serial.println(readback_val);
  }

  return (val == readback_val);
#else
  return true;
#endif
}

bool ad9862_write_guaranteed(const unsigned int reg, const unsigned int val)
{
  unsigned int readback_val;
  bool success=false;

  while (!success)
  {
    success=ad9862_write_verified(reg, val);
    delay(1);
  }
}

#define CHECK_RETURN(_x) if (!_x) return false;

void ics307_write(const unsigned char *progword)
{
  for (int i=0; (i<sizeof(progword_versaclock)); i++)
  {
    SPI.transfer(progword[i]);
  }

  // ics307 needs a 'blip' after programming
  digitalWrite(ICS307_SS_PIN, LOW);
  delay (1);
  digitalWrite(ICS307_SS_PIN, HIGH);
}

bool board_init()
{
  ics307_write(progword2);

  // The clock has changed to the AD9862 due to the above.
  // Give it chance to sort itself out.
  delay(10);

  // AD9862 programming: reset the device
  ad9862_write(0, 0x20); // can't use write_verified here - reset bit write-only

  CHECK_RETURN(ad9862_write_verified(ad9862dacW1[0], ad9862dacW1[1]))

  CHECK_RETURN(ad9862_write_verified(ad9862dacW2[0], ad9862dacW2[1]))

  // register 1 rx power down
  CHECK_RETURN(ad9862_write_verified(0x01, 0x01))

  // register 8 tx power down
  CHECK_RETURN(ad9862_write_verified(0x08, 0x07))

  // DLL power down
  CHECK_RETURN(ad9862_write_verified(24, 0x04))

  // Leave the ics307 with correct info - can disconnect USB
  // and have freq stay the same.
  ics307_write(progword2);

  return true;
}

void init_internal_pps_interrupt(void)
{
  TCB_t *timer_B = (TCB_t *)&TCB1;

  //https://forum.arduino.cc/index.php?topic=626736.msg4268642#msg4268642

  timer_B->CTRLB = TCB_CNTMODE_INT_gc;

  timer_B->CTRLA = (TCB_CLKSEL_CLKTCA_gc | TCB_ENABLE_bm);

  // 10,000,000 / 64 = 156250. Divide this by 15625 to give 10 interrupts/sec.
  // Or 15625*2 to give 5 interrupts/sec.
  timer_B->CCMP = 15624 * 2; // 5 internal interrupts per second

  timer_B->INTFLAGS = TCB_CAPT_bm; // clear interrupt request flag
  timer_B->INTCTRL = TCB_CAPT_bm;  // Enable the interrupt  
}

void init_external_pps_interrupt(void)
{
  pinMode(PPS_PIN, INPUT);
  attachInterrupt(digitalPinToInterrupt(PPS_PIN), oneSecondPassed, RISING);
}

void setup() {
  // clock the board at 10MHz by dividing the internal clock by two.
  // It is set up for 20Mhz in boards.txt
  _PROTECTED_WRITE(CLKCTRL_MCLKCTRLB, (CLKCTRL_PEN_bm | CLKCTRL_PDIV_2X_gc));

  red_led(on);

  // x2 to make up for clock running at half speed (10MHz instead of 20MHz).
  Serial.begin(115200*2);
  Serial.println("307GI03L Test");
  Serial.println("");

  pinMode(AD9862_CSEL_PIN, OUTPUT);
  pinMode(ICS307_SS_PIN, OUTPUT);
  digitalWrite(ICS307_SS_PIN, HIGH);
  digitalWrite(AD9862_CSEL_PIN, LOW);

  // Note on Nano Every LED_BUILTIN is SPI Clock pin (13) so is unusable
  // if SPI is used.
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  init_external_pps_interrupt();

  init_internal_pps_interrupt();
}

volatile unsigned int g_dbg;
volatile unsigned int g_internal_interrupts=0;
volatile unsigned char g_timerb_interrupt=0;

#define SECONDBOUNDARY 4
ISR(TCB1_INT_vect)
{
  TCB1.INTFLAGS = TCB_CAPT_bm;

  if (g_internal_interrupts==SECONDBOUNDARY)
  {
    g_timerb_interrupt=1;
    g_internal_interrupts=0;
    g_dbg++;
  }
  else
  {
    g_internal_interrupts++;
  }
}

#define INTERNAL_COUNT_HEAD_START 100
volatile unsigned char ISRcalled=0;
volatile unsigned int g_extseconds=0;
void oneSecondPassed()
{
  ISRcalled=1;

  // sync internal timer to 1pps and give it a head start
  TCB1.CNT = INTERNAL_COUNT_HEAD_START;
  g_internal_interrupts=0;
  g_extseconds++;
}

unsigned long int avg=0; // 32 bits on every
unsigned int avgcnt=0;
void reportClk()
{
  // ISRcalled gives us a second boundary initially.
  // Don't fully understand what's happening initially.
  // Why does ISRcalled HAVE to be in here? It does...
  if (g_timerb_interrupt || ISRcalled)
  {
    unsigned int spin_external=0;

#define SPIN_MAX 1000
    // internal interrupt
    while ((!ISRcalled) && (spin_external<SPIN_MAX))
      spin_external++;

    g_timerb_interrupt=0;
    ISRcalled=0;

#if 1
    //Serial.println(localISRcalled+(2*local_timerb));
    Serial.println(spin_external);
    Serial.println();
#endif

    if ((spin_external) && (spin_external != SPIN_MAX))
    {
      avg+=spin_external;
      avgcnt+=1;
    }

    if (g_extseconds==600)
    {
      float favg=float(avg)/float(avgcnt);
      avg=0;
      avgcnt=0;
      Serial.println(favg);
      Serial.print("dbg ");
      Serial.println(g_dbg);
      g_extseconds=0;
      g_dbg=0;
    }
  }
}

unsigned char is_eol(const char *c)
{
    return ((*c==' ') || (*c=='\r') || (*c=='\n') || (*c==';'));
}

#define MAX_LINE_LEN 81
static char g_str[MAX_LINE_LEN];

char getchar_nano(void)
{
  char c;

  // wait here
  while (Serial.available() < 1)
  {
    reportClk();
  }

  c = Serial.read();

  return c;
}

void getstr(char *str)
{
    unsigned char ptr=0;

    while (1)
    {
        char c = getchar_nano();

        if (! is_eol(&c))
            g_str[ptr++]=c;
        else
            break;
    }

    g_str[ptr]=0;
}

unsigned char hexdigittobyte(const char ch)
{
	unsigned char val;

	if ( (ch>='0') && (ch<='9') )
		val=ch-'0';
	else if ( (ch>='A') && (ch<='F') )
		val=ch-'A'+10;
	else
    {
		Serial.print("HEX ERROR: ");
		Serial.println(ch);
    }
  
	return val;
}

char bytetohexdigit(const unsigned char val)
{
    if (val>9)
        return (val-10)+'A';
    else
        return val+'0';
}

unsigned char strtohex(const char *chstr)
{
	unsigned char val, i=0, rval=0;

	while (i<2)
	{
		val = hexdigittobyte(chstr[i]);

		if (i==0)
		{
			rval=val<<4;
		}
		else
		{
			rval+=val;
		}

		i++;
	}

	return rval;
}

#define cmd(_cmpstr,_rtn) if (strcmp(g_str, _cmpstr)==0) {_rtn; break;}

#define partcmd(_cmpchar, _rtn) if (g_str[0]==_cmpchar) {_rtn; break; }

void act_synth(void)
{
  unsigned char offset=1, stroffset=1; // skip first command string byte "B"
  unsigned char progword_array[17];

  if (strlen(g_str) != 34)
  {
    Serial.println("ERROR: Synth command must be of the form BN*33 i.e. B followed by hex string of length 132 bits");
    return;
  }

  // get the first hex character - the first nibble.
  progword_array[0]=hexdigittobyte(g_str[stroffset++]);

	// at this point,remaining
	// hex string is always of form
	// 11223344 i.e. even number of chars
	while (g_str[stroffset]!=0)
	{
    progword_array[offset++]=strtohex(&g_str[stroffset]);
		stroffset+=2;
	}

  for (int i=0; (i<sizeof(progword_array)); i++)
    Serial.print(progword_array[i], HEX);

  Serial.println();

  ics307_write(progword_array);
}

// eg. D577
void act_dac()
{
  if (strlen(g_str)==1) // command is 'D'
  {
    unsigned int actual_dac_val_lo, actual_dac_val_hi, actual_dac_val;

    actual_dac_val_lo=ad9862_read(AD9862_SIGMA_DELTA_DAC_REG_LO);
    actual_dac_val_hi=ad9862_read(AD9862_SIGMA_DELTA_DAC_REG_HI);

    // and with 0xF0 to skip stuff in low nibble
    actual_dac_val = (actual_dac_val_hi<<8) + (actual_dac_val_lo & 0xF0);

    actual_dac_val>>=4;

    Serial.print("D");

    Serial.println(actual_dac_val, HEX);
  }
  else
  {
    if (strlen(g_str)!=4)
    {
      Serial.println("ERROR: DAC command must be of the form DNNN");
    }
    else
    {
      unsigned char hi,lownibble;

      hi = strtohex(&g_str[1]);
      lownibble = hexdigittobyte(g_str[3]);

      ad9862_write_guaranteed(AD9862_SIGMA_DELTA_DAC_REG_LO, lownibble<<4);
      ad9862_write_guaranteed(AD9862_SIGMA_DELTA_DAC_REG_HI, hi);

      // report current value using a recursive call
      g_str[1]=0;
      act_dac();
    }
  }
}

void switch_to_external_clock()
{
  // allow existing serial traffic to drain
  delay(10);

  _PROTECTED_WRITE(CLKCTRL_MCLKCTRLA, CLKCTRL_CLKSEL_EXTCLK_gc);
  
  while (1)
  {
    unsigned char mclkstat;

    mclkstat=CLKCTRL.MCLKSTATUS;

    if (mclkstat & CLKCTRL_EXTS_bm)
    {
      // no prescaler stuff - we have a 10MHz external clock now
      _PROTECTED_WRITE(CLKCTRL_MCLKCTRLB, 0);

      // allow clock to settle a bit - may not be needed
      delay(1);

      Serial.println("got external 10MHz clock");    
      break;
    }
            
    delay(1);
  }
}

static bool g_board_initialised;

void loop() {
  switch_to_external_clock();

  while(!g_board_initialised)
  {
    if (board_init())
    {
      Serial.println("init ok");
      g_board_initialised=true;
      red_led(off);
      green_led(on);
      break;
    }
    else
    {
        Serial.println("Waiting...");
        delay(100);
    }
  }
      
  // parse commands and action them forever
  while (1)
  {
    getstr(g_str);

    do
    {
        partcmd('B', act_synth());
        partcmd('D', act_dac());
        partcmd('I', board_init());
    } while (0);
  }
}
