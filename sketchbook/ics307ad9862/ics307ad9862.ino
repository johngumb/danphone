// i2c #include <Wire.h>
#include <SPI.h>


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
unsigned char  progword[]={0x00, 0x80, 0x3F, 0x80, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x1F, 0xF2};

// output 1 23.2MHz
// output 2 116 MHz
unsigned char progword2[]={0x00, 0x80, 0x3F, 0xC0, 0x46, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x1F, 0xF2};

#define TWO_BYTE_TRANSFER 0x40
// AD9862
unsigned char ad9862reset[]={0x00, 0x20};
unsigned char ad9862dacW1[]={0x2A, 0x60}; // 3:0 116MHz adjusted exactly
unsigned char ad9862dacW2[]={0x2B, 0x57}; // 11:4 116MHz adjusted exactly

#define GREEN_PIN2 2
#define RED_PIN3 3

/*
 * Arduino UNO
 * Pin 13 SCK
 * Pin 12 MISO
 * Pin 11 MOSI
 * Pin 10 Chip Select/Slave Select (SS)
 * Pin 9 AD9862 CSEL
 */
#define AD9862_CSEL 9

bool g_led_initialised;

void led_init(void) {
   pinMode(GREEN_PIN2, OUTPUT);
   pinMode(RED_PIN3, OUTPUT);
   g_led_initialised=1;
}

void set_led(const int led, const led_state_t state)
{
  if (!g_led_initialised)
    led_init();

  switch(state)
  {
  case off: digitalWrite(led, LOW);
  break;
  case on: digitalWrite(led, HIGH);
  break;
  }
}

void red_led(const led_state_t state)
{
  set_led(RED_PIN3, state);
}

void green_led(const led_state_t state)
{
  set_led(GREEN_PIN2, state);
}

void ad9862_write(const unsigned int reg, const unsigned int val)
{
  digitalWrite(AD9862_CSEL, HIGH);
  SPI.transfer(reg);
  SPI.transfer(val);
  digitalWrite(AD9862_CSEL, LOW);  
}

void ad9862_write2(const unsigned int reg, const unsigned int val1, const unsigned int val2)
{
  digitalWrite(AD9862_CSEL, HIGH);
  SPI.transfer(((reg+1)|TWO_BYTE_TRANSFER));
  SPI.transfer(val2);
  SPI.transfer(val1);
  digitalWrite(AD9862_CSEL, LOW);  
}

unsigned int ad9862_read(const unsigned int reg)
{
  unsigned int val;

  digitalWrite(AD9862_CSEL, HIGH);
  SPI.transfer(0x80|reg);
  val = SPI.transfer(0); // read back
  digitalWrite(AD9862_CSEL, LOW);

  return val;
}

bool ad9862_write_verified(const unsigned int reg, const unsigned int val)
{
  unsigned int readback_val;
  
  ad9862_write(reg, val);
  readback_val = ad9862_read(reg);

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
}

#define CHECK_RETURN(_x) if (!_x) return false;

bool board_init()
{
  for (int i=0; (i<sizeof(progword)); i++)
  {
    SPI.transfer(progword2[i]);
  }

  // ics307 needs a 'blip' after programming
  digitalWrite(SS, LOW);
  delay (1);
  digitalWrite(SS, HIGH);

  // The clock has changed to the AD9862 due to the above.
  // give it chance to sort itself out.
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

  return true;
}

void setup() {
  // put your setup code here, to run once:

  red_led(on);

  Serial.begin(115200);
  Serial.println("307GI03L Test");
  Serial.println("");

  pinMode(AD9862_CSEL, OUTPUT);
  digitalWrite(SS, HIGH);    // SS is pin 10
  digitalWrite(AD9862_CSEL, LOW);
  
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);
}

unsigned char is_eol(const char *c)
{
    return ((*c==' ') || (*c=='\r') || (*c=='\n') || (*c==';'));
}

static char g_str[40];

char getchar_nano(void)
{
  char c;

  // wait here
  while (Serial.available() < 1);

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
		printf("HEX ERROR:%x\n",(unsigned) ch);

	return val;
}

char bytetohexdigit(const unsigned char val)
{
    if (val>9)
        return (val-10)+'A';
    else
        return val+'0';
}

unsigned char strtohex(const char *ch)
{
	unsigned char val, i=0, rval=0;

	while (i<2)
	{
		val = hexdigittobyte(ch[i]);

		if (i==0)
		{
			rval=val<<4;
		}
		else
		{
			rval=rval+val;
		}

		i++;
	}

	return rval;
}

#define cmd(_cmpstr,_rtn) if (strcmp(g_str, _cmpstr)==0) {_rtn; break;}

#define partcmd(_cmpchar, _rtn) if (g_str[0]==_cmpchar) {_rtn; break; }


void act_synth(void)
{
	unsigned char fval, offset=1; // skip first command string byte "S"

	// deal with, for example "SD9C" or "S09C"
	if ((strlen(g_str)%2)==0)
	{
		fval=hexdigittobyte(g_str[offset++]);
		Serial.println(fval, HEX);
	}

	// at this point,remaining
	// hex string is always of form
	// 11223344 i.e. even number of chars
	while (g_str[offset]!=0)
	{
		Serial.println(strtohex(&g_str[offset]), HEX);
		offset+=2;
	}
}

static bool g_board_initialised;

void loop() {

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
        partcmd('S', act_synth());
    } while (0);
  }
}
