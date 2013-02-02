// FID:            35X000020
// Target:         C8051F35x
// Tool chain:     Keil C51 7.50 / Keil EVAL C51

//-----------------------------------------------------------------------------
// Includes
//-----------------------------------------------------------------------------

#include <c8051f350.h>                 // SFR declarations
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SR_AUDIO_PA 0x01
#define SR_POWER 0x02
#define SR_TX_RX 0x04
#define SR_TX_POWER_HI_LO 0x08
#define SR_EXT_ALARM 0x10
#define SR_TX_PA 0x20
#define SR_TX_AUDIO_ENABLE 0x40
#define SR_RX_AUDIO_ENABLE 0x80

//#define OOBAND
//-----------------------------------------------------------------------------
// Global CONSTANTS
//-----------------------------------------------------------------------------

#define SYSCLK      24500000           // SYSCLK frequency in Hz
#define BAUDRATE        115200           // Baud rate of UART in bps

#define SPI_CLOCK          250000      // Maximum SPI clock
                                       // The SPI clock is a maximum of 500 kHz
                                       // when this example is used with
                                       // the SPI0_Slave code example.



sbit power_on_bit=P0^1;
sbit synth_latch_bit=P0^3;
sbit shift_reg_latch_bit=P0^7;
sbit squelch_bit=P1^0;
sbit locked_bit=P1^1;

#define SYNTH_LATCH_ID 1
#define SHIFT_REG_LATCH_ID 2

static char str[20];
static unsigned int g_last_tx;
//-----------------------------------------------------------------------------
// Function PROTOTYPES
//-----------------------------------------------------------------------------

void SYSCLK_Init (void);
void UART0_Init (void);
void PORT_Init (void);
void Timer2_Init (int);
void SPI0_Init (void);

void SPI_Byte_Write (const unsigned char);

void pulsebithigh(const char latch_id);

void write_synth_spi(const unsigned int *);

void delay(unsigned int limit)
{
    int i;
    for (i=0;i<limit;i++);
}

void latch_init()
{
    synth_latch_bit=1;
    shift_reg_latch_bit=1;
}

void baa()
{
    SPI_Byte_Write(9);
    SPI_Byte_Write(1);
    pulsebithigh(SYNTH_LATCH_ID);

    SPI_Byte_Write(0);
    SPI_Byte_Write(0x8e);
    SPI_Byte_Write(0x42);
    pulsebithigh(SYNTH_LATCH_ID);

}

void latch(const char latchval, const char latch_id)
{
    switch(latch_id)
    {
        case SYNTH_LATCH_ID:
        {
         synth_latch_bit=latchval;
        }
        break;

        case SHIFT_REG_LATCH_ID:
        {
         shift_reg_latch_bit=latchval;
        }
        break;

    }
}

void pulsebithigh(const char latch_id)
{
    latch(0,latch_id);
    latch(1,latch_id);
    latch(0,latch_id);
}

void set_tx_state(const int txena)
{
    unsigned int w[2];
    w[0]=0;

    if (txena)
        SPI_Byte_Write(SR_TX_RX|SR_TX_AUDIO_ENABLE|SR_POWER);
    else
        SPI_Byte_Write(SR_RX_AUDIO_ENABLE|SR_POWER);

    pulsebithigh(SHIFT_REG_LATCH_ID);

    // rx freq is last tx freq
    if (txena)
        w[1]=g_last_tx;
    else
        w[1]=g_last_tx+10816; // 21.4 IF offset

    write_synth_spi(&w);
}

void set_pa_state(const int paena)
{
    if (paena)
        SPI_Byte_Write(SR_TX_RX|SR_TX_AUDIO_ENABLE|SR_POWER|SR_TX_PA|SR_TX_POWER_HI_LO);
    else
        SPI_Byte_Write(SR_TX_RX|SR_TX_AUDIO_ENABLE|SR_POWER);

    pulsebithigh(SHIFT_REG_LATCH_ID);
}

//-----------------------------------------------------------------------------
// MAIN Routine
//-----------------------------------------------------------------------------


int iswhitespace(const char *c)
{
    return ((*c==' ') || (*c=='\r') || (*c=='\n'));
}

void getstr(char *str)
{
    char c;
    int ptr=0;

    while (1)
    {
        c = getchar();
        if (! iswhitespace(&c))
            str[ptr++]=c;
        else
            break;
        
    }

    str[ptr]=0;
}


void act_up(const int p)
{
    printf("acton up\n");
}

void write_synth_spi(const unsigned int *w)
{
    const unsigned char *datptr=&w[0];

    printf("w1: %x w2: %x\n",w[0],w[1]);

    // HACK
    SPI_Byte_Write(9);
    SPI_Byte_Write(1);
    pulsebithigh(SYNTH_LATCH_ID);

    // write first word to SPI, MSB shifted out first.
    SPI_Byte_Write(datptr[0]);
    SPI_Byte_Write(datptr[1]);

    // write second word to SPI, MSB shifted out first.
    datptr=(const unsigned char *) &w[1];
    SPI_Byte_Write(datptr[0]);
    SPI_Byte_Write(datptr[1]);

    pulsebithigh(SYNTH_LATCH_ID);

    // write second val to SPI
    printf("setting synth\n");

    delay(50000);

    if (locked_bit)
        printf("locked\n");
    else
        printf("unlocked\n");
}


void act_set_synth()
{
    unsigned int w[2];

    getstr(str);

    w[0]=atoi(str);

    getstr(str);

    w[1]=atoi(str);

    if (w[1]==0)
    {
        printf("invalid w1\n");
        return;
    }

    write_synth_spi(&w);

}

unsigned char hexdigittobyte(char ch)
{
	unsigned char val;

	if ( (ch>='0') && (ch<='9') )
		val=ch-'0';
	else if ( (ch>='A') && (ch<='F') )
		val=ch-'A'+10;
	else
		printf("HEX ERROR:%x\n",ch);

	return val;
}

unsigned char strtohex(char *ch)
{
	unsigned char i=0;
	unsigned char rval=0;

	while (i<2)
	{
		unsigned char val = hexdigittobyte(ch[i]);


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

void stchar(void);

void act_control(void)
{
    unsigned char val = strtohex(&str[1]);

	SPI_Byte_Write(val);

	pulsebithigh(SHIFT_REG_LATCH_ID);

    /* allow unit to power up */
    if (val & SR_POWER)
        delay(1000);

    stchar();
    printf("K\n");
}


void act_synth(void)
{
	unsigned char offset=1; // skip first command string byte

	// deal with, for example "SD9C" or "S09C"
	if ((strlen(str)%2)==0)
	{
		unsigned char fval=0;

		fval+=hexdigittobyte(str[offset]);

		SPI_Byte_Write(fval);

		offset++;
	}

	// at this point,remaining
	// hex string is always of form
	// 11223344 i.e. even number of chars
	while (str[offset]!=0)
	{
		SPI_Byte_Write(strtohex(&str[offset]));
		//printf("%02x",strtohex(&str[offset]));
		offset+=2;
	}

	pulsebithigh(SYNTH_LATCH_ID);
    stchar();
    printf("K\n");
}

void act_set_power(const int powerstate)
{
    if (powerstate)
    {
        printf("poweringon\n");
        SPI_Byte_Write(0x82);
    }
    else
    {
        printf("poweringoff\n");
        SPI_Byte_Write(0);
    }

    pulsebithigh(SHIFT_REG_LATCH_ID);

    if (powerstate)
    {
        delay(1000);
        baa();
    }
}

void act_status()
{
    if (power_on_bit)
    {
        printf("off\n");
        return;
    }

    if (locked_bit)
        printf("locked\n");
    else
        printf("unlocked\n");


    if (squelch_bit)
        printf("open\n");
    else
        printf("closed\n");
     
}

void stchar()
{
    char result=0;

    if (!power_on_bit)
    {
        result+=4;

        if (locked_bit)
            result+=1;

        if (squelch_bit)
            result+=2;
    }

    putchar('0'+result);
}

void act_stbyte()
{
    stchar();
    putchar('Z');
    putchar('\n');
}

void act_test(int tv)
{
    unsigned int w[2];

    w[0]=0;

    switch(tv)
    {
        case 50:
        {
            w[1]=25600;

        }
        break;

        case 505:
        {
            w[1]=25856;

        }
        break;

        case 51:
        {
            w[1]=26112;

        }
        break;

        case 515:
        {
            w[1]=26368;

        }
        break;

        case 5151:
        {
            w[1]=26370;

        }
        break;

        case 5153:
        {
            w[1]=26372;

        }
        break;

        case 52:
        {
            w[1]=26624;

        }
        break;

		case 5081:
		{
			w[1]=25906;

		}
		break;

		case 5084:
		{
			w[1]=25910;

		}
		break;

#ifdef OOBAND
		case 53:
		{
			w[1]=27136;

		}
		break;

		case 54:
		{
			w[1]=27648;

		}
		break;

		case 55:
		{
			w[1]=28160;

		}
		break;

		case 56:
		{
			w[1]=28672;

		}
		break;


		case 57:
		{
			w[1]=29184;

		}
		break;

		case 58:
		{
			w[1]=29696;

		}
		break;
#endif
    }

    /* 12.5 kHz */
    SPI_Byte_Write(9);
    SPI_Byte_Write(1);
    pulsebithigh(SYNTH_LATCH_ID);

    g_last_tx=w[1];

    write_synth_spi(&w);
}

#define cmd(_cmpstr,_rtn) if (strcmp(str, _cmpstr)==0) {_rtn; break;}

#define partcmd(_cmpchar, _rtn) if (str[0]==_cmpchar) {_rtn; break; }

void main (void) 
{  
    PCA0MD &= ~0x40;                    // WDTE = 0 (clear watchdog timer 
    // enable)
    PORT_Init();                        // Initialize Port I/O
    SYSCLK_Init ();                     // Initialize Oscillator
    UART0_Init();
    SPI0_Init();

    latch_init();

    // come up powered off
    act_set_power(0);

    // default freq of 51.53
    g_last_tx=26372;

    while (1)
    {
        getstr(&str);

        do {
			partcmd('C', act_control());

			partcmd('S', act_synth());

            cmd("Z", act_stbyte());

            cmd("u", act_up(0))

            cmd("50",act_test(50))

            cmd("505",act_test(505))

            cmd("51",act_test(51))

            cmd("515",act_test(515))

            cmd("5151",act_test(5151))

            cmd("5153",act_test(5153))

            cmd("52",act_test(52))

#ifdef OOBAND
			cmd("53",act_test(53))

			cmd("54",act_test(54))

			cmd("55",act_test(55))

			cmd("56",act_test(56))

			cmd("57",act_test(57))

			cmd("58",act_test(58))
#endif

            cmd("n", act_set_synth())

            cmd("pon",act_set_power(1))

            cmd("poff",act_set_power(0))

            cmd("st",act_status())

            cmd("tx",set_tx_state(1))

            cmd("rx",set_tx_state(0))

            cmd("baa",baa())

            cmd("fx", act_test(5081));

            cmd("am", act_test(5084));

            cmd("paon",set_pa_state(1))

            cmd("paoff",set_pa_state(0))
        } while(0);
    }
}

//-----------------------------------------------------------------------------
// Initialization Subroutines
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// PORT_Init
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// Configure the Crossbar and GPIO ports.
//

//-----------------------------------------------------------------------------

#define P00 1<<0
#define P01 1<<1
#define P02 2<<2
#define P03 1<<3
#define P04 1<<4
#define P05 1<<5
#define P06 1<<6
#define P07 1<<7

#define P10 1<<0
#define P11 1<<1
#define P12 2<<2
#define P13 1<<3
#define P14 1<<4
#define P15 1<<5
#define P16 1<<6
#define P17 1<<7

#define SYNTH_LATCH P03
#define SR_LATCH P07
//
// P0.0 - SPI SCK    (digital output, push-pull)
// P0.1 - SPI MISO   (digital input, open-drain)
// P0.2 - SPI MOSI   (digital output, push-pull)
// P0.3 - SPI NSS    (digital output, push-pull) -- look to re-use
// P0.4   digital   push-pull    UART TX
// P0.5   digital   open-drain   UART RX
// P0.6 ?
// P0.7 - MC Micro shiftreg latch
// 
void PORT_Init (void)
{
   //P0MDOUT |= 0x15;                    // Enable UTX as push-pull output, SPI

   P0MDOUT = (P04|P07);    // Enable UTX as push-pull out, SCK, MOSI and SYNTH_LATCH are open drain
   P1MDOUT = 0;

   XBR0     = 0x03;                    // Enable UART on P0.4(TX) and P0.5(RX), SPI also
   XBR1     = 0x40;                    // Enable crossbar and enable weak pull-ups
}

//-----------------------------------------------------------------------------
// SYSCLK_Init
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// This routine initializes the system clock to use the internal oscillator
// at its maximum frequency.
// Also enables the Missing Clock Detector.
//-----------------------------------------------------------------------------

void SYSCLK_Init (void)
{
   OSCICN |= 0x03;                     // Configure internal oscillator for
                                       // its maximum frequency
   RSTSRC  = 0x04;                     // Enable missing clock detector
}

//-----------------------------------------------------------------------------
// UART0_Init
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// Configure the UART0 using Timer1, for <BAUDRATE> and 8-N-1.
//-----------------------------------------------------------------------------

void UART0_Init (void)
{
   SCON0 = 0x10;                       // SCON0: 8-bit variable bit rate
                                       //        level of STOP bit is ignored
                                       //        RX enabled
                                       //        ninth bits are zeros
                                       //        clear RI0 and TI0 bits
   if (SYSCLK/BAUDRATE/2/256 < 1) {
      TH1 = -(SYSCLK/BAUDRATE/2);
      CKCON &= ~0x0B;                  // T1M = 1; SCA1:0 = xx
      CKCON |=  0x08;
   } else if (SYSCLK/BAUDRATE/2/256 < 4) {
      TH1 = -(SYSCLK/BAUDRATE/2/4);
      CKCON &= ~0x0B;                  // T1M = 0; SCA1:0 = 01                  
      CKCON |=  0x01;
   } else if (SYSCLK/BAUDRATE/2/256 < 12) {
      TH1 = -(SYSCLK/BAUDRATE/2/12);
      CKCON &= ~0x0B;                  // T1M = 0; SCA1:0 = 00
   } else {
      TH1 = -(SYSCLK/BAUDRATE/2/48);
      CKCON &= ~0x0B;                  // T1M = 0; SCA1:0 = 10
      CKCON |=  0x02;
   }

   TL1 = TH1;                          // Init Timer1
   TMOD &= ~0xf0;                      // TMOD: timer 1 in 8-bit autoreload
   TMOD |=  0x20;                       
   TR1 = 1;                            // START Timer1
   TI0 = 1;                            // Indicate TX0 ready
}

//-----------------------------------------------------------------------------
// SPI0_Init
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// Configures SPI0 to use 4-wire Single Master mode. The SPI timing is
// configured for Mode 0,0 (data centered on first edge of clock phase and
// SCK line low in idle state).
//
//-----------------------------------------------------------------------------
void SPI0_Init()
{
   SPI0CFG   = 0x40;                   // Enable the SPI as a Master
                                       // CKPHA = '0', CKPOL = '0'
   SPI0CN    = 0x01;                   // 3-wire Single Master, SPI enabled

   // SPI clock frequency equation from the datasheet
   SPI0CKR   = (SYSCLK/(2*SPI_CLOCK))-1;

   ESPI0 = 0;                          // disable SPI interrupts
}


void SPI_Byte_Write (const unsigned char dat)
{
   //printf("writing %02X\n",(unsigned int) dat);

#if 0
   while (!NSSMD0);                    // Wait until the SPI is free, in case
                                       // it's already busy

   NSSMD0 = 0;
#endif

   SPIF=0; // may not be necessary

   SPI0DAT = dat;

   while (TXBMT != 1);

   while (!SPIF);

   SPIF=0;

#if 0
   NSSMD0 = 1;                         // Disable the Slave
#endif

}

#if 0
//-----------------------------------------------------------------------------
// SPI_Array_Write
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// Note: SPI_Data_Array must contain the data to be sent before calling this
// function.
//
// Writes an array of values of size MAX_BUFFER_SIZE to the SPI Slave.  The
// command consists of:
//
// Command = SPI_WRITE_BUFFER
// Length = 1 byte of command, MAX_BUFFER_SIZE bytes of data
//
// Note: Polled mode is used for this function in order to buffer the data
// being sent using the TXBMT flag.
//
//-----------------------------------------------------------------------------
void SPI_Array_Write (void)
{
   unsigned char array_index;

   while (!NSSMD0);                    // Wait until the SPI is free, in case
                                       // it's already busy

   ESPI0 = 0;                          // Disable SPI interrupts

   NSSMD0 = 0;

   SPI0DAT = SPI_WRITE_BUFFER;         // Load the XMIT register
   while (TXBMT != 1)                  // Wait until the command is moved into
   {                                   // the XMIT buffer
   }

   for (array_index = 0; array_index < MAX_BUFFER_SIZE; array_index++)
   {
      SPI0DAT = SPI_Data_Array[array_index]; // Load the data into the buffer
      while (TXBMT != 1)               // Wait until the data is moved into
      {                                // the XMIT buffer
      }
   }
   SPIF = 0;
   while (SPIF != 1)                   // Wait until the last byte of the
   {                                   // data reaches the Slave
   }
   SPIF = 0;

   NSSMD0 = 1;                         // Diable the Slave

   ESPI0 = 1;                          // Re-enable SPI interrupts
}
#endif
