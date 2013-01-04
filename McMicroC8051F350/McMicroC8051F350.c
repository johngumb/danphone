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

//-----------------------------------------------------------------------------
// Global CONSTANTS
//-----------------------------------------------------------------------------

#define SYSCLK      24500000           // SYSCLK frequency in Hz
#define BAUDRATE        115200           // Baud rate of UART in bps

#define SPI_CLOCK          300000      // Maximum SPI clock
                                       // The SPI clock is a maximum of 500 kHz
                                       // when this example is used with
                                       // the SPI0_Slave code example.


//sbit LED = P0^7;                          // LED='1' means ON

sbit synth_latch_bit=P0^3;
sbit shift_reg_latch_bit=P0^7;

#define SYNTH_LATCH_ID 1
#define SHIFT_REG_LATCH_ID 2

static char str[20];
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


typedef enum
{
SYNTH_VAL_TYPE_REF_DIVIDER,
SYNTH_VAL_TYPE_COUNTER} synth_val_type_t;

void act_set_synth(const synth_val_type_t synth_val_type)
{
	char latch_id=SYNTH_LATCH_ID;

	latch_id = SYNTH_LATCH_ID;

	getstr(str);

//	LED=~LED;

	if (synth_val_type==SYNTH_VAL_TYPE_COUNTER)
	{
		unsigned int n, a;
		const unsigned char *datptr=&n;

		n=atoi(str);

		getstr(str);

		a=atoi(str);

		printf("n: %x a: %x\n",n,a);

		// write n to SPI
		latch(0, latch_id);
		SPI_Byte_Write(datptr[0]);
		SPI_Byte_Write(datptr[1]);

		// write second val to SPI
		datptr=(const unsigned char *) &a;
		SPI_Byte_Write(datptr[0]);
		SPI_Byte_Write(datptr[1]);

		pulsebithigh(latch_id);

		// write second val to SPI
		printf("setting n\n");

	}
	else
	{
		unsigned int r;
	    r=atoi(str);
		printf("setting r %ud\n", r);
    }
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


   while (1)
   {
       getstr(&str);

       if (strcmp(str,"u")==0)
           act_up(0);

	   else if (strcmp(str,"r")==0)
           act_set_synth(SYNTH_VAL_TYPE_REF_DIVIDER);

	   else if (strcmp(str,"n")==0)
           act_set_synth(SYNTH_VAL_TYPE_COUNTER);

	   else if (strcmp(str,"pon")==0)
           act_set_power(1);

	   else if (strcmp(str,"poff")==0)
           act_set_power(0);

	   else if (strcmp(str,"txen")==0)
           act_set_power(2);

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
