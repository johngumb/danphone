/*
* OpenPMR - tools to make old PMR radios useful.
*
* Copyright (C) 2013-2018  John Gumb, G4RDC
*
* This file is part of OpenPMR.

* OpenPMR is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OpenPMR is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

* You should have received a copy of the GNU General Public License
* along with OpenPMR.  If not, see <http://www.gnu.org/licenses/>.
*/

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

//-----------------------------------------------------------------------------
// ADC stuff: 16-bit SFR Definitions for 'F35x
//-----------------------------------------------------------------------------

sfr16 ADC0DEC = 0x9A;                  // ADC0 Decimation Ratio Register

typedef union LONGDATA{                // Access LONGDATA as an
   unsigned long result;               // unsigned long variable or
   unsigned char Byte[4];              // 4 unsigned byte variables
}LONGDATA;

// With the Keil compiler and union byte addressing:
// [0] = bits 31-24, [1] =  bits 23-16, [2] = bits 15-8, [3] = bits 7-0
#define Byte3 0
#define Byte2 1
#define Byte1 2
#define Byte0 3

LONGDATA rawValue;

// uncomment one
#define SIXMETRES
//#define FOURMETRES
//#define TWOMETRES

#define TESTING
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

#define MDCLK         2457600          // Modulator clock in Hz (ideal is
                                       // (2.4576 MHz)
// timer 2 stuff
#define LED_TOGGLE_RATE            160  // LED toggle rate in milliseconds
                                       // if LED_TOGGLE_RATE = 1, the LED will
                                       // be on for 1 millisecond and off for
                                       // 1 millisecond

#define LED_TOGGLE_RATE_SCALED     20

#define TIMER2_SCALE (LED_TOGGLE_RATE/LED_TOGGLE_RATE_SCALED)

/* sbit unused_input=P0^1; look to re-use this; was used for 9.6V status */
sbit synth_latch_bit=P0^3;
sbit shift_reg_latch_bit=P0^7;
sbit dac_select_bit=P1^2;  /* also used as input for power-on indication for  */
                           /* 9.6V i.e. RF board power. ~10K resistor divider */
                           /* gives about 4.8V on this pin and also acts as a */
                           /* pull-up for the ref osc dac ~SEL pin.           */

sbit squelch_bit=P1^0;
sbit rf_power_pot_select_bit=P1^0;
sbit locked_bit=P1^1;
sbit power_on_bit=P1^2;
sbit rts_bit=P1^3; /* goes back to RS232 as CTS */
sbit cts_bit=P1^4; /* comes from RS232 as RTS */
sbit pin15_open_drain=P1^5;
sbit pin1_open_drain=P1^6;
sbit pin2_input_bit=P1^7; /* pin 2 off DB15 connector */
sbit rf_pa_status = P2^0;

#define SYNTH_LATCH_ID 1
#define SHIFT_REG_LATCH_ID 2

static char str[20];

// IF offset: 34108 for 4KHz step, 10816 for 12.5KHz step
static unsigned int g_last_tx, g_if_offset=10816;

static unsigned char g_t2_timeout;

#define REPORTING 0
#if REPORTING
#define CHAR_WAIT_MAX 1000

char g_line_in_progress;
static char g_reporting;
#endif

static unsigned char g_timer2_count, g_control_byte;

// Timer2 SFR
sfr16 TMR2RL = 0xCA;                   // Timer2 Reload Register
sfr16 TMR2 = 0xCC;                     // Timer2 Register
//-----------------------------------------------------------------------------
// Function PROTOTYPES
//-----------------------------------------------------------------------------

void SYSCLK_Init(void);
void UART0_Init(void);
void PORT_Init(void);
void Timer2_Init(void);
void SPI0_Init(void);
void PCA0_Init(void);
void ADC0_Init(void);

unsigned char SPI_Byte_Write (const unsigned char);

void pulsebithigh(const char latch_id);

void write_synth_spi(const unsigned int *);

void ref_dac_init(void);

unsigned long read_adc(void);

// delay() between dac_select_bit and synth_latch_bit so we are guaranteed
// dac_select_bit effect has propogated through so we can safely set
// synth_latch_bit

#define SQUELCH_POT_SELECT  {dac_select_bit=0; delay(5); synth_latch_bit=1; delay(20);}
#define SQUELCH_POT_DESELECT {dac_select_bit=1; synth_latch_bit=0;}

#define POWER_POT_SELECT  {rf_power_pot_select_bit=0; delay(10);}
#define POWER_POT_DESELECT rf_power_pot_select_bit=1

void init_squelch_potentiometer();

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
#ifdef SIXMETRES
// 50.016 MHz + 21.4 MHz
#define SYN_REF1 0x1C
#define SYN_REF2 0x21
#define SYN_DIV1 0x01
#define SYN_DIV2 0xBE
#define SYN_DIV3 0x1C
#endif

#ifdef FOURMETRES
// 70.45 MHz + 21.4 MHz
// GB3BAA is dead on 4
#define SYN_REF1 0x09
#define SYN_REF2 0x01
#define SYN_DIV1 0x00
#define SYN_DIV2 0xB7
#define SYN_DIV3 0x38
#endif

    SPI_Byte_Write(SYN_REF1);
    SPI_Byte_Write(SYN_REF2);
    pulsebithigh(SYNTH_LATCH_ID);

    SPI_Byte_Write(SYN_DIV1);
    SPI_Byte_Write(SYN_DIV2);
    SPI_Byte_Write(SYN_DIV3);
    pulsebithigh(SYNTH_LATCH_ID);

    // 34108 for 4KHz step
    g_if_offset=34108;
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
        // 34108 for 4KHz step
        // 10816 for 12.5KHz step
        w[1]=g_last_tx + g_if_offset; // 21.4 MHz IF offset

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


char getchar_jag (void)
{
   char c;
#if REPORTING
   int i;


   if ( g_line_in_progress)
       while (!RI0);

    else
    {
       for (i=0; (i<CHAR_WAIT_MAX); i++)
       {
          if (RI0)
            break;

          if (i==CHAR_WAIT_MAX)
            return -1;
        }
    }
#else
    while (!RI0);
#endif
   
   c = SBUF0;
   RI0 = 0;

   return c;
}

void getstr(char *str)
{
    char c;
    int ptr=0;

    while (1)
    {
        c = getchar_jag();
#if REPORTING
        if (c==-1)
        {
            g_line_in_progress=1;
            return c;
        }
        else 
#endif
        if (! iswhitespace(&c))
        {
#if REPORTING
            g_line_in_progress=1;
#endif

            str[ptr++]=c;
        }
        else
        {
#if REPORTING
            if (c=='\n')
                g_line_in_progress=1;
#endif
            break;
        }
    }

    str[ptr]=0;
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
		printf("HEX ERROR:%x\n",(unsigned) ch);

	return val;
}

char bytetohexdigit(unsigned char val)
{
    if (val>9)
        return (val-10)+'A';
    else
        return val+'0';
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

void act_stbyte();

void act_control(void)
{
    unsigned char val = strtohex(&str[1]);

	SPI_Byte_Write(val);

	pulsebithigh(SHIFT_REG_LATCH_ID);

    /* allow unit to power up */
    if (val & SR_POWER)
    {
        while(!power_on_bit);

        ref_dac_init();

        init_squelch_potentiometer();
    }

    g_control_byte = val;

    act_stbyte();
}

void act_synth(void)
{
	unsigned char offset=1; // skip first command string byte "S"

	// deal with, for example "SD9C" or "S09C"
	if ((strlen(str)%2)==0)
	{
		unsigned char fval;

		fval=hexdigittobyte(str[offset++]);

		SPI_Byte_Write(fval);
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

    act_stbyte();
}

void act_sync_required(void)
{
    // extend this to specify the exact delay required
    TMR2RL = -((strtohex(&str[1])<<8) + strtohex(&str[3])); // Reload value to be used in Timer2

    if (TMR2RL)
    {
        TMR2 = TMR2RL;                      // Init the Timer2 register

        TMR2CN = 0x04;                      // Enable Timer2 in auto-reload mode
        ET2 = 1;                            // Timer2 interrupt enabled
        EA = 1;                             // Enable global interrupts
    }
    else
    {
        TMR2CN = 0;
        ET2 = 0;
        EA = 0;
    }

    act_stbyte();
}

void write_ref_dac(unsigned char cmd, unsigned char d_hi, unsigned char d_lo)
{
	dac_select_bit=0;

	SPI_Byte_Write(cmd);

	SPI_Byte_Write(d_hi);

	SPI_Byte_Write(d_lo);

	dac_select_bit=1;
}

#define REF_DAC_CMD(rdcmd) ((rdcmd)<<3)

void ref_dac_init(void)
{
	// use specifed external reference from KXN1123AA
	write_ref_dac(REF_DAC_CMD(8), 0, 10);  // using register 8, VREF reg

    // gain control register - 1x gain
    write_ref_dac(REF_DAC_CMD(10), 0, 0);  // using register 10, gain control register
}

// Write to MCP48FEB22 12 bit DAC controlling 14.4MHz synth ref osc
// 0x000 14.398925 MHz
// 0xFFF 14.400251 MHz 4.2031V as measured on KXN1123AA input pin
void act_ref_dac()
{
	unsigned char dacno, data_high, data_low;

    // format of DAC command
    // DNABC
    // D: Dac command (that's how we got here so skip it)
    // N: Dac number
    //    0: DAC 0
    //    1: DAC 1
    //    2: Both
    //    3: DAC 0 written with ABC, DAC 1 written with ABC-1
    //    4: DAC 0 written with ABC, DAC 1 written with ABC+1
    // A: top 4 bits of DAC value 0-F
    // B: middle 4 bits of DAC value 0-F
    // C: bottom 4 bits of DAC value 0-F

    dacno = hexdigittobyte(str[1]);

    data_high = hexdigittobyte(str[2]);

    // must be 2 characters remaining
    data_low = strtohex(&str[3]);

    //printf("dacno %x\n",(unsigned) dacno);
    //printf("data_high %x\n",(unsigned)  data_high);
    //printf("data_low %x\n",(unsigned) data_low);

    if (TMR2RL)
    {
        while (g_t2_timeout);

        g_t2_timeout=1;
    }

    if (dacno >= 2)
    {
        write_ref_dac(REF_DAC_CMD(0), data_high, data_low);

        switch(dacno)
        {
            case 3:
                data_low-=1;
            break;

            case 4:
                data_low+=1;
            break;
        }
        write_ref_dac(REF_DAC_CMD(1), data_high, data_low); 
    }
    else
	    // using register "dacno", write a value to DAC specified
        write_ref_dac(REF_DAC_CMD(dacno), data_high, data_low);

    act_stbyte();
}

void init_pot_spi(void)
{
    // init TCON (Terminal Control Register)
    SPI_Byte_Write(0x40);    // TCON at address 4
    SPI_Byte_Write(0x0F);    // Everything enabled/connected
}

void init_squelch_potentiometer(void)
{
    SQUELCH_POT_SELECT;
    init_pot_spi();
    SQUELCH_POT_DESELECT;
}

void act_squelch_pot(void)
{
	unsigned char pot_data;

    // format of POT command
    // QAB
    // Q: Pot command (that's how we got here so skip it)
    // A: top 4 bits of pot value 0-F
    // B: bottom 4 bits of pot value 0-F

    // must be 2 characters remaining
    pot_data = strtohex(&str[1]);

    //printf("pot_data %x\n",(unsigned) pot_data);

    SQUELCH_POT_SELECT;

    SPI_Byte_Write(0);     // volatile writes - address 0
    SPI_Byte_Write(pot_data);

    SQUELCH_POT_DESELECT;

    act_stbyte();
}

#define BUGCHECK_PP 255
#define crash(crashcode) {while(1) { printf("BUG %d\n",(unsigned) crashcode); delay(1000); }}
//#define crash(crashcode) printf("BUG %d\n",(unsigned) crashcode);

void act_power_pot(void)
{
	unsigned char pot_data, i;

    // format of POT command
    // PAB
    // P: Pot command (that's how we got here so skip it)
    // A: top 4 bits of pot value 0-F
    // B: bottom 4 bits of pot value 0-F

    // must be 2 characters remaining
    pot_data = strtohex(&str[1]);

#define MAX_POWERPOT_ATTEMPTS 5
    for (i=0; (i<MAX_POWERPOT_ATTEMPTS); i++)
    {
    unsigned char read_low;

    POWER_POT_SELECT;

    SPI_Byte_Write(0);     // volatile writes - address 0
    SPI_Byte_Write(pot_data);

    POWER_POT_DESELECT;

    delay(10);

	// read back
    POWER_POT_SELECT;

    SPI_Byte_Write(0x0C); // high byte
	read_low = SPI_Byte_Write(0xFF); // low byte

	POWER_POT_DESELECT;

    if (pot_data == read_low)
    {
        break;
    }
    }

    if (i==MAX_POWERPOT_ATTEMPTS)
    {
        crash(BUGCHECK_PP);
    }

    act_stbyte();
}

void act_ctcss(void)
{
	unsigned char toneval;

	toneval = strtohex(str+1);

	if (!toneval)
		CR = 0;
	else
	{
		// Start PCA counter
		CR = 1;

		PCA0CPH0 = toneval;
	}

	//printf("CTCSS: %02x\n", (unsigned) toneval);

       act_stbyte();
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
        while(!power_on_bit);

#if defined(SIXMETRES)
#define REF_DAC_INIT_HI 0x0C
#define REF_DAC_INIT_LO 0x3E

#elif defined(FOURMETRES)
#define REF_DAC_INIT_HI 0x0C
#define REF_DAC_INIT_LO 0x3E

#elif defined(TWOMETRES)
#define REF_DAC_INIT_HI 0x0C
#define REF_DAC_INIT_LO 0x3E

#endif
        ref_dac_init();
        write_ref_dac(REF_DAC_CMD(0), REF_DAC_INIT_HI, REF_DAC_INIT_LO);

        write_ref_dac(REF_DAC_CMD(1), REF_DAC_INIT_HI, REF_DAC_INIT_LO);

        init_squelch_potentiometer();

        baa();
    }
}

void act_status()
{
    if (!power_on_bit)
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

unsigned char stval()
{
    char result=0;

    /* TODO simply read P11 byte as bits map directly to what we return here */
    /* possibly...                                                           */
    /* Doing this might mean un-meaningful values for locked/squelch get     */
    /* returned if the unit is powered off but in the interests of           */
    /* efficiency it might be worth tolerating this.                         */
    if (power_on_bit)
    {
        result+=4;

        if (locked_bit)
            result+=1;

        if (squelch_bit)
            result+=2;

        if (rf_pa_status)
            result +=8;
    }

    return result;
}

void act_stbyte()
{
    putchar(bytetohexdigit(stval()));
    putchar('K');
    putchar('\n');
}

void act_temperature()
{
    while(!AD0INT);
    printf("%lx",read_adc());
    AD0INT = 0;                         // clear AD0 interrupt flag
    act_stbyte();
}

#if REPORTING
void act_report(char startstop)
{
    g_reporting=startstop;
}
#endif

#ifdef TESTING
void set_rts(int state)
{
    rts_bit = state;
}


void set_pin15_open_drain(int state)
{
    pin15_open_drain = state;
}

void get_cts()
{
    char cts_val = cts_bit;
    putchar('0'+cts_val);
}

void get_pin2()
{
    char pin2_val = pin2_input_bit;
    putchar('0'+ pin2_val);
}

void set_pin1_open_drain(int state)
{
   pin1_open_drain = state;
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

		case 7045:
		{
			w[1]=35912;

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

    // 10816 for 12.5KHz step
    g_if_offset=10816;

    write_synth_spi(&w);
}
#endif

void act_hostsync()
{
    putchar(str[1]);

    putchar('Y');

    putchar('\n');
}

unsigned long read_adc(void)
{
    unsigned long mV;

   // Copy the output value of the ADC
   rawValue.Byte[Byte3] = 0x00;
   rawValue.Byte[Byte2] = (unsigned char)ADC0H;
   rawValue.Byte[Byte1] = (unsigned char)ADC0M;
   rawValue.Byte[Byte0] = (unsigned char)ADC0L;

   //                           Vref (mV)
   //   measurement (mV) =   --------------- * result (bits)
   //                       (2^24)-1 (bits)
   //
   //   measurement (mV) =  result (bits) / ((2^24)-1 (bits) / Vref (mV))
   //
   //
   //   With a Vref (mV) of 2500:
   //
   //   measurement (mV) =  result (bits) / ((2^24)-1 / 2500)
   //
   //   measurement (mV) =  result (bits) / ((2^24)-1 / 2500)
   //
   //   measurement (mV) =  result (bits) / (16777215 / 2500)
   //
   //   measurement (mV) =  result (bits) / (6710)

   mV = rawValue.result / 6710;        // Because of bounds issues, this
                                       // calculation has been manipulated as
                                       // shown above
                                       // (i.e. 2500 (VREF) * 2^24 (ADC result)
                                       // is greater than 2^32)

//   printf("AIN0.2 voltage: %4ld mV\n",mV);
   return mV;
}

#define cmd(_cmpstr,_rtn) if (strcmp(str, _cmpstr)==0) {_rtn; break;}

#define partcmd(_cmpchar, _rtn) if (str[0]==_cmpchar) {_rtn; break; }

void main (void) 
{  
#if REPORTING
    char last_cstat; 
    g_line_in_progress = 0;
    g_reporting = 0;
#endif

    PCA0MD &= ~0x40;                    // WDTE = 0 (clear watchdog timer enable)
    SYSCLK_Init ();                     // Initialize Oscillator
    UART0_Init();
    SPI0_Init();
	PCA0_Init();
    PORT_Init();                        // Initialize Port I/O
    ADC0_Init();
    Timer2_Init();

    latch_init();

    // initialise power pot - setup
    POWER_POT_SELECT;
    init_pot_spi();
    POWER_POT_DESELECT;

    // come up powered on to allow ref osc to stabilise
    act_set_power(1);

    // default freq of 51.53
    g_last_tx=26372;

    // default freq of 70.45
    //g_last_tx=35912;

    AD0INT = 0;                            // clear pending sample indication
    ADC0MD = 0x83;                         // Start continuous conversions

    while (1)
    {
        getstr(&str);

        do {
			partcmd('C', act_control());

            partcmd('H', act_temperature());

			partcmd('S', act_synth());

			partcmd('T', act_ctcss());

			partcmd('D', act_ref_dac());

            partcmd('E', act_sync_required());

            partcmd('Q', act_squelch_pot());

            partcmd('P', act_power_pot());

#if REPORTING
            cmd("X", act_report(0))

            cmd("Y", act_report(1))
#endif
            partcmd('R', act_hostsync())

            cmd("Z", act_stbyte());

#ifdef TESTING
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
#endif

            cmd("n", act_set_synth())

            cmd("pon",act_set_power(1))

            cmd("poff",act_set_power(0))

            cmd("st",act_status())

            cmd("tx",set_tx_state(1))

            cmd("rx",set_tx_state(0))

            cmd("baa",baa())

            cmd("paon",set_pa_state(1))

            cmd("paoff",set_pa_state(0))

#ifdef TESTING
            cmd("fx", act_test(5081));

            cmd("am", act_test(5084));

            cmd("rtson", set_rts(1))

            cmd("rtsoff", set_rts(0))

            cmd("getcts", get_cts())

            cmd("getpin2", get_pin2())

            cmd("pin15on", set_pin15_open_drain(1))

            cmd("pin15off", set_pin15_open_drain(0))

            cmd("pin1on", set_pin1_open_drain(1))

            cmd("pin1off", set_pin1_open_drain(0))
#endif

#if REPORTING
            if (g_reporting) {
                char cstat=stval();

                if (cstat!=last_cstat)
                {
                    act_stbyte();
                }
                last_cstat=cstat;
             }
#endif
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
#define P02 1<<2
#define P03 1<<3
#define P04 1<<4
#define P05 1<<5
#define P06 1<<6
#define P07 1<<7

#define P10 1<<0
#define P11 1<<1
#define P12 1<<2
#define P13 1<<3
#define P14 1<<4
#define P15 1<<5
#define P16 1<<6
#define P17 1<<7

#define SYNTH_LATCH P03
#define SR_LATCH P07
//
// P0.0 - SPI SCK    (digital output, push-pull)
// P0.1 - SPI MISO   (digital input, open-drain) -- look to re-use
// P0.2 - SPI MOSI   (digital output, push-pull)
// P0.3 - SPI NSS    (digital output, push-pull) -- synth latch
// P0.4   UART TX
// P0.5   UART RX
// P0.6 - CTCSS tone
// P0.7 - MC Micro shiftreg latch
// P1.0 - status - squelch open
// P1.1 - status - synth lock bit
// P1.2 - status - power on
// P1.2 - output - synth dac chip select
// P1.3 - output - CTS as seen by host
// P1.4 - status - RTS from host as seen by us
// P1.5 - output - Pin 12 on CPU to drive pin 15 on 'D' type (MOSFET)
// P1.6 - output - Pin 11 on CPU to drive pin 1 on 'D' type (MOSFET)
// P1.7 - input  - Pin 15 on CPU from pin 2 on 'D' type ground for logic 1
// P2.0 - input  - PA status
//
// must be push-pull for ft232 test lead
#define UART_TX_OPEN_DRAIN
void PORT_Init (void)
{
   // NSS gets used as a GPIO pin
   P0SKIP = P03;

#ifdef UART_TX_OPEN_DRAIN
   P0MDOUT = (P06|P07);     // Enable UTX (P04) as open drain out, SCK, MOSI and SYNTH_LATCH are open drain
#else
   P0MDOUT = (P00|P02|P04|P06|P07); // Enable UTX (P04) as push-pull out, SCK, MOSI and SYNTH_LATCH are open drain
#endif

   P1MDOUT = (P13|P15|P16);

   XBR0    = 0x03;                     // Enable UART on P0.4(TX) and P0.5(RX), SPI also
   XBR1    = 0x41;                     // Route CEX0 to a port pin
                                       // Enable crossbar and weak pull-ups

   pin1_open_drain=0;   // MOSFET outputs disabled
   pin15_open_drain=0;  // MOSFET outputs disabled
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
   OSCICN = 0x83;                     // Configure internal oscillator for
                                       // its maximum frequency
   RSTSRC  = 0x04;                     // Enable missing clock detector

   CLKSEL = 0;                          // Select the internal osc. as
                                        // the SYSCLK source
}

#define TIMER_PRESCALER            12  // Based on Timer2 CKCON and TMR2CN
                                       // settings

// There are SYSCLK/TIMER_PRESCALER timer ticks per second, so
// SYSCLK/TIMER_PRESCALER/1000 timer ticks per millisecond.
// 2042
#define TIMER_TICKS_PER_MS  SYSCLK/TIMER_PRESCALER/1000

// Note: LED_TOGGLE_RATE*TIMER_TICKS_PER_MS should not exceed 65535 (0xFFFF)
// for the 16-bit timer

//#define AUX1     (TIMER_TICKS_PER_MS+50)*LED_TOGGLE_RATE_SCALED

// measured 320ms period - offset 559. 567 seems to work best with FT8
#define AUX1     ((TIMER_TICKS_PER_MS*LED_TOGGLE_RATE_SCALED)+567)
#define AUX2     -AUX1

#define TIMER2_RELOAD            AUX2  // Reload value for Timer2


//-----------------------------------------------------------------------------
// Timer2_Init
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// This function configures Timer2 as a 16-bit reload timer, interrupt enabled.
// Using the SYSCLK at 24.5MHz with a 1:12 prescaler.
//
// Note: The Timer2 uses a 1:12 prescaler.  If this setting changes, the
// TIMER_PRESCALER constant must also be changed.
//-----------------------------------------------------------------------------
void Timer2_Init(void)
{
   CKCON &= ~0x60;                     // Timer2 uses SYSCLK/12
   TMR2CN &= ~0x01;
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


unsigned char SPI_Byte_Write (const unsigned char dat)
{
   //printf("writing %02X\n",(unsigned int) dat);

   SPIF=0;

   SPI0DAT = dat;

   while (!SPIF);

   return SPI0DAT;
}

void PCA0_Init (void)
{
   // Configure PCA time base; overflow interrupt disabled
   PCA0CN = 0x00;                      // Stop counter; clear all flags
   PCA0MD = 0x00;                      // Use SYSCLK/12 as time base

   PCA0CPM0 = 0x46;                    // Module 0 = Frequency Output mode

   // Configure frequency for CEX0
   // PCA0CPH0 = (SYSCLK/12)/(2*CEX0_FREQUENCY), where:
   // SYSCLK/12 = PCA time base
   // CEX0_FREQUENCY = desired frequency
//   PCA0CPH0 = (SYSCLK/12)/(2*CEX0_FREQUENCY);
    // [jag] cannot use above calculation on target - doesn't work
    // presumably due to 8 bit target. Better to calculate this value on host.
    //PCA0CPH0 = 207; // 77 Hz
    //PCA0CPH0 = 193; // 82.5Hz

   // PCA counter initially disabled
   CR = 0;
}

//-----------------------------------------------------------------------------
// ADC0_Init
//-----------------------------------------------------------------------------
//
// Return Value : None
// Parameters   : None
//
// Initialize the ADC to use the temperature sensor. (non-differential)
//
//-----------------------------------------------------------------------------
void ADC0_Init (void)
{
   REF0CN |= 0x01;                     // Enable internal Vref
   ADC0CN = 0x00;                      // Gain = 1, Unipolar mode
   ADC0CF = 0x00;                      // Interrupts upon SINC3 filter output
                                       // and uses internal VREF

   ADC0CLK = (SYSCLK/MDCLK)-1;         // Generate MDCLK for modulator.
                                       // Ideally MDCLK = 2.4576MHz

#define OWR                1          // Desired Output Word Rate in Hz

   // Program decimation rate for desired OWR
   ADC0DEC = ((unsigned long) MDCLK / (unsigned long) OWR /
              (unsigned long) 128) - 1;

   ADC0BUF = 0x00;                     // Turn off Input Buffers
   //ADC0MUX = 0x28;                     // Select AIN0.2
   ADC0MUX = 0xF8;  // use '8' as an 'all other values' value

   ADC0MD = 0x81;                      // Start internal calibration
   while(AD0CALC != 1);                // Wait until calibration is complete

#if 0
   EIE1   |= 0x08;                     // Enable ADC0 Interrupts
   ADC0MD  = 0x80;                     // Enable the ADC0 (IDLE Mode)

   AD0INT = 0;
   ADC0MD = 0x83;                      // Start continuous conversions
   EA = 1;                             // Enable global interrupts
#endif
}

//-----------------------------------------------------------------------------
// Interrupt Service Routines
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Timer2_ISR
//-----------------------------------------------------------------------------
//
// Here we process the Timer2 interrupt and toggle the LED
//
//-----------------------------------------------------------------------------
void Timer2_ISR (void) interrupt 5
{
   g_timer2_count+=1;

   if ((g_timer2_count%TIMER2_SCALE)==0)
    {
    g_t2_timeout=0;
    //pin15_open_drain = ~pin15_open_drain;
    }
   else
    g_t2_timeout=1;

   TF2H = 0;                           // Reset Interrupt
}

#if 0
//-----------------------------------------------------------------------------
// ADC0_ISR
//-----------------------------------------------------------------------------
//
// This ISR prints the result to the UART. The ISR is called after each ADC
// conversion.
//
//-----------------------------------------------------------------------------
void ADC0_ISR (void) interrupt 10
{
   unsigned long mV;

   while(!AD0INT);                     // Wait till conversion complete
   //AD0INT = 0;                         // Clear ADC0 conversion complete flag

   // Copy the output value of the ADC
   rawValue.Byte[Byte3] = 0x00;
   rawValue.Byte[Byte2] = (unsigned char)ADC0H;
   rawValue.Byte[Byte1] = (unsigned char)ADC0M;
   rawValue.Byte[Byte0] = (unsigned char)ADC0L;

   //                           Vref (mV)
   //   measurement (mV) =   --------------- * result (bits)
   //                       (2^24)-1 (bits)
   //
   //   measurement (mV) =  result (bits) / ((2^24)-1 (bits) / Vref (mV))
   //
   //
   //   With a Vref (mV) of 2500:
   //
   //   measurement (mV) =  result (bits) / ((2^24)-1 / 2500)
   //
   //   measurement (mV) =  result (bits) / ((2^24)-1 / 2500)
   //
   //   measurement (mV) =  result (bits) / (16777215 / 2500)
   //
   //   measurement (mV) =  result (bits) / (6710)

   mV = rawValue.result / 6710;        // Because of bounds issues, this
                                       // calculation has been manipulated as
                                       // shown above
                                       // (i.e. 2500 (VREF) * 2^24 (ADC result)
                                       // is greater than 2^32)

   printf("AIN0.2 voltage: %4ld mV\n",mV);
}
#endif
