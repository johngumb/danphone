// (14) 3CC08690ABAA 01 GBX434
// LM0043T05F4 original

// TODO slow loop frequency step (R value) selection

/*
 * I2C addresses responding:
 * 22 50 A2 D0
 * First byte of eeprom was 0x05 last byte 0xAE
 *
 * Unit 3CC08692AAAA 04 GBX431
 * LM0304T00M4
 * 0x01 0x04 ... 0x2C (byte 255)
 * Unlocked: 12.288? MHz fast ref, 13 MHz slow ref.
 * 12.283274 .. 12.291572
 * suspect range to be 1200..1400. Bottom end is 1229 MHz.
 * 13.1592 off VCO div for 1320 MHz.
 * 16.7377 kHz off lmx for 1320 MHz.
 * 
 * Unit 3CC08690AAAB 03
 * LM0210T0667
 * SP8855E synth
 * 13MHz slow VCO ref.
 * min freq 1103 MHz
 * max freq 1308 MHz
 * 0x01 0x02 ... 0x28
 *
 * Unit "(09) 3CC08692AAAB 03"
 * eeprom: 0x01 0x04 ... 0x28
 *
 * Unit "(05) 3CC09359ABAB 01"
 * eeprom: 0x00 0x02 ... 0x28
 */
#include <Wire.h>

/*
 * I2C addressess
 */
// PCF 8574 i2c GPIO devices
#define MX105_TXID 0x22
#define MX106_TXID 0x20
#define MX107_TXID 0x21

#define EEPROM_I2CADDR 0x50

typedef byte int8;

const unsigned int EEOFFSET=8;          // preserve existing data, fwiw
const unsigned int EEFREQOFFSET=128;    // drop current frequency in the middle

const int DEFAULT_LOCK_STATUS_ATTEMPTS=10;

typedef enum
{
  Qualcomm_Q3236=1,
  Zarlink_SP8855E
} fast_synth_t;

typedef enum
{
  none=1,
  rough,
  goledge,
} calibration_t;

typedef struct str_eedata
{
  unsigned int m_eeversion;
  unsigned int m_eedatsize;
  unsigned long int m_serialno;
  unsigned int m_lmx_freq_khz;
  unsigned int m_vcxo_freq_khz;
  unsigned int m_freq_min_mhz;
  unsigned int m_freq_max_mhz;
  fast_synth_t m_fast_synth_type;
  calibration_t m_calibration;
  bool m_ignore_vcxo_out_of_range;
  char m_model_pcb[16];
  char m_model_sticker[32];
  char m_model_sticker_lm[16];
  unsigned char m_csum; // must be at end
} __attribute__((packed)) eedata_t;

static eedata_t g_eedata;

unsigned long int g_vcxo_freq, g_curfreq;

bool g_eeprom_ok=false;

void persist_freq(unsigned long int freq)
{
  const unsigned char *freqptr=(unsigned char *)&freq;

  for (unsigned int i=0; (i<sizeof(freq)); i++)
    i2c_eeprom_write_byte(EEPROM_I2CADDR, EEFREQOFFSET+i, freqptr[i]);

  Serial.print(freq);
  Serial.println("Hz saved");
}

// not covered by checksum
unsigned long int get_saved_freq()
{
  unsigned long int freq=0;
  unsigned char *freqptr=(unsigned char *)&freq;

  i2c_eeprom_read_buffer(EEPROM_I2CADDR, EEFREQOFFSET, (byte *) freqptr, sizeof(freq));

  return freq;
}

//PCF 8582C
void i2c_eeprom_write_byte(int deviceaddress, unsigned int eeaddress, byte data)
{
    int rdata = data;
    //Serial.println(data,HEX);
    Wire.beginTransmission(deviceaddress);
    Wire.write((int)(eeaddress & 0xFF));
    Wire.write(rdata);
    Wire.endTransmission();
    delay(8);
}

byte i2c_eeprom_read_byte(int deviceaddress, unsigned int eeaddress)
{
    byte rdata = 0xFF;
    Wire.beginTransmission(deviceaddress);
    Wire.write((int)(eeaddress & 0xFF));
    Wire.endTransmission();
    Wire.requestFrom(deviceaddress,1);
    if (Wire.available()) rdata = Wire.read();
    return rdata;
}

// maybe let's not read more than 30 or 32 bytes at a time!
void i2c_eeprom_read_buffer(int deviceaddress, unsigned int eeaddress, byte *buffer, int length)
{
    Wire.beginTransmission(deviceaddress);
    //Wire.write((int)(eeaddress >> 8)); // MSB
    Wire.write((int)(eeaddress & 0xFF)); // LSB
    Wire.endTransmission();
    Wire.requestFrom(deviceaddress,length);
    int c = 0;
    for ( c = 0; c < length; c++ )
        if (Wire.available()) 
        {
          byte val = Wire.read();
          //Serial.println(val,HEX);
          if (buffer)
            buffer[c]=val;
        }
}

int read_bank(byte i2caddr, byte *data)
{
   int addr=0; //first address
    byte printed=0;
    byte tres[10];
    byte cval;
    byte j;
    byte failed;
    byte b;
    int bank_length = 256;

    while (addr<bank_length)
    {
        failed=true;
        while (failed)
        {
        for (j=0; j<10; j++)
        {
          tres[j] = i2c_eeprom_read_byte(i2caddr, addr); //access an address from the memory
        }
        cval=tres[0];
        for (j=1; j<10; j++)
        {
          if (tres[j]!=cval)
              break;
        }
        if (j==10)
          failed=false;
        }
        
        b=cval;
        if (data)
          data[addr]=cval;

        Serial.print("0x");
        if (b<16)
          Serial.print(0);
        Serial.print(b, HEX); //print content to serial port

        printed++;
        if ((printed%16)==0)
          Serial.println(",");
        else
          Serial.print(",");
          
        addr++;
    }

    return bank_length;
}

void acquit_alarm()
{
  Wire.beginTransmission(MX105_TXID);

  Wire.write((byte)0xF8);   // Ac_Al = 1, LE = 0, clock = 0
  Wire.write((byte)0x68);   // Ac_Al = 0, LE = 0, clock = 0
  Wire.write((byte)0xF8);   // Ac_Al = 1, LE = 0, clock = 0
  Wire.endTransmission();
}

/*
 * NOTE that both synthesiser lock detect outputs are ANDed together
 * in hardware on the board (reported as Al_OL)
 */
byte etat_synthe()
{
  unsigned char a=0xFF;
  Wire.requestFrom(MX105_TXID,1);
  if (Wire.available()) {
    a = Wire.read();
  
    a = ~a & 0x48;    // masque Al_OL : 0x08  déverrouillage PLL
                      // masque Al_SP : 0x40  VCXO hors limites
  
    if (a&0x48)
    {
      if (a&0x08)
        Serial.print("PLL unlocked");
      if ((a&0x48)==0x48)
        Serial.print("; ");
      if (a&0x40)
        Serial.print("VCXO out of range");
      Serial.println();
      Serial.println(a&0x48, HEX);
    }
  }
  return a;     
}

void longint_to_array(unsigned long int val, byte *arr, int length)
{
  for (int i=0;(i<length);i++)
  {
    const int power=((length-1)-i);
    const unsigned long int mask=((unsigned long int)1<<power);

    if (val & mask)
      arr[i]=1;
    else
      arr[i]=0;
  }
}

void sequence_A_lente(unsigned long int F, unsigned long int R)
{
  unsigned int STEP=(13000000/R);
  unsigned long int N=(F/STEP);
  unsigned long int act_F;

// F bits numbered from 1. Two control bits first. F7 if set disconnects charge pump.
//
// Pin 14 (Fo/LD) behaviour:
// F3 F4 F5
// 0  0  0  TRI STATE
// 0  0  1  R divider output (Fr)
// 0  1  0  N divider output: provides divided VCO freq (Fp)
// 0  1  1  Serial data output
// 1  0  0  Lock detect output (see LMX2326.pdf)
// 1  0  1  n-Channel open drain lock detect output (see LMX2326.pdf)
// 1  1  0  Active high
// 1  1  1
//
//      Contenu d'initialisation du registre F du circuit MX310 - LMX2326 :
    // Q3236 and LMX2326 lock detects are ANDed together in status
    byte F_data[21]  = {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,1};         // function latch - original
    //byte F_data[21]  = {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,1,1}; // original - lock nailed up

//      Contenu d'initialisation du registre R du circuit MX310 - LMX2326 :
//      Reference is 13MHz.
//      R = 832;  F_REF = 15.625 kHz;  pas à 24 GHz = 250 kHz :
//byte R_data[21]  = {0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0};         // R latch works

  byte R_data[21]  = {0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0};         // R latch
  byte R_data_calc[21];

  // FREQUENCE DE SORTIE A = 6400,0 MHz :
  //byte N_data[21]         =     {0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1}; // N = 102400


// 1500MHz N=96000
byte N_data[21]         =       {0,0,1,0,1,1,1,0,1,1,1, 0,0,0,0,0,0,0,0, 0,1};
byte N_data_calc[21];
  int i;

  memset(N_data_calc, 0, sizeof(N_data_calc));
  /* N_data_calc[0] is the 'go' bit */
  longint_to_array(N, &N_data_calc[1], 18);
  Serial.print("slow: ");
  Serial.println(N);

  Serial.print("slow step: ");
  Serial.println(STEP);

  act_F = N * (13000000/R);

  longint_to_array(R, &R_data[5], 14);

#if 0
  for (i=0; i<21; i++)
  {
    Serial.print(R_data[i]);
    Serial.println(R_data_calc[i]);
  }
#endif

  Serial.print("slow freq: ");
  Serial.println(act_F);

  N_data_calc[20]=1; // Control bits are 1, 0 i.e. LSB is 1.
 
  Wire.beginTransmission(MX105_TXID);

  for(i=0; i<21; i++)                               // function latch
  {
      Wire.write((byte)(F_data[i])|(0xF8));      // clock = 0
      Wire.write((byte)(F_data[i])|(0xFA));      // clock = 1
      Wire.write((byte)(F_data[i])|(0xF8));      // clock = 0
  }

  Wire.write((byte)0xFC);   // LE = 1, clock = 0 --> transfert
  Wire.endTransmission();
  Wire.write((byte)0xF8); // LE = 0, clock = 0 

  Wire.beginTransmission(MX105_TXID);
  for(i=0; i<21; i++)                               // R latch
  {
      Wire.write((byte)(R_data[i])|(0xF8));              // clock = 0
      Wire.write((byte)(R_data[i])|(0xFA));              // clock = 1
      Wire.write((byte)(R_data[i])|(0xF8));              // clock = 0
  }

  Wire.write((byte)0xFC); // LE = 1, clock = 0 --> transfert
  Wire.endTransmission();
  Wire.write((byte)0xF8); // LE = 0, clock = 0 

  Wire.beginTransmission(MX105_TXID);
  for(i=0; i<21; i++)                               // N latch
  {
    byte *ndata=N_data_calc;
      Wire.write((byte)(ndata[i])|(0xF8));              // clock = 0
      Wire.write((byte)(ndata[i])|(0xFA));              // clock = 1
      Wire.write((byte)(ndata[i])|(0xF8));              // clock = 0
  }

  Wire.write((byte)0xFC); // LE = 1, clock = 0
  Wire.write((byte)0xF8); // LE = 0, clock = 0 
  Wire.endTransmission();
}

void enable_i2c_fast()
{
  Wire.beginTransmission(MX105_TXID);                          
  Wire.write((byte) 0xD8);                        // fermeture commutateurs DG419
  Wire.endTransmission(); 
}

void disable_i2c_fast()
{

  Wire.beginTransmission(MX105_TXID);             // adresse MX105
  Wire.write((byte)0xF8);                         // ouverture commutateurs DG419
  Wire.endTransmission();
}

void calcmx_zarlink(unsigned long int VCXOF, unsigned long int F, int *mx106_out, int *mx107_out)
{
  unsigned long int DIVR, remainder, frac;
  unsigned char mx106, mx107, N;

  DIVR=F/VCXOF;

  remainder = (F - (DIVR * VCXOF));
  frac = (remainder * 64)/VCXOF;

  N = DIVR;

  Serial.print("calcmx_zarlink: ");
  Serial.print("Frequency: ");
  Serial.println(F);
  Serial.print("VCXO frequency: ");
  Serial.println(VCXOF);
  Serial.print("Division ratio: ");
  Serial.println(DIVR);
  Serial.print("Remainder: ");
  Serial.println(remainder);
  Serial.print("Fractional part: ");
  Serial.println(frac);

  mx106=frac+((N&0x03)<<6);
  Serial.print("mx106 0x");
  Serial.println(mx106,HEX);

  mx107=(N>>2);
  Serial.print("mx107 0x");
  Serial.println(mx107,HEX);

  (*mx106_out)=mx106;
  (*mx107_out)=mx107;
  Serial.println("endcalcmx_zarlink");
}

void calcmx_qualcomm(unsigned long int VCXOF, unsigned long int F, int *mx106_out, int *mx107_out)
{
  unsigned long int DIVR, remainder, frac, M, A;
  unsigned char mx106, mx107;

  DIVR=F/VCXOF;

  remainder = (F - (DIVR * VCXOF));
  frac = (remainder * 64)/VCXOF;

  //10*(M+1))+A = DIVR; Q3236 datasheet, dual modulus prescaler in use

  M = (DIVR/10) - 1;
  A = DIVR - (10*(M+1));
  Serial.print("calcmx_qualcom: ");
  Serial.print("Frequency: ");
  Serial.println(F);
  Serial.print("VCXO frequency: ");
  Serial.println(VCXOF);
  Serial.print("Division ratio: ");
  Serial.println(DIVR);
  Serial.print("Remainder: ");
  Serial.println(remainder);
  Serial.print("Fractional part: ");
  Serial.println(frac);
  Serial.print("M "); Serial.println(M);
  Serial.print("A "); Serial.println(A);


//  M=(mx107&0x3C)>>2;
//  A=((mx107&3)<<2)+( (mx106&0xC0)>>6 );
//  frac=mx106&(~0xC0);

  mx106=frac+((A&0x03)<<6);
  Serial.print("mx106 0x");
  Serial.println(mx106,HEX);

  mx107=(A>>2) + (M<<2);
  Serial.print("mx107 0x");
  Serial.println(mx107,HEX);

  (*mx106_out)=mx106;
  (*mx107_out)=mx107;
  Serial.println("endcalcmx_qualcomm");
}

void decode_qualcomm(byte mx106, byte mx107)
{
  unsigned long int M,A,DIVR,frac,F,VCXOF;

  VCXOF=g_vcxo_freq;

  // M3 pin 10 on Q3236
  // R=0, hardwired

  // div=10*(M+1) + A
  
  // M3=32 mx107
  // M2=16 mx107
  // M1=8 mx107
  // M0=4 mx107
  
  // A3==2 mx107
  // A2==1 mx107

  // A1=128 mx106
  // A0==64 mx106

  M=(mx107&0x3C)>>2;
  A=((mx107&3)<<2)+( (mx106&0xC0)>>6 );
  frac=mx106&(~0xC0);

  DIVR=(10*(M+1))+A;

  F=VCXOF*DIVR + ((frac*VCXOF)/64);
  Serial.print("M ");Serial.println(M);
  Serial.print("A ");Serial.println(A);
  Serial.print("Int divide ratio:");
  Serial.println(DIVR);
  Serial.print("Fractional divide ratio:");
  Serial.print(frac);
  Serial.println("/64");
  Serial.print("Fast Freq ");
  Serial.println(F);
}

void sequence_A_rapide(byte mx106, byte mx107)
{
  enable_i2c_fast();

  Wire.beginTransmission(MX107_TXID);             // adresse MX107
  Wire.write((byte)mx107);
  Wire.endTransmission();

  delay(100);

  Wire.beginTransmission(MX106_TXID);             // adresse MX106
  Wire.write((byte)mx106);
  Wire.endTransmission();                         // restart

  delay(100);

  disable_i2c_fast();
}

void decode_zarlink(byte mx106, byte mx107)
{
  unsigned long int DIVR,frac,F,VCXOF;
  unsigned char N;

  VCXOF=g_vcxo_freq;

  // N10=0 (hardwired)
  // N9=0  (hardwired)
  // N8=0  (hardwired)
  // N7=0  (hardwired) maybe bit 4 on mx107 is unused for zarlink based units?
  // N6=16 mx107
  // N5=8 mx107
  // N4=4 mx107
  // N3==2 mx107
  // N2==1 mx107

  // N1=128 mx106
  // N0==64 mx106

  N=((mx107&0x1F)<<2) +( (mx106&0xC0)>>6 );
  frac=mx106&(~0xC0); // low 6 bits

  DIVR=N;

  F=VCXOF*DIVR + ((frac*VCXOF)/64);
  Serial.print("Int divide ratio:");
  Serial.println(N);
  Serial.print("Fractional divide ratio:");
  Serial.print(frac);
  Serial.println("/64");
  Serial.print("Fast Freq ");
  Serial.println(F);
}

byte report_lock_status(int attempts)
{
  byte status=1;
  for (int i=0; i<attempts; i++)
  {
    acquit_alarm();
    delay(1000);

    status = etat_synthe();

    if (g_eedata.m_ignore_vcxo_out_of_range)
      status &= ~0x40;

    // all alarms gone?
    if (status==0)
    {
      Serial.println("Locked");
      break;
    }
  }

  return status;
}

void read_string(String &outstr)
{
  while (1)
  {
    if (Serial.available())
    {
      outstr = Serial.readStringUntil('\n');
      break;
    }
  }
}

void setfreq(unsigned long int F)
{
  int mx106=0, mx107=0;

  // stash the requested frequency - whether it works or not
  g_curfreq=F;

  // TODO clean up slow loop step (i.e. R div)
  //sequence_A_lente(F,650); // hardcoded R div; FIXME 832, 650=20kHz does seem to work
  sequence_A_lente(F,832);

  switch(g_eedata.m_fast_synth_type)
  {
    case Qualcomm_Q3236:
    {
      calcmx_qualcomm(g_vcxo_freq, F, &mx106, &mx107);
      decode_qualcomm(mx106,mx107); // cross check
      sequence_A_rapide(mx106,mx107);
    }
    break;

    case Zarlink_SP8855E:
    {
      calcmx_zarlink(g_vcxo_freq, F, &mx106, &mx107);
      decode_zarlink(mx106,mx107);  // cross check
      sequence_A_rapide(mx106,mx107);
    }
    break;

    default:
    {
      Serial.println("*** ERROR unknown fast synth type");
    }
    break;
  }
}

void report_freq(unsigned long int freq)
{
  unsigned long int output_freq=(freq/1000)*4;
  Serial.print(freq);
  Serial.print("Hz (");
  Serial.print(output_freq);
  Serial.println("kHz)");
}

unsigned char eecsum()
{
  unsigned char csum=0;
  const unsigned char *ptr=(const unsigned char *)&g_eedata;

  // TODO deal with size of structure and version changing
  // -1 to exclude the checksum itself from the summing process
  for (unsigned int i=0;(i<sizeof(g_eedata)-1);i++)
    csum+=ptr[i];

  // might as well include expected size of structure in checksum
  csum+=sizeof(g_eedata);

  return csum;
}

bool check_eeprom_ok()
{
  Serial.println("checking eeprom");

  // offset to avoid existing data
  i2c_eeprom_read_buffer(EEPROM_I2CADDR, EEOFFSET, (byte *) &g_eedata, sizeof(g_eedata));
  //Serial.println(eecsum(), HEX);
  //Serial.println(g_eedata.m_csum, HEX);
  //Serial.println(g_eedata.m_eeversion, HEX);
  return ((eecsum()==g_eedata.m_csum) && (g_eedata.m_eeversion==1));
}

void report_eeprom()
{
  Serial.print("Model (PCB): ");
  Serial.println(g_eedata.m_model_pcb);
  Serial.print("Model (sticker): ");
  Serial.println(g_eedata.m_model_sticker);
  Serial.print("Model (sticker lm): ");
  Serial.println(g_eedata.m_model_sticker_lm);
  //Serial.print("EEversion: ");
  //Serial.println(g_eedata.m_eeversion);
  //Serial.print("Checksum: ");
  //Serial.println(g_eedata.m_csum);
  Serial.print("Serial number: ");
  Serial.println(g_eedata.m_serialno);
  Serial.print("Minimum Frequency: ");
  Serial.print(g_eedata.m_freq_min_mhz);
  Serial.print(" MHz (");
  Serial.print(g_eedata.m_freq_min_mhz*4);
  Serial.println(" MHz)");
  Serial.print("Maximum Frequency: ");
  Serial.print(g_eedata.m_freq_max_mhz);
  Serial.print(" MHz (");
  Serial.print(g_eedata.m_freq_max_mhz*4);
  Serial.println(" MHz)");
  Serial.print("VCXO Centre Frequency: ");
  Serial.print(g_eedata.m_vcxo_freq_khz);
  Serial.println(" kHz");
  Serial.print("Calibration: ");
  Serial.println(g_eedata.m_calibration);
  Serial.print("Fast synth type: ");
  switch(g_eedata.m_fast_synth_type)
  {
    case Zarlink_SP8855E: Serial.println("Zarlink SP8855E"); break;
    case Qualcomm_Q3236: Serial.println("Qualcomm Q3236"); break;
    default: Serial.println("unknown"); break;
  }
  if (g_eedata.m_ignore_vcxo_out_of_range)
    Serial.println("Ignore VCXO out of range hack set");
}

bool setup_eeprom()
{
  const unsigned char *eeptr=(const unsigned char *)&g_eedata;
  String yn;

  Serial.println("Setting up eeprom...");

  Serial.println("Are you sure (Y/n)?");
  read_string(yn);

  if (!yn.equals("Y"))
    return false;

  memset(&g_eedata, 0, sizeof(g_eedata));
  g_eedata.m_eedatsize=sizeof(g_eedata);
  g_eedata.m_eeversion=1;

#if 0
/* Unit "3CC08670ABAA 03" (PCB) "(14) 3CC10676ABAA 04 GBX434" (sticker)
 * LM0043T05F4
 * Qualcomm_Q3236 synth
 * 13MHz slow TCXO ref.
 * 16 MHz fast VCXO ref.
 * min freq 1485 MHz
 * max freq 1718 MHz
 */
  g_eedata.m_serialno=1;
  g_eedata.m_lmx_freq_khz=13000;
  g_eedata.m_calibration=goledge;
  g_eedata.m_vcxo_freq_khz=16000;
  g_eedata.m_fast_synth_type=Qualcomm_Q3236;
  g_eedata.m_freq_min_mhz=1485;
  g_eedata.m_freq_max_mhz=1718;
  strcpy(g_eedata.m_model_pcb,"3CC08670ABAA 03");
  strcpy(g_eedata.m_model_sticker,"(14) 3CC10676ABAA 04 GBX434");
  strcpy(g_eedata.m_model_sticker_lm,"LM0043T05F4");
#endif

#if 0
/* Unit "3CC08697AAAA 02" (PCB) "(24) 3CC08692AAAA 04 GBX431" (sticker)
 * LM0304T00M4
 * Qualcomm_Q3236 synth
 * 13MHz slow TCXO ref.
 * 12.288 MHz fast VCXO ref. Out of range voltage warning odd.
 * min freq 1230 MHz
 * max freq 1400 MHz
 */
  // input level problem on pfd, hacked with potential divider network.
  g_eedata.m_serialno=2;
  g_eedata.m_lmx_freq_khz=13000;
  g_eedata.m_calibration=goledge;
  g_eedata.m_vcxo_freq_khz=12288;
  g_eedata.m_fast_synth_type=Qualcomm_Q3236;
  g_eedata.m_freq_min_mhz=1230;
  g_eedata.m_freq_max_mhz=1400;
  strcpy(g_eedata.m_model_pcb,"3CC08697AAAA 02");
  strcpy(g_eedata.m_model_sticker,"(24) 3CC08692AAAA 04 GBX431");
  strcpy(g_eedata.m_model_sticker_lm,"LM0304T00M4");
  g_eedata.m_ignore_vcxo_out_of_range=true; //hack
#endif

#if 0
/* Unit "3CC09469AAAB03" (PCB) "(07) 3CC09359AAAB 03" (sticker)
 * LM0210T0667
 * SP8855E synth
 * 13MHz slow TCXO ref.
 * 12.288 MHz fast VCXO ref.
 * min freq 1103 MHz
 * max freq 1300 MHz (lock dodgy above that)
 */
  g_eedata.m_serialno=3;
  g_eedata.m_lmx_freq_khz=13000;
  g_eedata.m_calibration=goledge;
  g_eedata.m_vcxo_freq_khz=12288;
  g_eedata.m_fast_synth_type=Zarlink_SP8855E;
  g_eedata.m_freq_min_mhz=1103;
  g_eedata.m_freq_max_mhz=1300;
  strcpy(g_eedata.m_model_pcb,"3CC09469AAAB03");
  strcpy(g_eedata.m_model_sticker,"(07) 3CC09359AAAB 03");
  strcpy(g_eedata.m_model_sticker_lm,"LM0210T0667");
#endif

#if 0
/* Unit "" (PCB) "(09) 3CC08692AAAB 03" (sticker)
 * LM0217T029T
 * SP8855E synth
 * 13MHz slow TCXO ref.
 * 12.288 MHz fast VCXO ref.
 * min freq 1187 MHz
 * max freq 1390 MHz
 */
  g_eedata.m_serialno=4;
  g_eedata.m_lmx_freq_khz=13000;
  g_eedata.m_calibration=rough;
  g_eedata.m_vcxo_freq_khz=12288;
  g_eedata.m_fast_synth_type=Zarlink_SP8855E;
  g_eedata.m_freq_min_mhz=1187;
  g_eedata.m_freq_max_mhz=1390;
  strcpy(g_eedata.m_model_pcb,"");
  strcpy(g_eedata.m_model_sticker,"(07) 3CC09359AAAB 03");
  strcpy(g_eedata.m_model_sticker_lm,"LM0210T0667");
#endif

#if 0
/* Unit "" (PCB) "(05) 3CC09359ABAB 01" (sticker)
 * LM0045T06P7
 * SP8855E synth
 * 13MHz slow TCXO ref.
 * 12.288 MHz fast VCXO ref.
 * min freq 1220 MHz
 * max freq 1390 MHz
 */
  g_eedata.m_serialno=5;
  g_eedata.m_lmx_freq_khz=13000;
  g_eedata.m_calibration=none;
  g_eedata.m_vcxo_freq_khz=12288;
  g_eedata.m_fast_synth_type=Zarlink_SP8855E;
  g_eedata.m_freq_min_mhz=1220;
  g_eedata.m_freq_max_mhz=1439;
  strcpy(g_eedata.m_model_pcb,"");
  strcpy(g_eedata.m_model_sticker,"(05) 3CC09359ABAB 01");
  strcpy(g_eedata.m_model_sticker_lm,"LM0045T06P7");
#endif

  g_eedata.m_csum=eecsum();

  for (unsigned int i=0; (i<(sizeof(g_eedata))); i++)
    i2c_eeprom_write_byte(EEPROM_I2CADDR, i+EEOFFSET, eeptr[i]);

  Serial.println("Setting up eeprom...done");
  return true;
}

int read_eeprom()
{
  int retval=0;

  // uncomment this to setup the eeprom
  //retval=setup_eeprom();

  if (check_eeprom_ok())
  {
    report_eeprom();
    retval = 1;
  }
  else
  {
    Serial.println("Checksum invalid");
    if (retval)
      report_eeprom();
  }

  return retval;
}

void reboot() {
  Serial.println("rebooting...");
  Serial.println();
  _PROTECTED_WRITE(WDT.CTRLA,WDT_PERIOD_8CLK_gc); // arm the watchdog
  while (1); // upset the 'dog.
}

void setup() {
  // board runs at 20MHz.

  Wire.begin();
  Wire.setTimeout(10000); // no idea if this helps with eeprom read hangs
  Serial.begin(115200);
  Serial.setTimeout(100);
  Serial.println("\nboot");

  // delay prevents hang on read_eeprom
  // To recover from hang, try stopping reading eeprom as first action and re-upload
  // hence if (0) below.
  delay(1500);

#if 0
  while (1)
 {
    read_bank(EEPROM_I2CADDR, NULL);
    delay(20000000);
 }
 #endif

  if (read_eeprom())
  //if(0)
  {
    Serial.println("eeprom read ok");
    g_vcxo_freq = ((unsigned long int) g_eedata.m_vcxo_freq_khz) * 1000;
    g_eeprom_ok = true;
  }
  else
  {
    Serial.println("eeprom not read ok");
    g_vcxo_freq = 12288000;
    g_eeprom_ok=false;
  }

  Serial.print("VCXO freq is ");
  Serial.println(g_vcxo_freq);

  if (g_eeprom_ok)
  {
    unsigned long int Fsaved(get_saved_freq());
    Serial.print("Setting saved frequency (Hz): ");
    Serial.println(Fsaved);

    setfreq(Fsaved);

    report_lock_status(DEFAULT_LOCK_STATUS_ATTEMPTS);
  }

  Serial.println("setup done");
}

void loop() {
  String freqstr;
  unsigned long int F;
  int attempts;

  attempts=DEFAULT_LOCK_STATUS_ATTEMPTS;
  Serial.print("Current requested frequency: ");
  report_freq(g_curfreq);

  Serial.println("Enter special values 0 for status and current frequency; 1 for reboot");
  Serial.println("Freq (Hz)?");

  read_string(freqstr);

  F=freqstr.toInt();

  // I suppose we need to think about a CLI here
  if (F>1)
  {
    Serial.print("Requested frequency: ");
    report_freq(F);
    setfreq(F);
  }
  else if (F==0)
  {
    // print this before status as that takes a while
    Serial.print("Current requested frequency: ");
    report_freq(g_curfreq);
    attempts=1;
  }
  else if (F==1)
  {
    reboot();
  }

  // save frequency if locked and we have a valid frequency
  if ((report_lock_status(attempts)==0) && (F>1))
     persist_freq(F);

  Serial.println();
}
