// 3CC08690ABAA

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
 * suspect range to be 1200..1400
 */
#include <Wire.h>

#define MX105_TXID 0x22
#define MX106_TXID 0x20
#define MX107_TXID 0x21

typedef byte int8;

typedef enum
{
  Qualcomm=1,
  Other
} fast_synth_t;

typedef struct str_eedata
{
  unsigned int m_eeversion;
  unsigned int m_eedatsize;
  unsigned int m_lmx_freq_khz;
  unsigned int m_vcxo_freq_khz;
  fast_synth_t m_fast_synth_type;
  char m_model[16];
  unsigned int m_csum; // must be at end
} __attribute__((packed)) eedata_t;

unsigned long int g_vcxo_freq;

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
    byte csum=0;
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

byte *find_i2caddrs()
{
  byte i=0;
  byte i2caddr=1;
  static byte i2caddrs[10];

  memset(i2caddrs, 0xFF, sizeof(i2caddrs));

  while (i2caddr) // wrap at 256 == 0
  {
    Wire.beginTransmission(i2caddr);
    // may hang
    Wire.requestFrom(i2caddr,1); // access the first address from the memory

    if (Wire.available())
    {
        Serial.println(i2caddr, HEX);
        i2caddrs[i++] = i2caddr;
    }
    i2caddr++;
    delay(10);
  }

  if (i)
    return i2caddrs;
  else
    return NULL;
}

void acquit_alarm()
{
  Wire.beginTransmission(MX105_TXID);

  Wire.write((byte)0xF8);                          // Ac_Al = 1, LE = 0, clock = 0
  Wire.write((byte)0x68);                          // Ac_Al = 0, LE = 0, clock = 0
  Wire.write((byte)0xF8);                          // Ac_Al = 1, LE = 0, clock = 0
  Wire.endTransmission();
}

byte etat_synthe()
{
  unsigned char a=0xFF;
  Wire.requestFrom(MX105_TXID,1);
  if (Wire.available()) {
    a = Wire.read();
  
    a = ~a & 0x48;    // masque Al_OL : 0x08  déverrouillage PLL
                      // masque Al_SP : 0x40  VCXO hors limites
  
  
    Serial.println(a,HEX);        // synthétiseur OK  --> a = 0x00
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

void sequence_A_lente(long int N)
{
//      Contenu d'initialisation du registre F du circuit MX310 - LMX2326 :
//byte F_data[21]  = {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,1,1};         // function latch
  byte F_data[21]  = {0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,1};

//      Contenu d'initialisation du registre R du circuit MX310 - LMX2326 :
//      Reference is 13MHz.
//      R = 832;  F_REF = 15.625 kHz;  pas à 24 GHz = 250 kHz :
byte R_data[21]  = {0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0};         // R latch

// FREQUENCE DE SORTIE A = 6400,0 MHz :
//byte N_data[21]         =     {0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1};    // N = 102400


// 1500MHz N=96000
byte N_data[21]         =       {0,0,1,0,1,1,1,0,1,1,1, 0,0,0,0,0,0,0,0, 0,1};
byte N_data_calc[21];
//  long int N=96256; // 1504MHz
  //long int N;
  //N=96000; // 1500MHz
  //N=86000; // 1300MHz
  int i;

  memset(N_data_calc, 0, sizeof(N_data_calc));
  /* N_data_calc[0] is the 'go' bit */
  longint_to_array(N, &N_data_calc[1], 18);
  Serial.print("slow: ");
  Serial.println(N);

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

void calcmx(unsigned long int VCXOF, unsigned long int F, int *mx106_out, int *mx107_out)
{
  unsigned long int DIVR, remainder, frac, M, A;
  unsigned char mx106, mx107;

  DIVR=F/VCXOF;

  remainder = (F - (DIVR * VCXOF));
  frac = (remainder * 64)/VCXOF;

  
  //10*(M+1))+A = DIVR;

  M = (DIVR/10) - 1;
  A = DIVR - (10*(M+1));
  Serial.print("calcmx:");
  Serial.println(DIVR);
  Serial.println(remainder);
  Serial.println(frac);
  Serial.println(M);
  Serial.println(A);


//  M=(mx107&0x3C)>>2;
//  A=((mx107&3)<<2)+( (mx106&0xC0)>>6 );
//  frac=mx106&(~0xC0);
  mx106=frac+((A&0x03)<<6);
  Serial.println(mx106);

  mx107=(A>>2) + (M<<2);
  Serial.println(mx107);

  (*mx106_out)=mx106;
  (*mx107_out)=mx107;
}

void sequence_A_rapide(byte mx106, byte mx107)
{
  // FREQUENCE DE SORTIE A = 6400,0 MHz :

  unsigned long int M,A,DIVR,frac,F;

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
  // F=16*(DIVR+frac/64)
#define VCXOF g_vcxo_freq
//#define VCXOF 12288000
  //F=VCXOF*DIVR+(frac/4); //16 megs
  //F=VCXOF*(DIVR+(frac/64)); // assume here VCXO is running at 16MHz
                      // LMX must be programmed for same target freq.
  F=VCXOF*DIVR + ((frac*VCXOF)/64);
  Serial.println(M);
  Serial.println(A);
  Serial.print("Int divide ratio:");
  Serial.println(DIVR);
  Serial.print("Fractional divide ratio:");
  Serial.print(frac);
  Serial.println("/64");
  Serial.print("Freq ");
  Serial.println(F);

  Wire.beginTransmission(MX106_TXID);             // adresse MX106
  Wire.write((byte)mx106);
  Wire.endTransmission();                         // restart

  Wire.beginTransmission(MX107_TXID);             // adresse MX107
  Wire.write((byte)mx107);
  Wire.endTransmission();
}
#if 0
typedef struct str_eedata
{
  unsigned int m_eeversion;
  unsigned int m_eedatsize;
  unsigned int m_csum;
  unsigned int m_lmx_freq_khz;
  unsigned int m_vcxo_freq_khz;
  char m_model[16];
  unsigned int m_csum;
} __attribute__((packed)) eedata_t;
#endif

static eedata_t eedata;
unsigned int eecsum()
{
  unsigned int csum=0;
  const unsigned char *ptr=(const unsigned char *)&eedata;
  for (int i=0;(i<sizeof(eedata)-2);i++)
    csum+=ptr[i];

  csum+=sizeof(eedata);
  return csum;
}

bool check_eeprom_ok()
{
  Serial.println("reading first byte");
  i2c_eeprom_read_buffer(0x50, 0, (byte *) &eedata, sizeof(eedata));
  return ((eecsum()==eedata.m_csum) && (eedata.m_eeversion==1));
}

void report_eeprom()
{
  Serial.print("Model: ");
  Serial.println(eedata.m_model);
  Serial.print("EEversion: ");
  Serial.println(eedata.m_eeversion);
  //Serial.print("Checksum: ");
  //Serial.println(eedata.m_csum);
}

void setup_eeprom()
{
  const unsigned char *eeptr=(const unsigned char *)&eedata;
  
  memset(&eedata, 0, sizeof(eedata));
  eedata.m_eedatsize=sizeof(eedata);
  eedata.m_eeversion=1;
  eedata.m_lmx_freq_khz=13000;
  eedata.m_vcxo_freq_khz=16000;
  eedata.m_fast_synth_type=Qualcomm;
  strcpy(eedata.m_model,"3CC08690ABAA");
  eedata.m_csum=eecsum();

  for (int i=0; i<sizeof(eedata); i++)
    i2c_eeprom_write_byte(0x50, i, eeptr[i]);
}

int read_eeprom()
{
  int retval=0;

  if (check_eeprom_ok())
  {
    report_eeprom();
    retval = 1;
  }
  else
  {
    Serial.println("Checksum invalid");
    //setup_eeprom();
  }

  return retval;
}
void setup() {
  // board runs at 20MHz.

  Wire.begin();
  Serial.begin(115200);
  Serial.println("boot");
#if 0
  while (1)
 {
    read_bank(0x50, NULL);
    delay(2000);
 }
 #endif

  delay(1000);
  if (read_eeprom())
  {
    Serial.println("eeprom read ok");
    g_vcxo_freq = 16000000;
  }
  else
  {
    Serial.println("eeprom not read ok");
    g_vcxo_freq = 12288000;
  }

#if 1
  //i2c_eeprom_write_byte(0x50, 0, 0x88);
  //i2c_eeprom_write_byte(0x50, 10, 0x53);
  //i2c_eeprom_write_byte(0x50, 20, 0x78);
  //i2c_eeprom_read_buffer(0x50, 0, NULL, 20);
  //for (int j=0;j<8;j++)
    //testdat[j]=j+1;
    //i2c_eeprom_write_byte(0x50, j, j+1);
  //i2ec_eeprom_write_page(0x50, 0, testdat, 5);
  //i2c_eeprom_read_buffer(0x50, 0, NULL, 20);
  //read_bank(0x50, NULL);
 // Serial.println();
 // read_bank(0x50, NULL);
#endif

  //read_bank(0x50, NULL);

  Serial.print("VCXO freq is ");
  Serial.println(g_vcxo_freq);
  Serial.println("setup done");
}

void setfreq(unsigned long int F)
{
  int mx106=0, mx107;
  int j;
  acquit_alarm();
  //F=1277952000;
  sequence_A_lente(F/15625); // hardcoded R div; FIXME

  calcmx(g_vcxo_freq, F, &mx106, &mx107);

  delay(1000); // seems to be needed; setfreq succeeds if we call it twice with this in.

  // M3=1 pin 10
  // R=0
  acquit_alarm(); // also seems to be needed
  enable_i2c_fast();

#if 0
  for (mx107=32;(mx107<60); mx107++)
  {
    byte state;
    for (mx106=0;(mx106<256); mx106++)
    {
      Serial.println(mx106);
      Serial.println(mx107);
      sequence_A_rapide(mx106,mx107);
      
      sequence_A_rapide(mx106,mx107);

      delay(1000);
      state=etat_synthe();
      if ((state&0x48) == 0x0)
        break;

    }
    if ((state&0x48) == 0x0)
        break;
  }
#endif
  //sequence_A_rapide(5,0);
  //sequence_A_rapide(224,33); // WORKS
  //sequence_A_rapide(200,32); // bottom of range 1490.445

  // VCO 16.004775 MHz 16.004763
  //sequence_A_rapide(48,34);  // 1580.475MHz
  //sequence_A_rapide(47,34);  // 1580.225MHz
  //sequence_A_rapide(46,34);  // 1579.972MHz
  //sequence_A_rapide(60,34);  // 1583.475MHz
  //sequence_A_rapide(61,34);  // 1583.725MHz
  //sequence_A_rapide(63,34);  // 1584.225MHz  // top of range?
  //sequence_A_rapide(70,34);  // 1583.475MHz
  //sequence_A_rapide(240,32); // 1500.00MHz
  //sequence_A_rapide(240,32); // 1500.00MHz //1 count=25Hz? WORKS
  //sequence_A_rapide(0,33); //1504 MHz
  sequence_A_rapide(mx106,mx107); // new device, try 1300 Mhz
  //sequence_A_rapide(0,37); 

#if 0
  for (j=0; j<1000; j++)
  {
    sequence_A_rapide(240,32);
    //delay(10);
    sequence_A_rapide(241,32);
    //delay(10);
  }
#endif
  
#if 0
  for (j=0;(j<8);j++)
  {
    sequence_A_rapide(0,(1<<j));
    Serial.print(j);
    delay(1000);
  }

  for (j=0;(j<8);j++)
  {
    sequence_A_rapide((1<<j),0);
    Serial.print(j);
    delay(1000);
  }
#endif
  // div=10*(M+1) + A
  
  // M3=32 mx107
  // M2=16 mx107
  // M1=8 mx107
  // M0=4 mx107
  
  // A3==2 mx107
  // A2==1 mx107

  // A1=128 mx106
  // A0==64 mx106
  //sequence_A_rapide(0,32);

  // 250kHz steps?
  disable_i2c_fast();
}

void loop() {
  String freqstr;
  unsigned long int F;

  Serial.println("Freq (MHz)?");
  while (1)
  {
    if (Serial.available())
    {
      freqstr = Serial.readStringUntil('\n');
      break;
    }
  }

  F=freqstr.toInt() * 1000000;

  setfreq(F);
  setfreq(F);

  delay(1000);

  etat_synthe();

#if 0
  long int N;
  for (N=83000;(N<90000);N++)
  {
    byte state;
    Serial.println(N);
    sequence_A_lente(N);
    delay(4000);
    state=etat_synthe();
    if ((state&0x48) == 0x0)
        break;
  }
#endif

#if 0
  byte *i2caddrs, i;

  i2caddrs = find_i2caddrs();

  if (i2caddrs)
  {
    Serial.println("loop: i2caddrs:");
  
    for (i=0; (i2caddrs[i] != 0xFF); i++)
    {
      Serial.println(i2caddrs[i], HEX);
    }
  }
#endif

}
