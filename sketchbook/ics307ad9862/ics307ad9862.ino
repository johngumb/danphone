// i2c #include <Wire.h>
#include <SPI.h>

// https://www.renesas.com/us/en/products/clocks-timing/clock-generation/clocks-general-purpose/307-03-serially-programmable-clock-source#tools_support
// 307GI03 synth chip.
// 

// 132 bit word generated by VersaClock II. NOTE this is padded with a 4 byte nibble of 0 at MSB
unsigned char  progword[]={0x00, 0x80, 0x3F, 0x80, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x1F, 0xF2};

// output 1 23.2MHz
// output 2 116 MHz
unsigned char progword2[]={0x00, 0x80, 0x3F, 0xC0, 0x46, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1C, 0x1F, 0xF2};

// AD9862
unsigned char ad9862reset[]={0x00, 0x20};
unsigned char ad9862dacW1[]={0x2A, 0xD0}; // 3:0 116MHz adjusted exactly
unsigned char ad9862dacW2[]={0x2B, 0x57}; // 11:4 116MHz adjusted exactly

/*
 * Arduino UNO
 * Pin 13 SCK
 * Pin 12 MISO
 * Pin 11 MOSI
 * Pin 10 Chip Select/Slave Select (SS)
 * Pin 9 AD9862 CSEL
 */
#define AD9862_CSEL 9

void setup() {
  // put your setup code here, to run once:

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

  for (int i=0; (i<sizeof(progword)); i++)
  {
    SPI.transfer(progword2[i]);
  }


  // ics307 needs a 'blip' after programming
  digitalWrite(SS, LOW);
  delay (1);
  digitalWrite(SS, HIGH);

  // AD9862 programming
  digitalWrite(AD9862_CSEL, HIGH);
  SPI.transfer(ad9862reset[0]);
  SPI.transfer(ad9862reset[1]);
  digitalWrite(AD9862_CSEL, LOW);
  
  delay(1);
  
  digitalWrite(AD9862_CSEL, HIGH);
  SPI.transfer(ad9862dacW1[0]);
  SPI.transfer(ad9862dacW1[1]);
  digitalWrite(AD9862_CSEL, LOW);
  
  delay(1);
  
  digitalWrite(AD9862_CSEL, HIGH);
  SPI.transfer(ad9862dacW2[0]);
  SPI.transfer(ad9862dacW2[1]);
  digitalWrite(AD9862_CSEL, LOW);
}

void loop() {
  // put your main code here, to run repeatedly:

}
