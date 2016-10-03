#include <SPI.h>

const int synth_latch = 53;
const int mc_micro_shiftreg_latch = 49;
const int buttonPin = 2;     // the number of the pushbutton pin
const int LED=13;

void setup()
{
    SPI.setClockDivider(SPI_CLOCK_DIV128);
    SPI.setBitOrder(MSBFIRST);
    pinMode(LED, OUTPUT);
    
    pinMode(49, OUTPUT);

    
    digitalWrite(LED, LOW);
    digitalWrite(mc_micro_shiftreg_latch, HIGH);

    // should not be necessary
    digitalWrite(synth_latch, LOW);
    
    SPI.begin();
    Serial.begin(115200);
    Serial.println("begin");
}

void latch(int latch_id)
{
    digitalWrite(latch_id,LOW);
    delayMicroseconds(10);
    digitalWrite(latch_id,HIGH);
    delayMicroseconds(10);
    digitalWrite(latch_id,LOW);
}

void loop()
{
  int buttonState = 0;
  const int SR_AUDIO_PA=0x01;
  const int SR_POWER=0x02;
  const int SR_TX_RX=0x04;
  const int SR_TX_POWER_HI_LO=0x08;
  const int SR_EXT_ALARM=0x10;
  const int SR_TX_PA=0x20;  
  const int SR_TX_AUDIO_ENABLE=0x40;
  const int SR_RX_AUDIO_ENABLE=0x80;
 
  // 10, 20,, 80, 01, 08 may be tx power or tx rx audio enable
  const int SR_TEST=0x01;
  
  Serial.println("jagtest"); 
  
  // should not be necessary
  digitalWrite(synth_latch, LOW);
 
  digitalWrite(mc_micro_shiftreg_latch, LOW);
  
  // power on
  //SPI.transfer(SR_POWER|SR_TX_POWER|SR_AUDIO_ENABLE);
  //SPI.transfer(SR_POWER|SR_TX_RX|SR_TX_PA|SR_TX_POWER_HI_LO);
  //SPI.transfer(SR_POWER|SR_TX_RX|SR_TX_PA);
  
  SPI.transfer(SR_POWER|SR_RX_AUDIO_ENABLE);
  
  latch(mc_micro_shiftreg_latch);
  
  //12.5Khz
    SPI.transfer(9);
    SPI.transfer(1);
    latch(synth_latch);
      Serial.println("jagtest2"); 

    //51M
//    SPI.transfer(0);
//    SPI.transfer(0x66);
//    SPI.transfer(0x00);
//   latch(synth_latch);

    //72.4M
//    SPI.transfer(0);
//    SPI.transfer(0x90);
//    SPI.transfer(0x40);

//71.416
//    SPI.transfer(0);
//    SPI.transfer(0x8e);
//    SPI.transfer(0x42);
//    latch(synth_latch);
    
//GB3BAA 50Mhz
//    SPI.transfer(0);
//    SPI.transfer(0x8e);
//    SPI.transfer(0x42);
//    latch(synth_latch);

//70.3875 Mhz Rx
    SPI.transfer(0);
    SPI.transfer(0xb7);
    SPI.transfer(0x2e);
    latch(synth_latch);
    
    //51M
//    SPI.transfer(0);
//    SPI.transfer(0x64);
//    SPI.transfer(0x20);
//    latch(synth_latch);
 
    Serial.println("jagtest4");
          
     digitalWrite(LED,HIGH);
     delayMicroseconds(10);
     digitalWrite(LED,LOW);
     delayMicroseconds(10);
     digitalWrite(LED,HIGH);
     delayMicroseconds(10);
     //digitalWrite(LED,LOW);
     
     while(1)
     {
     }
}
