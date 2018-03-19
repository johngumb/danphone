#include <SPI.h>

const int ledpin=13;
const int datapin=51;
const int clockpin=52;
const int latchpin = 53;
const int buttonPin = 2;     // the number of the pushbutton pin

void setup()
{
    //SPI.setClockDivider(SPI_CLOCK_DIV128);
    //SPI.setBitOrder(MSBFIRST);
    pinMode(13, OUTPUT);

    pinMode(datapin,OUTPUT);
    pinMode(clockpin,OUTPUT);
    pinMode(latchpin,OUTPUT);
    
    digitalWrite(13, LOW);
    digitalWrite(latchpin, LOW);
    
    
    //SPI.begin();
    Serial.begin(115200);
    Serial.println("begin");
}

void latch()
{
    delay(1);
    digitalWrite(latchpin,LOW);
    delay(1);
    digitalWrite(latchpin,HIGH);
    delay(1);
    digitalWrite(latchpin,LOW);
    delay(1);
}

void set_data(int val)
{
    if (val)
    {
      digitalWrite(datapin,HIGH);
      digitalWrite(ledpin,HIGH);
      //Serial.println('1');
    }
    else
    {
      digitalWrite(datapin,LOW);
      digitalWrite(ledpin,LOW);
      //Serial.println('0');
    }
}

void clockpulse(void)
{
  delay(1);
  digitalWrite(clockpin,LOW);
  digitalWrite(clockpin,HIGH);
  delay(1);
  digitalWrite(clockpin,LOW);
  delay(1);
}

void output_msb_first(unsigned int val, unsigned int nbits)
{
  unsigned int mask;
#ifdef DEBUG  
    Serial.println("output_msb_first");
    Serial.println(val);
    Serial.println(nbits);
#endif

    mask=1<<(nbits-1);
    
#ifdef DEBUG
    Serial.println(mask);
#endif

  while(mask) {
      set_data(val & mask);
      mask = mask >> 1;
      clockpulse();
  }      
#if 0
        mask = 1 << (nbits - 1)

        while mask != 0:

            self.m_hwif.setboolbit(self.m_data, val & mask)

            if self.m_debug:
                if val & mask:
                    print 1
                else:
                    print 0

            mask = mask >> 1

            self.clockpulse()

        return
#endif
}

void loop()
{
  int buttonState = 0;
  int refval,data;
  
  Serial.println("jagtest"); 
  
  digitalWrite(latchpin, LOW);
  
  delay(10);
  
  refval=0x901;
  output_msb_first(refval, 15);
  
  latch();

  // 2 bits of 0
  output_msb_first(0,2);
  
  //data=0x8c48;
  data=0x6600;
  output_msb_first(data, 16);
  latch();

  digitalWrite(ledpin, HIGH);
  while(1);
}

