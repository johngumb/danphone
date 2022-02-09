#include <assert.h>
#include <string.h>
#include <SPI.h>
#include <avr/wdt.h>
#include <EEPROM.h>
#include <MicroNMEA.h> /* probably a bit much for just getting the time */

const char *g_version="1.0";
//#define AD9862_SIM
#define USE_LEDS

/*
 * NOTE use of external clock at 10MHz requires the following change to boards.txt
 * Set the internal clock to 20MHz using boards.txt instead of the
 * usual standard arduino 16MHz. When the clock is switched to external
 * the board will run at half speed as far as the arduino system is concerned
 * (serial port, delay etc.)

boards.txt is at .arduino15/packages/arduino/hardware/megaavr/1.8.7/boards.txt

--- boards.txt.orig  2021-03-07 14:06:07.102841944 +0000
+++ boards.txt  2021-03-07 14:05:02.158206895 +0000
@@ -53,7 +53,7 @@
 nona4809.upload.extra_params=-P{serial.port}

 nona4809.build.mcu=atmega4809
-nona4809.build.f_cpu=16000000L
+nona4809.build.f_cpu=20000000L
 nona4809.build.board=AVR_NANO_EVERY
 nona4809.build.core=arduino
 nona4809.build.variant=nona4809
@@ -65,7 +65,7 @@
 nona4809.bootloader.file=atmega4809_uart_bl.hex
 nona4809.bootloader.SYSCFG0=0xC9
 nona4809.bootloader.BOOTEND=0x00
-nona4809.bootloader.OSCCFG=0x01
+nona4809.bootloader.OSCCFG=0x02
 nona4809.fuses.file=fuses_4809.bin

 menu.mode=Registers emulation
*/

/*
 * My GPS unit is an old Garmin GPS18 LVC.
 * Produces TTL level pps and TTL level RS232 NMEA data
 *
 * Arduino Nano Every usage:
 * Pin 13 SCK
 * Pin 12 MISO
 * Pin 11 MOSI
 * Pin 10 Blue LED
 * Pin 9 Green LED
 * Pin 8 SS ICS307 Slave Select.
 * Pin 7 AD9862 CSEL (Chip Select)
 * Pin 6 unused, possible TCB waveform output?
 * Pin 5 Red LED
 * Pin 4 GPS 1PPS in
 * Pin 3 unused, possible TCB waveform output?
 * Pin 2 External clock in
 * Pin 0 Received NMEA data via 74LS14 (for inversion) for Serial1.
 * Pin A0 rotary switch bit 2
 * Pin A1 rotary switch bit 1
 * Pin A2 rotary switch bit 0
 */
#define ICS307_SS_PIN SS
#define AD9862_CSEL_PIN 7
#define PPS_PIN 4

class EventListener
{
public:
  EventListener(){};
private:
};

class GPSMonitorEventListener : public EventListener
{
public:
  virtual void gps_up()=0;
  virtual void gps_down()=0;
};

class FrequencyCounterEventListener : public EventListener
{
public:
  virtual void process_delta(const int)=0;
  virtual void frequency_counter_reset()=0;
};

/*
 *  Interface to a round-robin task.
 *  We could rely on duck typing here but nice to make explicit 
 *  what's a task and what's a library.
 */
class RRTask
{
public:
  virtual void run_once()=0;
};

/* EventSources call event listeners. */
/* EventListeners run in the order they are added. */
class EventSource
{
public:
  EventSource(){};
  void add_listener(EventListener *el);

protected:
  EventListener *get_listener(int i){return m_event_listeners[i];}

private:
static const size_t MAX_LISTENERS=10;
  EventListener *m_event_listeners[MAX_LISTENERS];
};

class UptimeTracker
{
public:
  UptimeTracker(){zero();};
  void print() const;
  void zero();
  UptimeTracker operator ++(int)
  {
    tick();
    return *this;
  }
  unsigned int hours() const {return m_uptime_hours;}
  unsigned long int total_seconds() const {return m_uptime_seconds_total;}

private:
  void tick();

  // TODO feasible to keep seconds in 32 bits, time_t or similar
  unsigned int m_uptime_seconds, m_uptime_minutes, m_uptime_hours;
  int m_uptime_days;
  unsigned long int m_uptime_seconds_total;
};

/* My GPS unit is a very old Garmin GPS18 LVC. It has the weeks wrapping bug. */
/* https://gis.stackexchange.com/questions/157249/gpspipe-returns-correct-timestamp-but-wrong-date-21-dec-1995 */
/* It will only emit pps if it can be trusted. Track that behaviour. */
class GPSmonitor : public EventSource, public RRTask /* emits gps up/down notifications */
{
public:
  GPSmonitor() {m_gps_pause=m_gps_pause_max_pps_loss;}
  void pps();
  void tick();
  void run_once();
  bool ok() const {return !m_gps_lost;}
  unsigned char pauseval() const {return m_gps_pause;}    // for monitoring
  unsigned long int pps_losses() const {return m_gps_pps_losses;}
  unsigned long int pps_storms() const {return m_gps_pps_storms;}
  unsigned long int losses() const {return m_gps_pps_losses + m_gps_pps_storms;}
  const UptimeTracker &uptime() const {return m_gps_uptime;} // for monitoring
  const UptimeTracker &total_uptime() const {return m_gps_total_uptime;} // for monitoring
  unsigned long int internal_seconds() const {return m_internal_seconds;}
  unsigned long int external_seconds() const {return m_external_seconds;}

private:
  void reset_counters();
  void detect_gps_loss();
  void detect_gps_ok();
  bool m_gps_lost=false;
  bool m_gps_storm=false;
  unsigned char m_gps_pause;
  const unsigned char m_gps_pause_max_pps_loss=5;
  const unsigned char m_gps_pause_max_pps_storm=10;
  unsigned long int m_internal_seconds=0;
  unsigned long int m_external_seconds=0;
  unsigned long int m_gps_pps_losses=0;
  unsigned long int m_gps_pps_storms=0;
  UptimeTracker m_gps_uptime;
  UptimeTracker m_gps_total_uptime;
};

/* Relies on a TCB set up as in start_freq_measurement. */
/* Keep this object hardware independent for now. */
/* Sinks GPS up/down events. */
/* Emits frequency delta count (to the regulator) when available. */
class FrequencyCounter : public GPSMonitorEventListener, public EventSource, public RRTask
{
public:
  FrequencyCounter(const unsigned long int centreFrequency, const unsigned int sample_period);
  void process_timer_sample(const unsigned int timer_val); // called every gps pps from isr context
  void reset();
  int delta(const bool) const;
  void set_period(const unsigned int period);
  void set_centre_frequency(const unsigned long int);
  unsigned int get_period() const {return m_sample_period;}
  void gps_up(){reset();}
  void gps_down(){reset();}
  void run_once();
  unsigned int resets() const {return m_resets;}           // for monitoring
  unsigned int tcb2_int_cnt() const {return m_tcb2_int_cnt;} // for monitoring

private:
  void calc_remainder_and_reset();
  unsigned int m_tcb2_int_cnt=0;
  unsigned int m_data_start_val;
  unsigned int m_data_end_val;
  unsigned int m_starting_val;
  unsigned int m_sample_period;
  bool m_data_ready=false, m_reset_done=false;
  unsigned int m_tcb2_expected_remainder;
  unsigned long int m_centre_frequency;
  unsigned int m_resets=0;
};

typedef void(*SwitchFunPtr_t)(void);

class RotarySwitch : public RRTask
{
// valid positions are 0..SWITCH_POSITIONS-1
#define SWITCH_POSITIONS 8
const unsigned int max_debounce=500;
public:
  RotarySwitch();
  unsigned char get_switchpos_raw() const;
  void add_action(const unsigned char swpos, const SwitchFunPtr_t f){add_action(swpos,f,NULL);};
  void add_action(const unsigned char swpos, const SwitchFunPtr_t, SwitchFunPtr_t);
  void execute_position(const unsigned char pos); // store software request for run later
  void run_once();

private:
  void execute_action(const unsigned char) const;
  void execute_undo() const;
  bool m_initialised=false;
  unsigned char m_current_switchpos=0;
  SwitchFunPtr_t m_actions[SWITCH_POSITIONS];
  SwitchFunPtr_t m_undo_actions[SWITCH_POSITIONS];
  unsigned int m_debounce;
  bool m_software_request;
  unsigned char m_software_request_value;
};

class LED
{
public:
  /* Below 1000 we assume pwm is required. Maybe a bit hacky.*/
  typedef enum {
    led_begin=1000,
    led_off,
    led_on,
    led_toggle
  } led_state_t;
  LED(const unsigned char pin);
  void on() {set(led_on);}
  void off() {set(led_off);}
  void toggle() {set(led_toggle);}
  void pwm(const unsigned char pwm) {set((led_state_t) pwm);}
  led_state_t get() const {return m_state;}
  void set(led_state_t);
  void describe() const;
  void save() {m_temp_state=m_state;}
  void restore() {set(m_temp_state); m_state=m_temp_state; m_temp_state=led_off;}

private:
  LED::led_state_t m_state, m_temp_state;
  const unsigned char m_pin;
};

/* Wrapper for MicroNMEA */
/* Taken from https://github.com/stevemarple/MicroNMEA/tree/master/examples/demo */
/* My GPS is too old to get the date right. All this library gets right for me is time. */
/* But that's all I need. Consider doing this "by hand" to reduce code size. */

class NMEAlistener : public RRTask
{
public:
  NMEAlistener(){};
  void initialise(HardwareSerial *serialport, const unsigned int baud);
  void run_once();
  /* TODO could add time_t stuff here - not really worth it until we have a textual UI. */
  int hours() const {return m_nmea->getHour();}
  int minutes() const {return m_nmea->getMinute();}
  int seconds() const {return m_nmea->getSecond();}

private:
  HardwareSerial *m_serial_port=NULL;
  char m_nmea_buffer[100];
  MicroNMEA *m_nmea;
};

class Regulator : public FrequencyCounterEventListener
{
public:
  Regulator(){};
  void initialise();
  void process_delta(const int);
  void frequency_counter_reset();
  unsigned int out_of_range_events() const {return m_out_of_range;} // for reporting
  unsigned int stable_count() const {return m_stable_count;}        // for reporting
  unsigned int current_dac_val() const {return m_current_dac_val;}  // for reporting
  bool val_persisted() const {return m_persist_done;}               // for reporting

private:
  void maybe_adjust_frequency(const int);
  void maybe_persist_dacval();
  void attempt_system_recovery() const;
  unsigned int m_out_of_range=0;
  unsigned int m_current_dac_val=0;
  unsigned int m_stable_count=0;
  bool m_persist_done=false;
};

/* TODO maybe this should become a task. */
class StatsReporter : public FrequencyCounterEventListener
{
public:
  StatsReporter(){};
  void report_stats();
  void process_delta(const int);
  void frequency_counter_reset();
  void dac_changed();

private:
  unsigned long int m_stats_period=1;
  unsigned long int m_stats_periods_without_reset=1;
  unsigned long int m_stats_period_dac_last_changed=1;
  int m_last_delta=0;
  unsigned int m_dac_changes=0;
  unsigned int m_max_stable_count=0;
  unsigned int m_max_stable_val=0;
  unsigned long int m_max_stable_count_period=1;
};

/* Create objects */
UptimeTracker g_system_uptime;
UptimeTracker g_dac_time_without_change;

/* Watches for gps pps going away/coming back. */
GPSmonitor g_gps_monitor;

/* Counts external 10MHz system clock which we are attempting to govern. */
/* Listens for up/down events from GPSmonitor. */
/* Use 120s period. */
FrequencyCounter g_frequency_counter(10000000, 120);

/* Listens for data from the frequency counter and regulates the oscillator. */
Regulator g_regulator;

/*
 * Listens for data from the frequency counter and periodically reports status
 * on arrival of data from frequency counter.
 */
StatsReporter g_stats_reporter;

/* An old hand held radio channel switch. Great UI. */
/* Consumes no power, even indicates setting when unit is powered off. */
RotarySwitch g_rotary_switch;

/* GPS NMEA wrapper. */
NMEAlistener g_nmea_listener;

/* LEDs: specify pin number for constructor */
LED red_led(5), green_led(9), blue_led(10);

// text output by default
bool g_reporting=true;

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

// to disable output2: 803F40460200000000000000001C1FF2

// AD9862
#define AD9862_SIGMA_DELTA_DAC_REG_LO 42
#define AD9862_SIGMA_DELTA_DAC_REG_HI 43

unsigned char ad9862reset[]={0x00, 0x20};

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

  red_led.on();
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
  else
    red_led.off();

  return (val == readback_val);
#else
  return true;
#endif
}

void ad9862_write_guaranteed(const unsigned int reg, const unsigned int val)
{
  while (!ad9862_write_verified(reg, val))
    delay(1);
}

unsigned int read_dac()
{
  unsigned int actual_dac_val_lo, actual_dac_val_hi, actual_dac_val;

  actual_dac_val_lo=ad9862_read(AD9862_SIGMA_DELTA_DAC_REG_LO);
  actual_dac_val_hi=ad9862_read(AD9862_SIGMA_DELTA_DAC_REG_HI);

  // and with 0xF0 to skip stuff in low nibble
  actual_dac_val = (actual_dac_val_hi<<8) + (actual_dac_val_lo & 0xF0);

  actual_dac_val>>=4;

  return actual_dac_val;
}

unsigned int g_last_dac_val_written=0;
unsigned int read_dac_cached()
{
  if (g_last_dac_val_written)
    return g_last_dac_val_written;
  else
    return read_dac();
}

void write_dac(const unsigned int dacval)
{
  const unsigned char lownibble=dacval&0x0F;

  ad9862_write_guaranteed(AD9862_SIGMA_DELTA_DAC_REG_LO, lownibble<<4);
  ad9862_write_guaranteed(AD9862_SIGMA_DELTA_DAC_REG_HI, dacval>>4);

  if (dacval != g_last_dac_val_written)
    // accounting
    g_stats_reporter.dac_changed();

  // cache
  g_last_dac_val_written=dacval;
}

#define CHECK_RETURN(_x) if (!_x) return false;

void ics307_write(const unsigned char *progword)
{
  for (size_t i=0; (i<sizeof(progword_versaclock)); i++)
  {
    SPI.transfer(progword[i]);
  }

  /* ics307 needs a 'blip' on it's slave select after programming. */
  /* NOTE that we have a CD4049 level converter between */
  /* ICS307_SS_PIN and the ics307 device hence the */
  /* pulse low here */
  digitalWrite(ICS307_SS_PIN, LOW);
  delay (1);
  digitalWrite(ICS307_SS_PIN, HIGH);
}

unsigned int dac_from_eeprom_checked();
bool board_init()
{
  ics307_write(progword2);

  // The clock has changed to the AD9862 due to the above.
  // Give it chance to sort itself out.
  delay(10);

  // AD9862 programming: reset the device
  ad9862_write(0, 0x20); // can't use write_verified here - reset bit write-only

  // start up in the best state we can
  write_dac(dac_from_eeprom_checked());

  /* Apart from the DAC, we don't use the AD9862. Power it all down. */

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

unsigned int g_internal_interrupts;
void start_internal_pps_interrupt(void)
{
  //https://forum.arduino.cc/index.php?topic=626736.msg4268642#msg4268642

  TCB1.CTRLB = TCB_CNTMODE_INT_gc;

  TCB1.CTRLA = TCB_CLKSEL_CLKDIV1_gc; // run at full rate 10MHz

  // 10,000,000 / 50,000 -> 200Hz.
#define TIMERCOUNT 50000

  TCB1.CCMP = TIMERCOUNT-1; // 200 internal interrupts per second
                                // -1 because counting from 0..49999
                                // is 50,000 counts.
  TCB1.CNT = 0;
  TCB1.INTFLAGS = TCB_CAPT_bm; // clear interrupt request flag
  TCB1.INTCTRL = TCB_CAPT_bm;  // Enable the interrupt

  TCB1.CTRLA |= TCB_ENABLE_bm; // Enable the timer

  g_internal_interrupts=0;
}

/* https://forum.arduino.cc/index.php?topic=694633.0 */
void start_freq_measurement(void)
{
  EVSYS.CHANNEL2 = EVSYS_GENERATOR_PORT0_PIN6_gc; // route pin4 (pps), PC06
  EVSYS.USERTCB2 = EVSYS_CHANNEL_CHANNEL2_gc;

  /*
   * ATmega4808-4809-Data-Sheet-DS40002173A.pdf
   * 21.3.3.1.3 input capture on event mode rather than frequency measurement mode.
   * Just let the counter wrap.
   * This makes the FrequencyCounter delta calculation very simple.
   */
  TCB2.CTRLB = TCB_CNTMODE_CAPT_gc;

  /* may not need use of 'filter' here */
  TCB2.EVCTRL = TCB_EDGE_bm | TCB_CAPTEI_bm | TCB_FILTER_bm;

  TCB2.CTRLA = TCB_CLKSEL_CLKDIV1_gc; // run at full rate 10MHz

  TCB2.INTFLAGS = TCB_CAPT_bm; // clear interrupt request flag
  TCB2.INTCTRL = TCB_CAPT_bm;  // Enable the interrupt

  TCB2.CTRLA |= TCB_ENABLE_bm; // Enable the timer
}

void start_external_pps_interrupt(void)
{
  start_freq_measurement();
}

void connect_objects()
{
  /* GPS up event required in order to reset the sampling process */
  g_gps_monitor.add_listener(&g_frequency_counter);

  /* regulator listens for deltas from the frequency counter */
  g_frequency_counter.add_listener(&g_regulator);

  /* periodically report stats */
  g_frequency_counter.add_listener(&g_stats_reporter);

  /* bind methods to switch positions */
  g_rotary_switch.add_action(0, act_swpos0);
  g_rotary_switch.add_action(1, act_swpos1);
  g_rotary_switch.add_action(2, act_swpos2);
  g_rotary_switch.add_action(3, act_swpos3, act_swpos3_undo);
}

bool switch_to_external_clock();
void setup()
{
  bool clock_ok=false;

  // Clock the MCU at 10MHz by dividing the internal clock by two.
  // It is set up for 20Mhz in boards.txt
  // delay, millis etc. will be out by a factor of two but generally we do not care.
  _PROTECTED_WRITE(CLKCTRL_MCLKCTRLB, (CLKCTRL_PEN_bm | CLKCTRL_PDIV_2X_gc));

  red_led.on();

  // x2 to make up for clock running at half speed (10MHz instead of 20MHz).
  Serial.begin(115200*2);
  Serial.print("307GI03L Test v");
  Serial.println(g_version);
  Serial.println("");

  /* GPS time */
  g_nmea_listener.initialise(&Serial1, 4800*2);

  pinMode(AD9862_CSEL_PIN, OUTPUT);
  pinMode(ICS307_SS_PIN, OUTPUT);

  // default states
  digitalWrite(ICS307_SS_PIN, HIGH);
  digitalWrite(AD9862_CSEL_PIN, LOW);

  // Note on Nano Every LED_BUILTIN is SPI Clock pin (13) so LED is unusable
  // if SPI/I2C is used.
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);
  SPI.setClockDivider(SPI_CLOCK_DIV64);

  /* bring up the oscillator board */
  while(!board_init())
  {
    Serial.println("Waiting before board init retry...");
    delay(100);
  }

  Serial.println("init ok");
  red_led.off();
  green_led.on();
  blue_led.on();

  // Consider use of watchdog to cover losing external clock.
  clock_ok=switch_to_external_clock();

  // use blue led to cover switching to external clock
  if (clock_ok)
    blue_led.off();

  /* stitch the software together */
  connect_objects();

  /* execute the default setting on the rotary switch now */
  g_rotary_switch.run_once();

  /* regulator may reset the board so keeps track of a reasonable dac val */
  g_regulator.initialise();

  /* start things going */
  start_external_pps_interrupt();
  start_internal_pps_interrupt();

  Serial.println("System started");
  Serial.println();
}

#define CLOCKWAITMAX 1000000
bool switch_to_external_clock()
{
  unsigned long int clockwait=0;
  bool extclock_ok=true;

  // Switch to external clock from internal one running at 20MHz with divide by 2 prescale
  _PROTECTED_WRITE(CLKCTRL_MCLKCTRLA, CLKCTRL_CLKSEL_EXTCLK_gc);

  // Wait for it....
  while (!(CLKCTRL.MCLKSTATUS & CLKCTRL_EXTS_bm))
  {
    clockwait++;

    if (clockwait > CLOCKWAITMAX)
    {
      extclock_ok=false;
      break;
    }
  }

  if (extclock_ok)
  {
    // Switch off prescaler stuff - we have a 10MHz external clock now.
    _PROTECTED_WRITE(CLKCTRL_MCLKCTRLB, 0);
    Serial.print("got external 10MHz clock after ");
    Serial.print(clockwait);
    print_units("count", clockwait);
    Serial.println();
  }
  else
    Serial.println("Failed to get external clock");

  return extclock_ok;
}

// 10MHz clock divided down to 200Hz.
#define SECONDBOUNDARY ((10000000/TIMERCOUNT)-1)

/* Verify at compile time what we expect from the above. */
#if SECONDBOUNDARY != 199
#error SECONDBOUNDARY wrong
#endif

ISR(TCB1_INT_vect) /* generate internal "1pps" */
{
  TCB1.INTFLAGS = TCB_CAPT_bm;

  // internal one second timer event
  if (g_internal_interrupts==SECONDBOUNDARY)
  {
    g_internal_interrupts=0;
    g_system_uptime++;
    g_gps_monitor.tick();
    g_dac_time_without_change++;
  }
  else
    g_internal_interrupts++;
}

//#define SIM_GPS_DOWN
#ifdef SIM_GPS_DOWN
bool g_sim_gpsdown=false;
#define SIM_GPS_DOWN_START_BRACKET if (!g_sim_gpsdown) {
#define SIM_GPS_DOWN_END_BRACKET }
#else
#define SIM_GPS_DOWN_START_BRACKET
#define SIM_GPS_DOWN_END_BRACKET
#endif

ISR(TCB2_INT_vect)
{
  /* This is the heart of the frequency measurement system. */
  const unsigned int tcb2_val=TCB2.CCMP; // reading CCMP clears interrupt flag

  SIM_GPS_DOWN_START_BRACKET

  /* Shouldn't make any difference whether we call frequency counter   */
  /* or GPS monitor first as this method (ISR) runs to completion.     */
  /* No real timing constraints here as we have our measurement safely */
  /* in tcb2_val.                                                      */
  g_frequency_counter.process_timer_sample(tcb2_val);

  g_gps_monitor.pps();

  SIM_GPS_DOWN_END_BRACKET
}

#ifdef SIM_GPS_DOWN
void sim_gpsdown()
{
  g_sim_gpsdown=true;
}

void sim_gpsback()
{
  g_sim_gpsdown=false;
}
#endif

bool gps_ok()
{
  return g_gps_monitor.ok();
}

/* until we get some proper alarm state management */
#define red_led_conditional_op(_op, _successcode)\
{\
  switch (red_led.get()) \
  {\
    case LED::led_on: \
    case LED::led_toggle: \
    { \
      Serial.print("Red led operation ");\
      Serial.print(#_op);\
      Serial.print(" not performed due to current state: ");\
      red_led.describe(); \
      Serial.println(); \
    } \
    break; \
\
    default: \
    {\
      red_led._op;\
      _successcode;\
    }\
  }\
}

/* Blue (fast) or green LED (slow) glows more brightly the further off we are. */
/* If corrective action is being taken, red is added with the same brightness. */
unsigned char delta_to_led(const int delta)
{
  const unsigned int absdelta=abs(delta);
  unsigned char result;

  if (absdelta<256)
    result=absdelta*5;
  else
    result=255;

  return result;
}

void Regulator::initialise()
{
  m_current_dac_val=read_dac();
}

void Regulator::process_delta(const int delta)
{
  maybe_adjust_frequency(delta);

  /* save something reasonable for next boot */
  maybe_persist_dacval();
}

void Regulator::frequency_counter_reset()
{
  m_stable_count=0;
}

// TODO not really Regulator (speed) related stuff.
// Maybe need a clock board object.
void Regulator::attempt_system_recovery() const
{
  bool board_init_ok=false;

  Serial.println("Resetting oscillator board...");

  /* try resetting the oscillator board */
  for (int i=0; ((i<100)&&(!board_init_ok)); i++)
    board_init_ok=board_init();

  if (board_init_ok)
  {
    write_dac(m_current_dac_val);

    g_frequency_counter.reset();

    red_led.off();

    Serial.println("Resetting oscillator board...ok.");
    Serial.print("DAC value set to ");
    Serial.println(m_current_dac_val, HEX);
  }
  else
    Serial.println("Resetting oscillator board...failed.");
}

void Regulator::maybe_adjust_frequency(const int delta)
{
  Serial.print("Regulator running at ");
  print_time_now();

// Prevent flapping.
// The following values based on 120s period with frequency counter.
#define MAXCNTDIFF 4
#define PERFECTION 1
#define MAX_SENSIBLE_DIFF 100

  bool written=false;
  const unsigned char ledpwm=delta_to_led(delta);

  /*
   * This may happen if we miss a gps pps during a frequency counter period.
   * Or if the oscillator board is powered down/reset underneath us.
   * Neither should be regular occurrences.
   */
  if (abs(delta) > MAX_SENSIBLE_DIFF)
  {
    m_out_of_range++;

    /* provide some visual clue bad stuff is happening */
    red_led.on();

    Serial.print("Large dac delta diff, red LED set to on: ");
    Serial.println((unsigned int)abs(delta));

    attempt_system_recovery();
  }
  else if (delta > PERFECTION)
  {
    // running fast

    blue_led.pwm(ledpwm);
    Serial.print("Running fast, blue LED ");
    Serial.print((unsigned int)abs(delta));
    green_led.off();

    if (delta > MAXCNTDIFF)
    {
      const unsigned int val=read_dac_cached();

      write_dac(val-1);
      written=true;
    }

    m_stable_count=0;
  }
  else
  {
    if (delta<-PERFECTION)
    {
      // running slow

      Serial.print("Running slow, green LED ");
      green_led.pwm(ledpwm);
      blue_led.off();
      Serial.print((unsigned int)abs(delta));

      if (abs(delta) > MAXCNTDIFF)
      {
        const unsigned int val=read_dac_cached();
        write_dac(val+1);
        written=true;
      }

      m_stable_count=0;
    }
    else
    {
      Serial.print(delta);
      Serial.print(" perfect!");
      green_led.pwm(30);
      blue_led.pwm(13);
      m_stable_count++;
    }
  }

  if (written)
  {
    /* Red led should be off or PWM'd - check before set */
    red_led_conditional_op(pwm(ledpwm),
      {Serial.print(", correcting, red LED "); \
       Serial.println((unsigned int) abs(delta));});

    Serial.print("New DAC value: ");
    Serial.println(read_dac_cached(),HEX);
  }
  else
  {
    /* Red led should be off or PWM'd - check before set */
    red_led_conditional_op(off(), ;);
    Serial.println();
  }

  // keep in case we need to reset the board
  m_current_dac_val=read_dac_cached();
}

/*
 * Occasionally provide a good value for next boot.
 */
void Regulator::maybe_persist_dacval()
{
  const unsigned int persisted_dacval(dac_from_eeprom());
  const unsigned int difference(abs(m_current_dac_val - persisted_dacval));
  const bool significant_dac_difference(difference > 1);
  const bool up_long_enough(g_system_uptime.hours()>5);
  const bool stable(m_stable_count>7);

  /* gps should be ok here - not really worth checking for */
  if (up_long_enough && stable && (!m_persist_done))
  {
    if (significant_dac_difference)
    {
      Serial.print("Persisting dac value ");
      Serial.print(m_current_dac_val, HEX);
      Serial.print(" at ");
      g_system_uptime.print();
      Serial.println();
      dac_to_eeprom(m_current_dac_val);
    }
    else
    {
      Serial.print("Would persist dac value ");
      Serial.print(m_current_dac_val, HEX);
      Serial.print(" at ");
      g_system_uptime.print();
      Serial.println();
    }
    Serial.print("Difference: ");
    Serial.println(difference);

    /* we will only do this once during the lifetime of the boot */
    m_persist_done=true;
  }
}

/* round robin our various tasks */
void do_work()
{
  /* Might as well have the system time right before we do anything. */
  g_nmea_listener.run_once();

  /* Bring gps pps status up to date. */
  g_gps_monitor.run_once();

  /* Process any data ready with the frequency counter. */
  /* The regulator will take any action required to adjust the oscillator. */
  g_frequency_counter.run_once();

  g_rotary_switch.run_once();
}

void print_with_leading_zero(const int val)
{
  if (val<=9)
    Serial.print("0");
  Serial.print((int) val);
}

void print_time_now()
{
  Serial.print(int(g_nmea_listener.hours()));
  Serial.print(':');
  print_with_leading_zero(g_nmea_listener.minutes());
  Serial.print(':');
  print_with_leading_zero(g_nmea_listener.seconds());
  Serial.println(" UTC");
}

/*
 * Trigger from frequency counter indicating
 * that new frequency data is available. Just
 * use it to provide an operational update on
 * the console.
 */
void StatsReporter::process_delta(const int delta)
{
  m_last_delta=delta;
  report_stats();

  m_stats_period++;
  m_stats_periods_without_reset++;
  Serial.println();
  Serial.print("Period ");
  Serial.print(m_stats_period);
  Serial.print(" started at ");
  print_time_now();
}

void StatsReporter::frequency_counter_reset()
{
  m_stats_periods_without_reset=0;
}

void StatsReporter::dac_changed()
{
  m_stats_periods_without_reset=0;
  m_stats_period_dac_last_changed=m_stats_period;
  m_dac_changes++;
  g_dac_time_without_change.zero();
}

extern int get_temperature(void);
void StatsReporter::report_stats()
{
  const unsigned int stable_count(g_regulator.stable_count());

  if (stable_count>m_max_stable_count)
  {
    m_max_stable_count=stable_count;
    m_max_stable_count_period=m_stats_period;
    m_max_stable_val=g_regulator.current_dac_val();
  }

  if (!g_reporting)
    return;

  Serial.print("Period: ");
  Serial.println(m_stats_period);
  Serial.print("DAC time unchanged: ");
  g_dac_time_without_change.print();
  Serial.print("(period ");
  Serial.print(m_stats_period_dac_last_changed);
  Serial.println(")");
  Serial.print("DAC changes: ");
  Serial.println(m_dac_changes);
  Serial.print("Periods without reset: ");
  Serial.println(m_stats_periods_without_reset);
  Serial.print("Frequency counter resets: ");
  Serial.println(g_frequency_counter.resets());
  Serial.print("TCB2 interrupts: ");
  Serial.println(g_frequency_counter.tcb2_int_cnt());
  Serial.print("GPS Monitor seconds internal/external:");
  Serial.print(g_gps_monitor.internal_seconds());
  Serial.print("/");
  Serial.println(g_gps_monitor.external_seconds());
  Serial.print("GPS uptime: ");
  g_gps_monitor.uptime().print();
  Serial.println();
  Serial.print("GPS total uptime: ");
  g_gps_monitor.total_uptime().print();
  Serial.print("(");
  Serial.print(((g_gps_monitor.total_uptime().total_seconds()*100.0)/g_system_uptime.total_seconds()));
  Serial.println("% available)");
  Serial.print("System uptime: ");
  g_system_uptime.print();
  Serial.println();
  if (gps_ok())
    Serial.println("GPS status: up");
  else
    Serial.println("GPS status: down");
  Serial.print("GPS wait: ");
  Serial.println(g_gps_monitor.pauseval());
  Serial.print("GPS losses: ");
  Serial.print(g_gps_monitor.losses());
  if (g_gps_monitor.losses())
  {
    Serial.print(" (");
    Serial.print(g_gps_monitor.pps_losses());
    Serial.print(" pps losses, ");
    Serial.print(g_gps_monitor.pps_storms());
    Serial.print(" pps storms)");
  }
  Serial.println();
  Serial.print("Out of range events: ");
  Serial.println(g_regulator.out_of_range_events());
  Serial.print("Current DAC value: ");
  Serial.print(read_dac_cached(),HEX);
  Serial.print(" stable count ");
  Serial.print(stable_count);
  Serial.print(" max stable count ");
  Serial.print(m_max_stable_count);
  Serial.print(" period ");
  Serial.print(m_max_stable_count_period);
  Serial.print(" val ");
  Serial.print(m_max_stable_val, HEX);
  if (g_regulator.val_persisted())
    Serial.println(" persisted");
  else
    Serial.println();
  Serial.print("Temperature: ");
  Serial.print(get_temperature());
  Serial.println("C");
  g_frequency_counter.delta(true);
  Serial.print("Red LED ");
  red_led.describe();
  Serial.print(" Green LED ");
  green_led.describe();
  Serial.print(" Blue LED ");
  blue_led.describe();

  Serial.println();
}

unsigned char is_eol(const char *c)
{
    return ((*c==' ') || (*c=='\r') || (*c=='\n') || (*c==';'));
}

#define MAX_LINE_LEN 81
static char g_str[MAX_LINE_LEN];

char getchar_nano(void)
{
  while (Serial.available() < 1)
    do_work();

  return Serial.read();
}

void getstr()
{
  unsigned char ptr=0;

  while (1)
  {
    const char c = getchar_nano();

    if (!is_eol(&c))
        g_str[ptr++]=c;
    else
        break;
  }

  g_str[ptr]=0;
}

unsigned char hexdigittobyte(const char p_ch)
{
	unsigned char ch, val=0;

  ch=toupper(p_ch);
	if ( (ch>='0') && (ch<='9') )
		val=ch-'0';
	else if ( (ch>='A') && (ch<='F') )
		val=ch-'A'+10;
	else
    {
		  Serial.print("HEX ERROR: ");
		  Serial.println(p_ch);
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

unsigned char strtohexbyte(const char *chstr)
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
    progword_array[offset++]=strtohexbyte(&g_str[stroffset]);
		stroffset+=2;
	}

  for (size_t i=0; (i<sizeof(progword_array)); i++)
    Serial.print(progword_array[i], HEX);

  Serial.println();

  ics307_write(progword_array);
}

unsigned int dac_from_eeprom()
{
  unsigned int persisted_dacval;

  EEPROM.get(0, persisted_dacval);

  persisted_dacval&=0xFFF;

  return persisted_dacval;
}

unsigned int dac_from_eeprom_checked()
{
  const unsigned int eeval(dac_from_eeprom());
  unsigned int result=0;

  if ((eeval<0x500) || (eeval>0x5A0))
    result=0x55A; // half sensible default
  else
    result=eeval;

  return result;
}

void dac_to_eeprom(const unsigned int dacval)
{
  EEPROM.put(0,dacval);

  const unsigned int readback(dac_from_eeprom());

  if (dacval != readback) // unlikely
  {
    Serial.print("eeprom write failed, wrote ");
    Serial.print(dacval);
    Serial.print(" got back ");
    Serial.println(readback);
    red_led.on();
  }
}

/* use 'EW' command to write current DAC value to EEprom */
void act_eeprom()
{
  if (strlen(g_str)==1) // command is 'E'
  {
    Serial.print("E");

    Serial.println(dac_from_eeprom(), HEX);
  }

  if (toupper(g_str[1])=='W')
  {
    unsigned int dacval=read_dac_cached();
    dac_to_eeprom(dacval);

    // report current value using a recursive call
    g_str[1]=0;
    act_eeprom();
  }
}

// eg. D577
void act_dac()
{
  if (strlen(g_str)==1) // command is 'D'
  {
    Serial.print("D");

    Serial.println(read_dac(), HEX);
  }
  else
  {
    if (strlen(g_str)!=4)
    {
      Serial.println("ERROR: DAC command must be of the form DNNN");
    }
    else
    {
      unsigned char hi, lownibble;

      hi = strtohexbyte(&g_str[1]);
      lownibble = hexdigittobyte(g_str[3]);
      write_dac((hi<<4)+lownibble);

      // report current value using a recursive call
      g_str[1]=0;
      act_dac();
    }
  }
}

void act_report_stats()
{
  Serial.println();
  Serial.print("System time: ");
  print_time_now();
  g_stats_reporter.report_stats();
  Serial.println();
}

void act_reset_and_report_stats()
{
  g_frequency_counter.reset();

  /* Make sure frequency counter event listeners get notified of the reset. */
  g_frequency_counter.run_once();

  act_report_stats();
}

void reboot() {
  Serial.println("rebooting...");
  Serial.println();
  _PROTECTED_WRITE(WDT.CTRLA,WDT_PERIOD_8CLK_gc); // arm the watchdog
  while (1); // upset the 'dog.
}

void act_clk3(const bool enable)
{
  if (!enable)
    ics307_write(progword2);
  else
    {
      unsigned char *w=(unsigned char *)malloc(sizeof(progword2));
      if (w)
      {
        memcpy(w,progword2,sizeof(progword2));
        w[0]=0x02;
        ics307_write(w);
        free(w);
      }
      else
        Serial.println("malloc fail - disable failed");
    }
}

/* Facility to turn off 116MHz out
   in case we want to investigate if it's causing QRM on 2m. */
void act_set116(bool enable)
{
  if (enable)
  {
    ics307_write(progword2);
    Serial.println("116 MHz enabled");
  }
  else
    {
      unsigned char *w=(unsigned char *)malloc(sizeof(progword2));
      if (w)
      {
        memcpy(w,progword2,sizeof(progword2));
        w[3]=0x40; // only CLK1 enabled now - need that for the DAC
                   // else things will start failing. We could deal with that...
                   // i.e. the AD9862 gets it's clock from the ICS307.
        ics307_write(w);
        free(w);
        Serial.println("116 MHz disabled");
      }
      else
        Serial.println("malloc fail - disable failed");
    }
}

void act_set_reports(const bool val)
{
  g_reporting=val;
}

void act_knob()
{
  if (strlen(g_str)==1) // command is 'K'
  {
    Serial.print("K");

    Serial.println(to_user_switchpos(g_rotary_switch.get_switchpos_raw()));
  }
  else
  {
    if (strlen(g_str)!=2)
    {
      Serial.println("ERROR: knob command must be of the form K1..8");
    }
    else
    {
      const unsigned char k=hexdigittobyte(g_str[1]);

      g_rotary_switch.execute_position(from_user_switchpos(k));
    }
  }
}

#define cmd(_cmpstr,_rtn) if (strcmp(g_str, _cmpstr)==0) {_rtn; break;}

#define partcmd(_cmpchar, _rtn) if (toupper(g_str[0])==_cmpchar) {_rtn; break; }

void loop()
{
  /* Parse commands and action them forever. */
  /* While we are waiting for characters, do any other work necessary. */

  getstr();

  do
  {
#ifdef SIM_GPS_DOWN
    cmd("gpsd",sim_gpsdown());
    cmd("gpsb",sim_gpsback());
#endif
      cmd("w", act_reset_and_report_stats());
      cmd("disable", act_set116(false));
      cmd("enable", act_set116(true));
      cmd("nv", act_set_reports(false));
      cmd("v", act_set_reports(true));
      partcmd('B', act_synth());
      partcmd('D', act_dac());
      partcmd('E', act_eeprom());
      partcmd('I', board_init());
      partcmd('S', act_report_stats());
      partcmd('K', act_knob());
      partcmd('R', reboot());
  } while (0);
}

void enable_10MHz_clock_out()
{
  register8_t mclktrla_val=CLKCTRL_MCLKCTRLA;

  mclktrla_val|=CLKCTRL_CLKOUT_bm;

  _PROTECTED_WRITE(CLKCTRL_MCLKCTRLA, mclktrla_val);
}

void disable_10MHz_clock_out()
{
  register8_t mclktrla_val=CLKCTRL_MCLKCTRLA;

  mclktrla_val&=~CLKCTRL_CLKOUT_bm;

  _PROTECTED_WRITE(CLKCTRL_MCLKCTRLA, mclktrla_val);
}

/* switch position actions */
void act_swpos0()
{
  Serial.println("Disabling 10MHz out");
  disable_10MHz_clock_out();
  act_clk3(false);
}

void act_swpos1()
{
  Serial.println("Enabling 10MHz out");
  enable_10MHz_clock_out();
}

void act_swpos2()
{
  Serial.println("Enabling clk3");
  act_clk3(true);
}

void act_swpos3()
{
  act_set116(false);
}

void act_swpos3_undo()
{
  act_set116(true);
}

/* Object Implementations */

void EventSource::add_listener(EventListener *el)
{
  size_t i=0;
  bool result=false;

  for (i=0; (i<MAX_LISTENERS); i++)
    if (!m_event_listeners[i])
      break;

  if (i<MAX_LISTENERS)
  {
    /* we found a free slot */
    m_event_listeners[i]=el;
    result=true;
  }

  /* must succeed */
  assert(result);
}

void UptimeTracker::tick() // can be called from isr context
{
  m_uptime_seconds++;
  m_uptime_seconds_total++;
  if (m_uptime_seconds==60)
  {
    m_uptime_seconds=0;
    m_uptime_minutes++;
  }

  if (m_uptime_minutes==60)
  {
    m_uptime_minutes=0;
    m_uptime_hours++;
  }

  if (m_uptime_hours==24)
  {
    m_uptime_hours=0;
    m_uptime_days++;
  }
}

void UptimeTracker::zero() // can be called from isr context
{
  m_uptime_seconds=0;
  m_uptime_seconds_total=0;
  m_uptime_minutes=0;
  m_uptime_hours=0;
  m_uptime_days=0;
}

void print_units(const char *unit, const unsigned int val)
{
  Serial.print(" ");
  Serial.print(unit);
  if (val!=1)
    Serial.print("s");
  Serial.print(" ");
}

void UptimeTracker::print() const
{
  if (m_uptime_days)
  {
    Serial.print((int)m_uptime_days);
    print_units("day", m_uptime_days);
  }

  if (m_uptime_hours)
  {
    Serial.print((int)m_uptime_hours);
    print_units("hour", m_uptime_hours);
  }

  if (m_uptime_minutes)
  {
    Serial.print((int)m_uptime_minutes);
    print_units("minute", m_uptime_minutes);
  }

  if ((m_uptime_seconds) || (m_uptime_seconds_total==0))
  {
    Serial.print((int)m_uptime_seconds);
    print_units("second", m_uptime_seconds);
  }
}

void GPSmonitor::tick() // called from isr context
{
  m_internal_seconds++;

  /* Track gps uptime using internal ticks */
  /* and a status check so it agrees with system uptime. */
  if (ok())
  {
    m_gps_uptime++;
    m_gps_total_uptime++;
  }
}

void GPSmonitor::pps() // called from isr context
{
  m_external_seconds++;

  if ((m_gps_lost) && (m_gps_pause))
  {
    m_gps_pause--;
    blue_led.toggle();
  }
}

void GPSmonitor::run_once()
{
  detect_gps_loss();

  detect_gps_ok();
}

void GPSmonitor::detect_gps_loss()
{
  if (!m_gps_lost)
  {
    if (m_internal_seconds > (m_external_seconds+1)) // lack of gps pps
    {
      Serial.print("*** GPS pps lost at ");
      print_time_now();
      m_gps_pps_losses++;
      m_gps_lost=true;
      m_gps_storm=false;
      reset_counters();

      save_leds();

      const unsigned int GPS_LOSS_PWM(100);
      blue_led.pwm(GPS_LOSS_PWM);
      red_led.pwm(GPS_LOSS_PWM);
      green_led.pwm(GPS_LOSS_PWM);

      /* Notify frequency counter, probably */
      int i=0;
      while (EventListener *l=get_listener(i++))
         reinterpret_cast<GPSMonitorEventListener *>(l)->gps_down();
    }
    else if (m_external_seconds > (m_internal_seconds+1)) // gps pps storm
    {
      Serial.print("*** GPS pps storm at ");
      print_time_now();
      m_gps_pps_storms++;
      m_gps_lost=true;
      m_gps_storm=true;

      reset_counters();

      save_leds();

      const unsigned int GPS_LOSS_PWM(200);
      blue_led.pwm(GPS_LOSS_PWM);
      red_led.pwm(GPS_LOSS_PWM);
      green_led.pwm(GPS_LOSS_PWM);

      /* Notify frequency counter, probably */
      int i=0;
      while (EventListener *l=get_listener(i++))
         reinterpret_cast<GPSMonitorEventListener *>(l)->gps_down();
    }
  }
}

void GPSmonitor::detect_gps_ok()
{
  if ((m_gps_pause==0) && (m_gps_lost))
  {
    Serial.print("*** GPS back at ");
    print_time_now();
    m_gps_lost=false;
    m_gps_storm=false;
    restore_leds();

    /* Notify frequency counter, probably */
    int i=0;
    while (EventListener *l=get_listener(i++))
       reinterpret_cast<GPSMonitorEventListener *>(l)->gps_up();

    reset_counters();

    m_gps_uptime.zero();
  }
}

void GPSmonitor::reset_counters()
{
  if (m_gps_storm)
      m_gps_pause = m_gps_pause_max_pps_storm;
  else
      m_gps_pause = m_gps_pause_max_pps_loss;

  m_internal_seconds=0;
  m_external_seconds=0;
  m_gps_uptime.zero();
}

FrequencyCounter::FrequencyCounter(const unsigned long int centre_frequency,
                                   const unsigned int period)
{
  set_centre_frequency(centre_frequency);
  set_period(period);
}

void FrequencyCounter::set_centre_frequency(const unsigned long int centre_frequency)
{
  m_centre_frequency = centre_frequency;

  calc_remainder_and_reset();
}

void FrequencyCounter::set_period(const unsigned int sample_period)
{
  m_sample_period=sample_period;

  calc_remainder_and_reset();
}

void FrequencyCounter::calc_remainder_and_reset()
{
  // example:
  // (120 * 10,000,000 / 65536) == 18310 remainder 35840

  unsigned long int total_period_counts=m_sample_period*m_centre_frequency;
  unsigned int expected_overflows=total_period_counts/65536;
  unsigned long int count_without_remainder=(expected_overflows*65536);
  unsigned long int l_expected_remainder=total_period_counts-count_without_remainder;

  m_tcb2_expected_remainder=l_expected_remainder;

  reset();
}

void FrequencyCounter::process_timer_sample(const unsigned int timerval)  // called from isr context on gps pps
{
  if ((m_tcb2_int_cnt%m_sample_period)==0)
  {
    /* m_tcb2_int_cnt is either 0 or 120 (say) here. 0 means initialisation */
    if (m_tcb2_int_cnt==0)
    {
      m_starting_val=timerval;
      m_data_ready=false;
    }
    else
    {
      m_data_start_val=m_starting_val;
      m_data_end_val=timerval;
      m_starting_val=timerval;
      m_data_ready=true;
    }
    m_tcb2_int_cnt=0;
  }

  m_tcb2_int_cnt++;
}

void FrequencyCounter::reset()
{
  m_tcb2_int_cnt=0;
  m_data_ready=false;
  m_resets++;
  m_reset_done=true;
}

int FrequencyCounter::delta(const bool print_workings) const
{
  const unsigned int diff=m_data_end_val-m_data_start_val;
  const int delta=diff-m_tcb2_expected_remainder;

  if (g_reporting && print_workings)
  {
    Serial.print("tcb2cnt at start, end of period; difference; delta: ");
    Serial.print(m_data_start_val);
    Serial.print(", ");
    Serial.print(m_data_end_val);
    Serial.print(", ");
    Serial.print(diff);
    Serial.print(", ");
    Serial.println(delta);
  }

  return delta;
}

void FrequencyCounter::run_once()
{
  /* If we have frequency data ready, distribute it to our clients. */
  if (m_data_ready)
  {
    /* unlikely that gps is not ok */
    if (gps_ok())
    {
      const int dv=delta(false);

      /* Notify the regulator and the stats reporter, probably */
      int i=0;
      while (EventListener *l=get_listener(i++))
        reinterpret_cast<FrequencyCounterEventListener *>(l)->process_delta(dv);
    }
    m_data_ready=false;
  }

  if (m_reset_done)
  {
     /* Notify the regulator and the stats reporter, probably */
      int i=0;
      while (EventListener *l=get_listener(i++))
        reinterpret_cast<FrequencyCounterEventListener *>(l)->frequency_counter_reset();

      m_reset_done=false;
  }
}
/* humans work from 1..8, computers work from 0..7 */
/* work with int at UI level so Serial.print interprets data correctly */
unsigned int to_user_switchpos(const unsigned char switchpos)
{
  return (switchpos+1)&0xFF;
}

unsigned char from_user_switchpos(const unsigned int user_switchpos)
{
  return (user_switchpos&0xFF)-1;
}

RotarySwitch::RotarySwitch() :
m_initialised(false),
m_debounce(0),
m_software_request(false),
m_software_request_value(0)
{
  /* No Serial.print allowed here */
  pinMode(A0, INPUT_PULLUP);
  pinMode(A1, INPUT_PULLUP);
  pinMode(A2, INPUT_PULLUP);

  m_current_switchpos=get_switchpos_raw(); /* no debounce, hopefully it's not moving */

  memset(m_actions, 0, sizeof(m_actions));
  memset(m_undo_actions, 0, sizeof(m_undo_actions));
}

void RotarySwitch::add_action(const unsigned char swpos,
                              const SwitchFunPtr_t switchfun,
                              const SwitchFunPtr_t switchfun_undo)
{
  m_actions[swpos]=switchfun;
  m_undo_actions[swpos]=switchfun_undo;
}

unsigned char RotarySwitch::get_switchpos_raw() const
{
  unsigned char val;

  val=7-( (digitalRead(A0)<<2) + (digitalRead(A1)<<1) + digitalRead(A2) );

  return val;
}

/* allow 'turning' the switch through software */
void RotarySwitch::execute_position(const unsigned char pos)
{
  m_software_request_value=pos;
  m_software_request=true;
}

void RotarySwitch::execute_action(const unsigned char switchpos) const
{
  /* use int here so Serial.print displays correctly */
  const unsigned int user_switchpos=to_user_switchpos(switchpos);

  if (switchpos>=SWITCH_POSITIONS)
  {
    Serial.print("Invalid switch position: ");
    Serial.println(user_switchpos);
  }
  else
  {
    Serial.print("Switch position: ");
    Serial.println(user_switchpos);
    if (m_actions[switchpos])
      m_actions[switchpos]();
    else
      Serial.println("No action defined");
  }
}

void RotarySwitch::execute_undo() const
{
  if (m_undo_actions[m_current_switchpos])
    m_undo_actions[m_current_switchpos]();
}

void RotarySwitch::run_once()
{
  const unsigned char val=get_switchpos_raw();

  if (!m_initialised)
  {
    execute_action(m_current_switchpos);

    m_initialised=true;
    return;
  }

  if (m_software_request)
  {
    execute_action(m_software_request_value);
    m_software_request=false;
    return;
  }

  /* switch is turning */
  if (val != m_current_switchpos)
  {
    if (m_debounce==0)
    {
      red_led.save();

      /* some user feedback - provide a red led 'blip' during debounce */
      red_led.on();
    }
    m_debounce++;
  }

  /* should be stable by now */
  if (m_debounce==max_debounce)
  {
    m_debounce=0;
    red_led.restore();

    /* perform any action request on leaving current position */
    execute_undo();

    /* perform the action for the new switch position */
    execute_action(val);

    /* switch is at new position */
    m_current_switchpos=val;
  }
}

void save_leds()
{
  blue_led.save();
  red_led.save();
  green_led.save();
}

void restore_leds()
{
  blue_led.restore();
  red_led.restore();
  green_led.restore();
}

LED::LED(const unsigned char pin) :
m_temp_state(led_off),
m_pin(pin)
{
  pinMode(pin, OUTPUT);
  set(led_off);
}

void LED::set(led_state_t state)
{
#ifdef USE_LEDS
  if (state>led_begin)
  {
    switch(state)
    {
      case led_off: digitalWrite(m_pin, LOW);
      break;

      case led_on: digitalWrite(m_pin, HIGH);
      break;

      case led_toggle: digitalWrite(m_pin, CHANGE);
      break;

      default:
      break;
    }
  }
  else
  {
    analogWrite(m_pin, state);
  }
#else
  /* avoid compiler warning */
  delay(state*0);
#endif

  m_state=state;
}

void LED::describe() const
{
  if (m_state>led_begin)
  {
    switch(m_state)
    {
      case led_off: Serial.print("off");
      break;

      case led_on: Serial.print("on");
      break;

      case led_toggle: Serial.print("toggle");
      break;

      default:
      {
        Serial.print("unexpected ");
        Serial.print(m_state);
      }
      break;
    }
  }
  else
  {
    Serial.print("PWM ");
    Serial.print(m_state);
  }
}

void NMEAlistener::initialise(HardwareSerial *port, const unsigned int baud)
{
  m_serial_port=port;
  m_serial_port->begin(baud);

  /* toss anything there - probably unnecessary */
  while (m_serial_port->available())
    m_serial_port->read();

  memset(m_nmea_buffer, 0, sizeof(m_nmea_buffer));
  m_nmea=new MicroNMEA(m_nmea_buffer, sizeof(m_nmea_buffer));
  assert(m_nmea);
  m_nmea->clear();
}

void NMEAlistener::run_once()
{
  while (m_serial_port->available()) {
    m_nmea->process(m_serial_port->read());
  }
}

/* https://tomalmy.com/reading-the-chip-temperature/ */
int get_temperature(void) {
  VREF.CTRLA = VREF_ADC0REFSEL_1V1_gc; // 1.1 volt reference
  ADC0.CTRLC = ADC_SAMPCAP_bm + 3; // VREF reference, correct clock divisor
  ADC0.MUXPOS = ADC_MUXPOS_TEMPSENSE_gc; // Select temperature sensor
  ADC0.CTRLD = 2 * (1 << ADC_INITDLY_gp); // Initial delay of 32us
  ADC0.SAMPCTRL = 31; // Maximum length sample time (32us is desired)
  ADC0.COMMAND = 1; // Start the conversion
  while ((ADC0.INTFLAGS & ADC_RESRDY_bm) == 0) ; // wait for completion
  // The following code is based on the ATmega4809 data sheet
  const int8_t sigrow_offset = SIGROW.TEMPSENSE1; // Read signed value from signature row
  const uint8_t sigrow_gain = SIGROW.TEMPSENSE0; // Read unsigned value from signature row
  const uint16_t adc_reading = ADC0.RES; // ADC conversion result with 1.1 V internal reference
  uint32_t temp = adc_reading - sigrow_offset;
  temp *= sigrow_gain; // Result might overflow 16 bit variable (10bit+8bit)
  temp += 0x80; // Add 1/2 to get correct rounding on division below
  temp >>= 8; // Divide result to get Kelvin
  uint16_t temperature_in_K = temp;
  return temperature_in_K - 273; // Return Celsius temperature
}
