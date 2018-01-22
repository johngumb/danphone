/*
  *  Use the I2C bus with EEPROM 24LC64 adapted for PCD8572/McMicro
  *  Sketch:    eeprom.ino
  *
  *  Author: hkhijhe
  *  Date: 01/10/2010
  *
  *
  */

#include <Wire.h>

//byte edata[]={0x00,0x41,0x81,0x10,0x16,0x81,0x12,0x01,0x02,0x0C,0x00,0x4E,0xA8,0x20,0x16,0x3B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x70,0x05,0x19,0x40,0x05,0x6F,0x20,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x08,0x6E,0x09,0x12,0x6F};
byte onehander425[]=  {0x32,0x41,0x81,0x4C,0x16,0x81,0x12,0x01,0x02,0x0C,0x00,0x4E,0xA8,0x20,0x16,0x3B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x70,0x05,0x19,0x40,0x05,0x6F,0x20,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x19,0x38,0x05,0x6F,0x18,0x05,0x11,0x38};
byte onehander475[]=  {0x32,0x41,0x81,0x0C,0x16,0x81,0x12,0x01,0x02,0x0C,0x00,0x4E,0xA8,0x20,0x16,0x3B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x70,0x05,0x19,0x40,0x05,0x6F,0x20,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x19,0x48,0x05,0x6F,0x28,0x05,0x11,0x38};

// Tx 71.875
// Rx 85.375
// no clock shift
// ACK:STD; SE.CA.:300; MU.CA.:448; G.CA.:STD; G.LEVEL:; B.CA:; S.RF:25W; TOT/RE DLY 075:OMIT; A.R. STD: 7; MAB Numbers B137,B075, B300
byte eightch_orig_e1_245RNE0659[]={0xB3,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1F,0xC3,0x05,0x25,0x53,0x25,0x18,0xB0,0x04,0x1C,0x70,0x32,0x18,0xB0,0x04,0x1C,0x70,0x32,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x18,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0x0};
byte eightch_orig_e2_245RNE0659[]={0xCE,0x00,0x93,0xCA,0xA1,0x06,0x02,0x02,0x81,0x82,0x70,0x00,0x06,0x02,0x02,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x70,0x70,0x00,0x06,0x02,0x02,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x06,0x02,0x02,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xFF,0xFF,0xFF,0x3E,0xFF,0x00,0x06,0x59};

// Tx 71.875
// Rx 85.375
// no clock shift
// ACK:STD; SE.CA.:300; MU.CA.:448; G.CA.:STD; G.LEVEL:; B.CA:; S.RF:25W; TOT/RE DLY 075:OMIT; A.R. STD: 7
byte eightch_orig_e1_245RLQ0854[]={0xAE,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1F,0xC3,0x05,0x25,0x53,0x25,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x17,0x2F,0x03,0x1A,0x78,0x36,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0x00};
byte eightch_orig_e2_245RLQ0854[]={0xCE,0x00,0x93,0xCA,0xA1,0x06,0x03,0x01,0x81,0x82,0x70,0x00,0x06,0x03,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x71,0x70,0x00,0x06,0x03,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x06,0x03,0x01,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xFF,0xFF,0xFF,0x3E,0xFF,0x01,0x08,0x54};

// 425/475/400/375/3625/3875/4325/45
byte eightch_one_eeprom_245RMC1243[]={0x16,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x02,0x67,0x03,0xB4,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1F,0xC3,0x05,0x25,0x53,0x25,0x1C,0x02,0x04,0x20,0x61,0x2B,0x18,0xB0,0x04,0x1C,0x70,0x32,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0xE0};

// 8 ch low band M110 rx 71.875 tx 85.375 tx "monitor"
byte eightch_m110_186RSA2205_e1[]={0xF2,0xF3,0xE1,0xB2,0xB2,0xB0,0xB5,0x45,0x5A,0x39,0xF0,0x32,0x00,0x00,0x00,0x40,0xFE,0x59,0x00,0x03,0x35,0x03,0x35,0x13,0x3D,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x4A,0xD9,0x41,0xCE,0x02,0x03,0x07,0x08,0x08,0x70,0x02,0x03,0x07,0x80,0x81,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x18,0xB0,0x04,0x1C,0x70,0x32,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x21,0xD3,0x05,0x27,0x4D,0x23,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x2B,0x85,0x83,0x16,0x8F,0x40,0x31,0x80,0x83,0x19,0x7D,0x38,0x05,0x00,0x01,0xAA,0xC2};
byte eightch_m110_186RSA2205_e2[]={0x13,0x13,0x00,0x09,0x36,0x3C,0x00,0x0A,0x00,0x16,0x81,0x12,0x01,0x03,0x3A,0x01,0x00,0x01,0x3C,0x85,0x1F,0x28,0x05,0xAB,0x08,0x03,0x3A,0x05,0x00,0x01,0x3C,0x85,0x1F,0x28,0x05,0xAB,0x08,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};

byte eightch_m110_186RRE0644_e1[]={0x52,0x52,0x45,0x30,0x36,0x34,0x34,0x45,0x5A,0x39,0xF0,0x32,0x00,0x00,0x00,0x19,0xFE,0x59,0x00,0x03,0x35,0x03,0x35,0x13,0x3D,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x4A,0xD9,0x41,0xCE,0x07,0x02,0x02,0x08,0x08,0x70,0x07,0x02,0x02,0x80,0x81,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x21,0xD3,0x05,0x27,0x4D,0x23,0x18,0xB0,0x04,0x1C,0x70,0x32,0x18,0xB0,0x04,0x1C,0x70,0x32,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x2B,0x85,0x83,0x16,0x8F,0x40,0x31,0x80,0x83,0x19,0x7D,0x38,0x05,0x1E,0x01,0xAA,0xC2};
byte eightch_m110_186RRE0644_e2[]={0x13,0x0D,0x00,0x09,0x36,0x3C,0x00,0x00,0x00,0x16,0x81,0x12,0x01,0x00,0x00,0x01,0x00,0x01,0x3D,0x85,0x1F,0x28,0x05,0xAB,0x08,0x00,0x00,0x05,0x00,0x01,0x3D,0x85,0x1F,0x28,0x05,0xAB,0x08,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};

byte eightch_m110_186RWP0321_e1[]={0x52,0x50,0x57,0x30,0x33,0x32,0x31,0x45,0x5A,0x39,0xF0,0x32,0x00,0x00,0x00,0x0E,0xFE,0x59,0x00,0x03,0x35,0x03,0x35,0x13,0x3D,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x4A,0xD9,0x41,0xCE,0x06,0x01,0x07,0x08,0x08,0x70,0x06,0x01,0x07,0x80,0x81,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x70,0x1F,0xC3,0x05,0x25,0x53,0x25,0x17,0x2F,0x03,0x1A,0x78,0x36,0x21,0xD3,0x05,0x27,0x4D,0x23,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0xAA,0xC2};
byte eightch_m110_186RWP0321_e2[]={0x00,0x40,0x00,0x40,0x36,0x00,0x00,0x07,0x00,0x16,0x81,0x12,0x01,0x00,0x00,0x01,0x00,0x01,0x3C,0x85,0x1F,0x28,0x05,0xAB,0x08,0x00,0x00,0x05,0x00,0x01,0x3C,0x85,0x1F,0x28,0x05,0xAB,0x08,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};

//77MHz PA?
// 2 channel micro
byte twoch_micro_817RPN0058_B_e1[]={0x00,0x58,0x00,0x57,0x16,0x81,0x12,0x01,0x02,0x00,0x00,0x40,0xB0,0x11,0x06,0x3B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x50,0x01,0x60,0x14,0x01,0xCB,0x14,0x01,0x60,0x0A,0x01,0xCB,0x0A,0x01,0x60,0x0A,0x01,0xCB,0x0A,0x01,0x60,0x0A,0x01,0xCB,0x0A,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04,0x05,0xB1,0x34,0x05,0x26,0x04};

//245RLQ0854A 8 Channel micro 425/475/4/375/3625/3875/4375/45 (Vin) with M110 head 186RPW0321
// old style cpu board
byte eightch_4m_245RLQ0854A[]={0x7E,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x02,0x67,0x03,0xB4,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1F,0xC3,0x05,0x25,0x53,0x25,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x17,0x2F,0x03,0x1A,0x78,0x36,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0xE0};

//245RLQ0854B 8 Channel micro 425/475/4/375/3625/3875/4375/45 (Vin) with M110 head 186RPW0321
// old style cpu board
byte eightch_4m_245RLQ0854B[]={0xCE,0x00,0x9B,0x2A,0xA8,0x06,0x03,0x01,0x81,0x82,0x70,0x00,0x06,0x03,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x72,0x70,0x00,0x06,0x03,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x06,0x03,0x01,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0x85,0x19,0x38,0x05,0x6F,0x18,0x85,0x19,0x48,0x05,0x6F,0x28,0x85,0x19,0x30,0x05,0x6F,0x10,0x85,0x19,0x28,0x05,0x6F,0x08,0xC5,0x19,0x24,0x45,0x6F,0x04,0xC5,0x19,0x2C,0x45,0x6F,0x0C,0xC5,0x19,0x3C,0x45,0x6F,0x1C,0x85,0x19,0x40,0x05,0x6F,0x20,0xFF,0xFF,0xFF,0x3E,0xFF,0x01,0x08,0x54};

//245RPS0459 8 Channel micro 425/475/4/375/3625/3875/4375/45 (Peter Matthews)
byte eightch_4m_245RPS0459A[]={0x4A,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x03,0x3A,0x03,0xB4,0x02,0x67,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x17,0x2F,0x03,0x1A,0x78,0x36,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x3E,0x01,0x0D,0x20,0x00,0x13,0x36,0x00,0x00,0x00,0x00,0xE0};
  
//245RPS0459 8 Channel micro 425/475/4/375/3625/3875/4375/45 (Peter Matthews)
byte eightch_4m_245RPS0459B[]={0xCC,0x00,0x9B,0x2A,0xA8,0x03,0x03,0x01,0x81,0x82,0x70,0x00,0x03,0x03,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x77,0x70,0x00,0x03,0x03,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0x85,0x19,0x38,0x05,0x6F,0x18,0x85,0x19,0x48,0x05,0x6F,0x28,0x85,0x19,0x30,0x05,0x6F,0x10,0x85,0x19,0x28,0x05,0x6F,0x08,0x85,0x19,0x20,0x05,0x6E,0x50,0xC5,0x19,0x24,0x45,0x6F,0x04,0xC5,0x19,0x3C,0x45,0x6F,0x1C,0x85,0x19,0x40,0x05,0x6F,0x20,0xFF,0xFF,0xFF,0x3E,0xFF,0x00,0x04,0x59};

//245RLQ00875A 8 Channel micro 425/475/4/375/3625/3875/4375/45 M0MPM replacement 
byte eightch_4m_245RLQ00875A[]={0xEA,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x02,0x67,0x03,0xB4,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x18,0xB0,0x04,0x1C,0x70,0x32,0x18,0xB0,0x04,0x1C,0x70,0x32,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0xE0};

//245RLQ00875B 8 Channel micro 425/475/4/375/3625/3875/4375/45 M0MPM replacement 
byte eightch_4m_245RLQ00875B[]={0xCE,0x00,0x9B,0x2A,0xA8,0x03,0x02,0x02,0x81,0x82,0x70,0x00,0x03,0x02,0x02,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x72,0x70,0x00,0x03,0x02,0x02,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x03,0x02,0x02,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0x85,0x19,0x38,0x05,0x6F,0x18,0x85,0x19,0x48,0x05,0x6F,0x28,0x85,0x19,0x30,0x05,0x6F,0x10,0x85,0x19,0x28,0x05,0x6F,0x08,0xC5,0x19,0x24,0x45,0x6F,0x04,0xC5,0x19,0x2C,0x45,0x6F,0x0C,0xC5,0x19,0x3C,0x45,0x6F,0x1C,0x85,0x19,0x40,0x05,0x6F,0x20,0xFF,0xFF,0xFF,0x3E,0xFF,0x01,0x08,0x75};

//245RLQ00842A 8 Channel micro 425/475/4/375/3625/3875/4375/45 G3XYX Graham Bedwell old style CPU board
byte eightch_4m_245RLQ00842A[]={0x6B,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x02,0x67,0x03,0xB4,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x26,0x5D,0x06,0x2D,0x42,0x1E,0x1F,0xC3,0x05,0x25,0x53,0x25,0x1A,0x4C,0x04,0x1E,0x68,0x2F,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0xE0};

//245RLQ00842B 8 Channel micro 425/475/4/375/3625/3875/4375/45 G3XYX Graham Bedwell old style CPU board
byte eightch_4m_245RLQ00842B[]={0xCE,0x00,0x9B,0x2A,0xA8,0x09,0x06,0x03,0x81,0x82,0x70,0x00,0x09,0x06,0x03,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x72,0x70,0x00,0x09,0x06,0x03,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x09,0x06,0x03,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0x85,0x19,0x38,0x05,0x6F,0x18,0x85,0x19,0x48,0x05,0x6F,0x28,0x85,0x19,0x30,0x05,0x6F,0x10,0x85,0x19,0x28,0x05,0x6F,0x08,0xC5,0x19,0x24,0x45,0x6F,0x04,0xC5,0x19,0x2C,0x45,0x6F,0x0C,0xC5,0x19,0x3C,0x45,0x6F,0x1C,0x85,0x19,0x40,0x05,0x6F,0x20,0xFF,0xFF,0xFF,0x3E,0xFF,0x01,0x08,0x42};

// 245RLQ0883A 8 Channel micro original Tx71.875 Rx85.375
byte eightch_4m_245RLQ0883A[]={0x60,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x26,0x5D,0x06,0x2D,0x42,0x1E,0x1D,0xD3,0x05,0x22,0x5A,0x28,0x17,0x2F,0x03,0x1A,0x78,0x36,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0x00};

// 245RLQ0883B 8 Channel micro original Tx71.875 Rx85.375
byte eightch_4m_245RLQ0883B[]={0xCE,0x00,0x93,0xCA,0xA1,0x09,0x05,0x01,0x81,0x82,0x70,0x00,0x09,0x05,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x70,0x70,0x00,0x09,0x05,0x01,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x09,0x05,0x01,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xFF,0xFF,0xFF,0x3E,0xFF,0x01,0x08,0x83};
 
// 245RLQ0881A 8 Channel micro original Tx71.875 Rx85.375
byte eightch_4m_245RLQ0881A[]={0x51,0x03,0x35,0x13,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x13,0x3C,0x00,0x00,0x3D,0xC8,0x23,0x0E,0x25,0x54,0x27,0xC3,0x2A,0x5A,0x2D,0x18,0x30,0x07,0x33,0x25,0x36,0x7C,0x3A,0x02,0x4A,0xD9,0x41,0xCE,0x00,0x00,0x00,0x00,0x00,0x00,0x57,0x53,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0xB0,0x04,0x1C,0x70,0x32,0x1C,0x02,0x04,0x20,0x61,0x2B,0x1F,0xC3,0x05,0x25,0x53,0x25,0x24,0x08,0x06,0x2A,0x48,0x20,0x24,0x08,0x06,0x2A,0x48,0x20,0x1F,0xC3,0x05,0x25,0x53,0x25,0x2B,0x85,0x83,0x16,0x8F,0x40,0x28,0xDC,0x83,0x15,0x99,0x45,0x05,0x02,0x01,0x0D,0x09,0x00,0x13,0x36,0x00,0x00,0x87,0xC7,0x00};

// 245RLQ0881B 8 Channel micro original Tx71.875 Rx85.375
byte eightch_4m_245RLQ0881B[]={0xCE,0x00,0x93,0xCA,0xA1,0x02,0x04,0x06,0x81,0x82,0x70,0x00,0x02,0x04,0x06,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x71,0x70,0x70,0x71,0x70,0x00,0x02,0x04,0x06,0x08,0x08,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x02,0x04,0x06,0x08,0x09,0x70,0x00,0x01,0x70,0x70,0x70,0x70,0x70,0x00,0x16,0x81,0x12,0x01,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xD9,0x67,0x1E,0x5A,0x15,0x46,0xFF,0xFF,0xFF,0x3E,0xFF,0x01,0x08,0x81};
  
byte *edata=twoch_micro_817RPN0058_B_e1;

void i2c_eeprom_write_byte( int deviceaddress, unsigned int eeaddress, byte data ) {
    int rdata = data;
    Wire.beginTransmission(deviceaddress);
    //Wire.write((int)(eeaddress >> 8)); // MSB
    Wire.write((int)(eeaddress & 0xFF)); // LSB
    Wire.write(rdata);
    Wire.endTransmission();
}

// WARNING: address is a page address, 6-bit end will wrap around
// also, data can be maximum of about 30 bytes, because the Wire library has a buffer of 32 bytes
void i2c_eeprom_write_page( int deviceaddress, unsigned int eeaddresspage, byte* data, byte length ) {
    Wire.beginTransmission(deviceaddress);
    Wire.write((int)(eeaddresspage >> 8)); // MSB
    Wire.write((int)(eeaddresspage & 0xFF)); // LSB
    byte c;
    for ( c = 0; c < length; c++)
        Wire.write(data[c]);
    Wire.endTransmission();
}

byte i2c_eeprom_read_byte( int deviceaddress, unsigned int eeaddress ) {
    byte rdata = 0xFF;
    Wire.beginTransmission(deviceaddress);
    //Wire.write((int)(eeaddress >> 8)); // MSB
    Wire.write((int)(eeaddress & 0xFF)); // LSB
    Wire.endTransmission();
    Wire.requestFrom(deviceaddress,1);
    if (Wire.available()) rdata = Wire.read();
    return rdata;
}

// maybe let's not read more than 30 or 32 bytes at a time!
void i2c_eeprom_read_buffer( int deviceaddress, unsigned int eeaddress, byte *buffer, int length ) {
    Wire.beginTransmission(deviceaddress);
    Wire.write((int)(eeaddress >> 8)); // MSB
    Wire.write((int)(eeaddress & 0xFF)); // LSB
    Wire.endTransmission();
    Wire.requestFrom(deviceaddress,length);
    int c = 0;
    for ( c = 0; c < length; c++ )
        if (Wire.available()) buffer[c] = Wire.read();
}




void setup()
{
    char somedata[] = "this is data from the eeprom"; // data to write
    Wire.begin(); // initialise the connection
    Serial.begin(115200);
    //i2c_eeprom_write_page(0x50, 0, (byte *)somedata, sizeof(somedata)); // write to EEPROM

    delay(100); //add a small delay
    Serial.println("Initialised");

    //Serial.println("Memory written");
    
}

#define EELEN 128

void readeprom()
{
   int addr=0; //first address
    Serial.println("reading");
    //byte i2caddr=0xA0;
    byte i2caddr=0x01;
    byte b=0xFF;
    byte printed=0;

#if 1   
    while (i2caddr)
    {
      b = i2c_eeprom_read_byte(i2caddr, 0); // access the first address from the memory
      if (b != 0xFF)
        break;
      i2caddr+=1;
      delay(10);
    }
#endif
    //i2caddr=0xA3;
    Serial.println("i2caddr");
    Serial.print(i2caddr,HEX);
    Serial.println();
 
    delay(1000);
    while (addr<EELEN)
    {
        Serial.print("0x");
        if (b<16)
          Serial.print(0);
        Serial.print(b, HEX); //print content to serial port

        printed++;
        if ((printed%16)==0)
          Serial.println();
        else
          Serial.print(" ");
          
        addr++; //increase address
        b = i2c_eeprom_read_byte(i2caddr, addr); //access an address from the memory
    }
    addr=0;
    Serial.println(" ");
    delay(10000);
}

void checkeprom()
{
   int addr=0; //first address
    Serial.println("checking");
    //byte i2caddr=0xA0;
    byte i2caddr=0x01;
    byte b=0xFF;
    byte printed=0;

#if 1   
    while (i2caddr)
    {
      b = i2c_eeprom_read_byte(i2caddr, 0); // access the first address from the memory
      if (b != 0xFF)
        break;
      i2caddr+=1;
      delay(10);
    }
#endif
    //i2caddr=0xA3;
    Serial.println("i2caddr");
    Serial.print(i2caddr,HEX);
    Serial.println();
 
    delay(1000);
    while (addr<EELEN)
    {
        if (b!=edata[addr])
        {
          Serial.print("Fail at ");
          Serial.println(addr, HEX);
        }
        addr++; //increase address
        b = i2c_eeprom_read_byte(i2caddr, addr); //access an address from the memory
    }
    addr=0;
    Serial.println(" ");
    delay(10000);
}

void writeeprom()
{
   int addr=0; //first address
    Serial.println("starting write");
    //byte i2caddr=0xA0;
    byte i2caddr=0x51;
    byte b=0xFF;
    byte printed=0;

    while (addr<EELEN)
    {
        i2c_eeprom_write_byte(i2caddr, addr, edata[addr]); 
        delay(100);
        while (i2c_eeprom_read_byte(i2caddr, addr) != edata[addr])
        {
          Serial.print("retry at ");
          Serial.println(addr, HEX);
          i2c_eeprom_write_byte(i2caddr, addr, edata[addr]); 
          delay(100);
        }
        
        addr++; //increase address
    }
    addr=0;
    Serial.println(" ");
    //delay(10000);
}

void loop()
{
//  bool written=false;
  
//  readeprom();
  
//  if (!written)
//  {
//    writeeprom();
//    written=true;
//  }
  
//  checkeprom();
  //checkeprom();
  readeprom();
}
