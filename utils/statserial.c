/*
 *		statserial - Serial Port Status Utility
 *
 * Copyright (C) 1994 Jeff Tranter (Jeff_Tranter@Mitel.COM)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 ********************************************************************
 *
 * See the man page for a description of what this program does and what
 * the requirements to run it are.
 * 
 * It was developed using:
 * - Linux kernel 1.1.73
 * - gcc 2.5.8
 * - no-name serial card (16450), FIFO card (16550)
 * 
 * Jeff Tranter (Jeff_Tranter@Mitel.COM)
 * Frank Baumgart (godot@uni-paderborn.de)
 */

#include <curses.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <string.h>
#include <sys/ioctl.h>

/* global variables */
char device[255];                          /* name of device to open */
const char *defaultDevice = "/dev/ttyS0";   /* default device if none specified on command line */
int n_option = 0;                          /* set if -n option is used */
int d_option = 0;                          /* set if -d option is used */
int x_option = 0;                          /* set if -x option is used */

/* print command usage and exit */
void usage()
{
  fprintf(stderr, "usage: statserial [-n | -d | -x] [device]\n");
  fprintf(stderr, "  -n option disables looping\n");
  fprintf(stderr, "  -d option prints modem status as a decimal number\n");
  fprintf(stderr, "  -x option prints modem status as a hexadecimal number\n");
  fprintf(stderr, "  Default device is %s\n", device);
  exit(1);
}

/* handle command line options */
void parse_args(int argc, char **argv)
{
  const char     *flags = "ndx";
  int             c;
  
  while ((c = getopt(argc, argv, flags)) != EOF) {
    switch (c) {
    case 'n':
      n_option = 1;
      break;
    case 'd':
      d_option = 1;
      break;
    case 'x':
      x_option = 1;
      break;
    case '?':
      usage();
      break;
    }
  }
    
  if (n_option + d_option + x_option > 1) {
    printf("statserial: options are mutually exclusive\n");
    usage();
  }
    
  /* check for a single additional argument */
  if ((argc - optind) > 1)
    usage(); /* too many arguments */
  
  if ((argc - optind) == 1)
    strcpy(device, argv[optind]); /* one argument */
}

/* called before exiting */
void cleanup()
{
  endwin(); /* required by curses */
}

/* main program */  
int main(int argc, char *argv[])
{
  int fd;                       /* for serial device */
  int status;                   /* status of system calls */
  unsigned int old_status = 0;  /* value of previous call */
  unsigned int arg;             /* value returned by ioctl */

  /* make default the default, use $MODEM if set */
  if (getenv("MODEM"))
    strcpy(device, getenv("MODEM"));
  else
    strcpy(device, defaultDevice);

  /* parse command line arguments */
  parse_args(argc, argv);

  /* open port */
  fd = open(device, O_RDONLY);
  if (fd == -1) {
    char s[255];
    sprintf(s, "statserial: can't open device `%s'", device);
    perror(s);
    exit(1);
  }

  /* init curses */
  if (!d_option && !x_option) {
    initscr();
    atexit(cleanup);
  }

  /* loop forever */
  for (;;) {

    /* get modem status info */  
    status = ioctl(fd, TIOCMGET, &arg);
    if (status != 0) {
      perror("statserial: TIOCMGET failed");
      exit(1);
    }

    /* avoid unneccessary screen updates */
    if (arg == old_status)
    {
      sleep(1);
      continue;
    }
    old_status = arg;

    /* home cursor */
    if (!d_option && !x_option) {
      move(0,0);
    }

    /* print status in decimal */
    if (d_option) {
      printf("%d\n", arg);
      exit(0);
    }

    /* print status in hex */
    if (x_option) {
      printf("%x\n", arg);
      exit(0);
    }

    printw("Device: %s\n\n", device);
    printw("Signal  Pin  Pin  Direction  Status  Full\n");
    printw("Name    (25) (9)  (computer)         Name\n");
    printw("-----   ---  ---  ---------  ------  -----\n");
    printw("FG       1    -      -           -   Frame Ground\n");
    printw("TxD      2    3      out         -   Transmit Data\n");
    printw("RxD      3    2      in          -   Receive  Data\n");
    printw("RTS      4    7      out         %1d   Request To Send\n", !!(arg & TIOCM_RTS));
    printw("CTS      5    8      in          %1d   Clear To Send\n", !!(arg & TIOCM_CTS));
    printw("DSR      6    6      in          %1d   Data Set Ready\n", !!(arg & TIOCM_DSR));
    printw("GND      7    5      -           -   Signal Ground\n");
    printw("DCD      8    1      in          %1d   Data Carrier Detect\n", !!(arg & TIOCM_CAR));
    printw("DTR     20    4      out         %1d   Data Terminal Ready\n", !!(arg & TIOCM_DTR));
    printw("RI      22    9      in          %1d   Ring Indicator\n", !!(arg & TIOCM_RNG));
    refresh();

    /* break out if -n option was used */
    if (n_option)
      exit(0);

    /* delay 1 second between loops */
    sleep(1);
  }
  
  return 0;
}
