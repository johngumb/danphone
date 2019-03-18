#include <fcntl.h>
#include <sys/ioctl.h> //ioctl() call defenitions

main()
{
   int fd;
   fd = open("/dev/ttyS0",O_RDWR | O_NOCTTY );//Open Serial Port
  
   int RTS_flag;
   RTS_flag = TIOCM_RTS;
   ioctl(fd,TIOCMBIS,&RTS_flag);//Set RTS pin
   getchar();
   ioctl(fd,TIOCMBIC,&RTS_flag);//Clear RTS pin
   close(fd);
}
