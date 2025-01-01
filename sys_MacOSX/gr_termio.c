/* Support routines for terminal I/O. This module defines the following
   Fortran-callable routines: GROTRM, GRCTRM, GRWTRM, GRRTRM.

   Modified:
   
   10th Mar 1998. KS/AAO.  Now uses POSIX terminal control scheme, instead
                  of the BSD one using sgtty.h.   
*/

#include <stdio.h>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>

#if defined(SOLARIS2)
#include <string.h>
#include <sys/ioctl.h>
#else
#include <strings.h>
#endif

long int grotrm_(device, l)	/* Open terminal */
char *device;
long int *l;
{
    int fd, n;
    char name[64];

    n = *l;
    if (n > 63)
	n = 63;
    strncpy(name, device, n);
    name[n] = '\0';
    if ((fd = open(name, O_CREAT | O_RDWR, 0644)) == -1)
	{
/*	perror("Cannot access graphics device");
*/
	perror(name);
	return -1;
	}
    else
	{
	return fd;
	}
}

void grctrm_(fd)	/* Close terminal */
long int *fd;
{
    close(*fd);
}

void grwtrm_(fd, buf, n)	/* Write to terminal */
long int *fd;
char *buf;
long int *n;
{
    int nwritten;
    struct termios tty, save_tty;

/*    printf ("writing %d bytes on unit %d\n", *n, *fd);  */

    tcgetattr (*fd,&tty);
    save_tty = tty;
    tty.c_lflag &= ~(ECHO | ICANON);
    tty.c_cc[VMIN] = 1;
    tty.c_cc[VTIME] = 0;
    tcsetattr (*fd,TCSAFLUSH,&tty);

    nwritten = write (*fd, buf, *n);
    tcsetattr (*fd, TCSAFLUSH, &save_tty);
    if (nwritten != *n)
	perror("Error writing to graphics device");
/*    printf ("written\n");  */
    *n = 0;
    return;
}

void grrtrm_(fd, buf, n)	/* Read from terminal */
long int *fd;
char *buf;
long int *n;
{
    int nread;
    struct termios tty, save_tty;

    tcgetattr (*fd,&tty);
    save_tty = tty;
    tty.c_lflag &= ~(ECHO | ICANON);
    tty.c_cc[VMIN] = *n;
    tty.c_cc[VTIME] = 0;
    tcsetattr (*fd,TCSAFLUSH,&tty);

    nread = read (*fd, buf, *n);
    tcsetattr (*fd, TCSAFLUSH, &save_tty);
    if (nread != *n)
	perror("Error reading from graphics device");
    return;
}
