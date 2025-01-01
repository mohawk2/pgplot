/* Support routines for terminal I/O. This module defines the following
   Fortran-callable routines: GROTER, GRCTER, GRWTER, GRRTER. */

/* Modified:
    3rd April 1993.  KS/AAO. Changed =* to = * in two places to placate the
                     ULTRIX compiler, which thought it was archaic.
   10th Mar 1998.    KS/AAO.  Now uses POSIX terminal control scheme, instead
                     of the BSD one using sgtty.h.
*/  

#include <termios.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>

long int groter_(cdev, ldev, cdev_len)
char *cdev;
long int *ldev;
int cdev_len;

/* Open a channel to the device specified by 'cdev'.
 *
 * cdev      I    The name of the device to be opened
 * ldev      I    Number of valid characters in cdev
 * cdev_len  I    Used by Fortran compiler to pass character length
 * groter      O  The open channel number (-1 indicates an error)
 */
{
    int fd, n;
    char name[64];

    n = *ldev;
    if (n > 63)
        n = 63;
    strncpy(name, cdev, n);
    name[n] = '\0';
    if ((fd = open(name, 2)) == -1)
        {
/*      perror("Cannot access graphics device");
 */
        perror(name);
        return -1;
        }
    else
        {
        return fd;
        }
}

void grcter_(fd)
int *fd;

/* Close a previously opened channel.
 *
 * fd        I    The channel number to be closed
 */
{
    close(*fd);
}

void grwter_(fd, cbuf, lbuf, cbuf_len)
int *fd;
char *cbuf;
long int *lbuf;
int cbuf_len;

/* Write lbuf bytes from cbuf to the channel fd.  Data is written in
 * CBREAK mode.
 *
 * fd        I    The channel number
 * cbuf      I    Character array of data to be written
 * lbuf      I/O  The number of bytes to write, set to zero on return
 * cbuf_len  I    Used by Fortran compiler to pass character length
 */
{
    int nwritten;
    struct termios tty, save_tty;

/*    printf ("writing %d bytes on unit %d\n", *lbuf, *fd);  */

    tcgetattr (*fd,&tty);
    save_tty = tty;
    tty.c_lflag &= ~(ECHO | ICANON);
    tty.c_cc[VMIN] = 1;
    tty.c_cc[VTIME] = 0;
    tcsetattr (*fd,TCSAFLUSH,&tty);

    nwritten = write (*fd, cbuf, *lbuf);
    tcsetattr (*fd, TCSAFLUSH, &save_tty);
    if (nwritten != *lbuf)
        perror("Error writing to graphics device");
    *lbuf = 0;
    return;
}

void grpter_(fd, cprom, lprom, cbuf, lbuf, cprom_len, cbuf_len)
int  *fd;
char *cprom, *cbuf;
long int *lprom, *lbuf;
int  cprom_len, cbuf_len;

/* Write prompt string on terminal and then read response.  This version
 * will try to read lbuf characters.
 *
 * fd        I    The channel number
 * cprom     I    An optional prompt string
 * lprom     I    Number of valid characters in cprom
 * cbuf        O  Character array of data read
 * lbuf      I/O  The number of bytes to read, on return number read
 * cbuf_len  I    Used by Fortran compiler to pass character length
 */
{
    int i0, nread, ntry;
    struct termios tty, save_tty;

    tcgetattr (*fd,&tty);
    save_tty = tty;
    tty.c_lflag &= ~(ECHO | ICANON);
    tty.c_cc[VMIN] = 1;
    tty.c_cc[VTIME] = 0;
    tcsetattr (*fd,TCSAFLUSH,&tty);

    if( *lprom>0)
      write (*fd, cprom, *lprom);
    i0=0;
    ntry= *lbuf;
    do {
        nread = read (*fd, &cbuf[i0], ntry);
 /*   printf("Nread=%d, Ntry=%d\n",nread,ntry); */
        i0=i0+nread;
        ntry= *lbuf-i0-1;
    } while (nread>0 && ntry>0);

    tcsetattr (*fd, TCSAFLUSH, &save_tty);
    *lbuf=i0;
    return;
}
