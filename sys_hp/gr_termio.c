/* Support routines for terminal I/O. This module defines the following
   Fortran-callable routines: GROTRM, GRCTRM, GRWTRM, GRRTRM. */

#include <stdio.h>
#include <sgtty.h>
#include <strings.h>
#include <fcntl.h>
#define  CBREAK  (0x00000002)

long int grotrm(device, l)	/* Open terminal */
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

grctrm(fd)	/* Close terminal */
long int *fd;
{
    close(*fd);
}

grwtrm(fd, buf, n)	/* Write to terminal */
long int *fd;
char *buf;
long int *n;
{
    int nwritten;
    struct sgttyb tty;
    int save_flags;

/*    printf ("writing %d bytes on unit %d\n", *n, *fd);  */

    ioctl(*fd, TIOCGETP, &tty);
    save_flags = tty.sg_flags;
    tty.sg_flags |= CBREAK;
    ioctl(*fd, TIOCSETP, &tty);
    tty.sg_flags = save_flags;

    nwritten = write (*fd, buf, *n);
    ioctl(*fd, TIOCSETP, &tty);
    if (nwritten != *n)
	perror("Error writing to graphics device");
/*    printf ("written\n");  */
    *n = 0;
    return;
}

grrtrm(fd, buf, n)	/* Read from terminal */
long int *fd;
char *buf;
long int *n;
{
    int nread;
    struct sgttyb tty;
    int save_flags;

    ioctl(*fd, TIOCGETP, &tty);
    save_flags = tty.sg_flags;
    tty.sg_flags |= RAW;
    ioctl(*fd, TIOCSETP, &tty);
    tty.sg_flags = save_flags;

    nread = read (*fd, buf, *n);
    ioctl(*fd, TIOCSETP, &tty);
    if (nread != *n)
	perror("Error reading from graphics device");
    return;
}
