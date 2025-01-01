/* Support routines for terminal I/O. This module defines the following
   Fortran-callable routines: GROTRM, GRCTRM, GRWTRM, GRRTRM. */

#include <stdio.h>
#include <sys/termio.h>
#include <string.h>
#include <fcntl.h>

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

grctrm_(fd)	/* Close terminal */
long int *fd;
{
    close(*fd);
}

grwtrm_(fd, buf, n)	/* Write to terminal */
long int *fd;
char *buf;
long int *n;
{
    int nwritten;
    struct termio tty;
    short save_flags;

/*    printf ("writing %d bytes on unit %d\n", *n, *fd);  */

    ioctl(*fd, LIOCGETP, &tty);
    save_flags = tty.c_lflag;
    tty.c_lflag |= ICANON;
    ioctl(*fd, LIOCSETP, &tty);
    tty.c_lflag = save_flags;

    nwritten = write (*fd, buf, *n);
    ioctl(*fd, LIOCSETP, &tty);
    if (nwritten != *n)
	perror("Error writing to graphics device");
/*    printf ("written\n");  */
    *n = 0;
    return;
}

grrtrm_(fd, buf, n)	/* Read from terminal */
long int *fd;
char *buf;
long int *n;
{
    int nread;
    struct termio tty;
    short save_flags;

    ioctl(*fd, LIOCGETP, &tty);
    save_flags = tty.c_lflag;
    tty.c_lflag &= 0xef;
    ioctl(*fd, LIOCSETP, &tty);
    tty.c_lflag = save_flags;

    nread = read (*fd, buf, *n);
    ioctl(*fd, LIOCSETP, &tty);
    if (nread != *n)
	perror("Error reading from graphics device");
    return;
}
