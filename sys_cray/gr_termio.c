/* Support routines for terminal I/O */

#include <stdio.h>
#include <termio.h>
#include <strings.h>

typedef int STRING;
#define addr(string) ((char *)(string & 0xE0000007FFFFFFFF))
#define len(string) ((string & 0x1FFFFFF800000000) >> 35 )

#define MAXBUF 1030       /*  Add a few characters for VT prefix/suffix  */

long int GROTERM(device, l)
STRING device;
long int *l;
{
    int fd, n;
    char name[64];

    n = *l;
    if (n > 63)
	n = 63;
    strncpy(name, addr(device), n);
    name[n] = '\0';
    if ((fd = open(name, 2)) == -1)
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

GRCTERM(fd)
long int *fd;
{
    close(*fd);
}

GRWTERM(fd, buf, n)
long int *fd;
int *buf;
long int *n;
{
    int nwritten;
    struct termio tty;
    char *s,buffer[MAXBUF];
    int save_flags,i;

/* Convert the integer buffer to a buffer of chars. */

    if(*n > MAXBUF)perror("Buffer overflow");
    for(s=buffer,i=0; i < *n; i++) *s++ = *buf++;

/*    printf ("writing %d bytes on unit %d\n", *n, *fd);  */

    ioctl(*fd, TCGETA, &tty);
    save_flags = tty.c_lflag;
    tty.c_lflag &= ~ICANON;
    ioctl(*fd, TCSETA, &tty);
    tty.c_lflag = save_flags;

    nwritten = write (*fd, buffer, *n);
    ioctl(*fd, TCSETA, &tty);
    if (nwritten != *n)
	perror("Error writing to graphics device");
/*    printf ("%d actually written\n",nwritten);  */
/*    getc(stdin);  */
    *n = 0;
    return;
}

GRRTERM(fd, buf, n)
long int *fd;
long int *buf;
long int *n;
{
    char *s,buffer[MAXBUF];
    int nread,i;
    struct termio tty;
    int save_flags;

    if(*n > MAXBUF)perror("Buffer overflow");
    ioctl(*fd, TCGETA, &tty);
    save_flags = tty.c_lflag;
    tty.c_lflag &= ~ICANON;
    ioctl(*fd, TCSETA, &tty);
    tty.c_lflag = save_flags;

    nread = read (*fd, buffer, *n);
    ioctl(*fd, TCSETA, &tty);
    if (nread != *n)
	perror("Error reading from graphics device");

/* Copy the characters to the output buffer. */

    for(s = buffer,i=0; i < nread; i++) *buf++ = *s++;

    return;
}
