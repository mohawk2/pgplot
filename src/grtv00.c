#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <strings.h>
#include "imdef.h"

#define BUFSIZE 8192
static int fd;
static unsigned short buffer[BUFSIZE];
static int bufcount;
static struct {
	unsigned short count;
	unsigned short *address;
	} iobuf = {0, buffer};
static int cursor_on;

/*
static perror(s)
char *s;
{
#include <errno.h>
    printf("%s: ERRNO=%d\n", s, errno);
}
*/

static flush()			/* Flush buffer */
{
    int retval;

    if (bufcount > 0)
	{
	iobuf.count = bufcount;
	bufcount = 0;
        retval = ioctl(fd, IM_IOCNWFIFO, &iobuf);
        if (retval != 0) perror("%PGPLOT, Imagraph error (IOCWNFIFO)");
	}
    return;
}

grtv00_(name, l, unit, ier)	/* Open device */
char *name;
long int *l, *unit, *ier;
{
    int i, retval;
    char device[256];
    int j = *l;

    if (j > 255) j = 255;
    strncpy(device, name, j);
    device[j] = '\0';
#ifdef NOISY
    printf ("Device is \"%s\"\n", device);
#endif
    if ((fd = open(device, O_RDWR)) == -1) {
        perror("%PGPLOT, Cannot access Imagraph device");
        *unit = -1;
        *ier = 0;
        return;
    }
    *unit = fd;
    *ier = 1;
    bufcount = 0;
    return;
}

grtv01_()		/* Close device */
{
#ifdef NOISY
    printf ("Close\n");
#endif
    close(fd);
    return;
}

grtv02_()		/* Erase screen */
{
    int retval;
    unsigned short opcode = 0;

#ifdef NOISY
    printf("Erase\n");
#endif
    flush();
    retval = ioctl(fd, IM_IOCCLRSCR, &opcode);
    if (retval != 0) perror("%PGPLOT, Imagraph error (IOCCLRSCR)");
    return;
}

grtv03_(x,y)		/* Move to x,y */
unsigned short *x,*y;
{
    if (bufcount > BUFSIZE - 4)
	flush();
    buffer[bufcount++] = 0x8000;	/* move */
    buffer[bufcount++] = *x;
    buffer[bufcount++] = *y;
    return;
}

grtv04_(x,y)		/* Draw to x,y */
unsigned short *x,*y;
{
    if (bufcount > BUFSIZE - 5)
	flush();
    buffer[bufcount++] = 0x8800;	/* line */
    buffer[bufcount++] = *x;
    buffer[bufcount++] = *y;
    buffer[bufcount++] = 0xCC00;	/* dot */
    return;
}

grtv05_()		/* Dot at current position */
{
    if (bufcount > BUFSIZE - 2)
	flush();
    buffer[bufcount++] = 0xCC00;	/* dot */
    return;
}

grtv06_()		/* End picture */
{
    flush();
    return;
}

grtv07_(x,y)		/* Turn on cursor at x,y */
int *x,*y;
{
    int retval;
    struct {
	unsigned short x;
	unsigned short y;
	} cursor;
    cursor.x = *x;
    cursor.y = *y;
    flush();
    retval = ioctl(fd, IM_IOCCURSMOVE, &cursor);
    if (retval != 0) perror("%PGPLOT, Imagraph error (IOCCURSMOVE)");
    if (!cursor_on)
	{
	retval = ioctl(fd, IM_IOCCURSON);
	if (retval != 0) perror("%PGPLOT, Imagraph error (IOCCURSON)");
	}
    return;
}

grtv08_(x,y)		/* Turn off cursor and return poistion */
int *x,*y;
{
    int retval;
    cursor_on = 0;
    retval = ioctl(fd, IM_IOCCURSOFF);
    if (retval != 0) perror("%PGPLOT, Imagraph error (IOCCURSOFF)");
    return;
}

grtv09_(c)		/* Select color index */
unsigned short *c;
{
    int retval;
    unsigned short mask = (*c) | ((*c)<<8);

#ifdef NOISY
    printf ("Color %d\n", *c);
#endif
    if (bufcount > BUFSIZE - 5)
	flush();
    buffer[bufcount++] = 0x0800;	/* Write parameter reg 0 */
    buffer[bufcount++] = mask;
    buffer[bufcount++] = 0x0801;	/* Write parameter reg 1 */
    buffer[bufcount++] = mask;
    return;
}

grtv10_(c,r,g,b)	/* Set color representation */
unsigned short *c,*r,*g,*b;
{
    int retval;
    struct {
	unsigned short start;
	unsigned short size;
	unsigned short *address;
	} lut;
    unsigned short rgb[3];

#ifdef NOISY
    printf("Setcolor %d %d %d %d\n", *c, *r, *g, *b);
#endif
    flush();
    rgb[0] = *r;
    rgb[1] = *g;
    rgb[2] = *b;
    lut.start = *c;
    lut.size = 1;
    lut.address = &rgb[0];
    retval = ioctl(fd, IM_IOCWLUT, &lut);
    if (retval !=0 ) perror("%PGPLOT, Imagraph error (IOCWLUT)");
    return;
}

grtv11_(x, y)		/* Draw rectangle */
unsigned short *x, *y;
{
    if (bufcount > BUFSIZE - 4)
	flush();
    buffer[bufcount++] = 0xC000;	/* rectangle */
    buffer[bufcount++] = *x;
    buffer[bufcount++] = *y;
    return;
}

grtv12_(mono)		/* Initialize color tables (color indices 0-15) */
unsigned short *mono;
{
    int retval;
    static unsigned short lut[] = {
	  0,  0,  0,	/* black */
	255,255,255,	/* white */
	255,  0,  0,	/* red */
	  0,255,  0,	/* green */
	  0,  0,255,	/* blue */
	  0,255,255,	/* cyan */
	255,  0,255,	/* magenta */
	255,255,  0,	/* yellow */
	255,128,  0,
	128,255,  0,
	  0,255,128,
	  0,128,255,
	128,  0,255,
	255,  0,128,
	 85, 85, 85,	/* dark gray */
	170,170,170	/* light gray */
	};
    short i, r, g, b;
    float grey;

#ifdef NOISY
    printf ("Cold start\n");
#endif
    i = 0;
    retval = ioctl(fd, IM_IOCDOCOLD, &i);
    if (retval != 0) perror("%PGPLOT, Imagraph error (IOCDOCOLD)");
    i = 1;
    retval = ioctl(fd, IM_IOCSETSWAP, &i);
    if (retval != 0) perror("%PGPLOT, Imagraph error (IOCSETSWAP)");
    for (i = 0; i < 16; i++)
	{
	r = lut[3*i];
	g = lut[3*i+1];
	b = lut[3*i+2];
        if (*mono == 1)
	    {
	    grey = 0.30*r + 0.59*g + 0.11*b;
	    r = g = b = grey + 0.5;
	    }
	grtv10_(&i, &r, &g, &b);
        }
    cursor_on = 0;
    return;
}

grtv13_(x,y,n,rbuf)	/* Line of pixels */
int *x,*y,*n;
float rbuf[];
{
    int i, retval;
    unsigned short ix, iy, ic;
    unsigned char row[1024];
    struct {
	unsigned short x;
	unsigned short y;
	} pos;

    flush();
    pos.x = *x;
    pos.y = 1023 - (*y);
    if (pos.x < 0 || pos.x > 1023 || pos.y < 0 || pos.y > 1023)
	perror("%PGPLOT: internal error: bad x,y in grtv13");
    if (pos.x + *n > 1024)
	perror("%PGPLOT: internal error: wrong bytecount in grtv13");
    for (i=0; i < *n; i++)
	{
	row[i] = rbuf[i];
	}
    retval = ioctl(fd, IM_IOCLSEEK, &pos);
    if (retval != 0) perror("%PGPLOT, Imagraph error (IOCLSEEK)");
    retval = write(fd, row, *n);
    if (retval != (*n)) perror("%PGPLOT, Imagraph error (WRITE)");
    return;
}
