#include "gadef.h"
#include <sys/ioctl.h>
#include <sys/ioccom.h>

#define LBSIZE 4096
#define BYTEORDER 1
#define HEADERSIZE 4
#define MAXPARM 4096
#define True 1
#define False 0
#define NULL 0

unsigned short  loadbuf[LBSIZE], *lbufptr;
int lbufi;

unsigned short	outbuf[HEADERSIZE+MAXPARM], inbuf[HEADERSIZE+MAXPARM], *bufptr;
int fd, actual;

ivasOPEN ()
{
	lbufptr = loadbuf;
	return 0;
}
ivasPURGE ()
{
	if (lbufptr == loadbuf) return 0;
	ivasGPHload(loadbuf,lbufptr-loadbuf);
	lbufptr = loadbuf;
	return 0;
}

ivasINITall ()
{
	long i, j;
	int x;
	initbuffer (0, 0);
	packheadr (0, 12, 0, 1);
	write(fd, outbuf, (bufptr - outbuf) << 1);
/*	for (i=0; i < 100000 ; i++)
		j = j+1; */
	ioctl(fd, UNIXinterruptWait, &i);
	lbufptr = loadbuf;
	return(0);
}

ivasCSshape(shape, size, thick, noblank)
int shape, size, thick, noblank;
{

	initbuffer(8,8);
	packheadr(0, 4, 0, 0);
	packint(shape);
	packint(size);
	packint(thick);
	packint(noblank);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGPHorigin (xOrigin, yOrigin)
int xOrigin;
int yOrigin;
{
	initbuffer (4, 4);
	packheadr (0, 3, 7, 0);
	packint (xOrigin);
	packint (yOrigin);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGPHset (xStart, yStart, xSize, ySize, value)
int xStart;
int yStart;
int xSize;
int ySize;
int value;
{
	initbuffer (10, 10);
	packheadr (0, 3, 8, 0);
	packint (xStart);
	packint (yStart);
	packint (xSize);
	packint (ySize);
	packint (value);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGMdefGraphic (grValue, mode, gphData, mask)
int grValue;
int mode;
int gphData;
int mask;
{
	initbuffer (8, 8);
	packheadr (0, 6, 2, 0);
	packint (grValue);
	packint (mode);
	packint (gphData);
	packint (mask);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGMfreeze (flag)
int flag;
{
	initbuffer (2, 2);
	packheadr (0, 6, 1, 0);
	packint (flag);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGPHvalue (value0, value1)
int value0;
int value1;
{
	if(((lbufptr-loadbuf)+4) > LBSIZE) ivasPURGE();
	*lbufptr++ = 0x0800;
	*lbufptr++ = value0 | value0 << 4 | value0 << 8 | value0 << 12;
	*lbufptr++ = 0x0801;
	*lbufptr++ = value1 | value1 << 4 | value1 << 8 | value1 << 12;
	return 0;
}

ivasGPHmove (x, y, mode)
int x;
int y;
int mode;
{
	if(((lbufptr-loadbuf)+3) > LBSIZE) ivasPURGE();
	*lbufptr++ = 0x8000;
	*lbufptr++ = x;
	*lbufptr++ = y;
	return 0;
}

ivasGMselGraphic (grValue, mode, mask)
int grValue;
int mode;
int mask;
{
	initbuffer (6, 6);
	packheadr (0, 6, 5, 0);
	packint (grValue);
	packint (mode);
	packint (mask);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGFchar (character)
int character;
{
	initbuffer (2, 2);
	packheadr (0, 8, 1, 0);
	packint (character);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGPHdot (mode)
int mode;
{
	if(((lbufptr-loadbuf)+1) > LBSIZE) ivasPURGE();
	*lbufptr++ = 0xcc00;
	return 0;
}

ivasGPHline (x, y, mode)
int x;
int y;
int mode;
{
	if(((lbufptr-loadbuf)+3) > LBSIZE) ivasPURGE();
	*lbufptr++ = 0x8800;
	*lbufptr++ = x;
	*lbufptr++ = y;
	return 0;
}

ivasGPHwindow (xStart, yStart, xSize, ySize, enable)
int xStart;
int yStart;
int xSize;
int ySize;
int enable;
{
	initbuffer (10, 10);
	packheadr (0, 3, 25, 0);
	packint (xStart);
	packint (yStart);
	packint (xSize);
	packint (ySize);
	packint (enable);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGPHarea (xStart, yStart, xSize, ySize)
int xStart;
int yStart;
int xSize;
int ySize;
{
	initbuffer (8, 8);
	packheadr (0, 3, 6, 0);
	packint (xStart);
	packint (yStart);
	packint (xSize);
	packint (ySize);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasGPHfill (x, y, mode)
int x;
int y;
int mode;
{
	if(((lbufptr-loadbuf)+3) > LBSIZE) ivasPURGE();
	*lbufptr++ = 0xc000;
	*lbufptr++ = x;
	*lbufptr++ = y;
	return 0;
}

ivasGPHload (data, count)
short *data;
int count;
{
	int i;

	initbuffer (MAXPARM, 2 + (count*2)/2);
	packheadr (0, 3, 0, 1);
	packint (count);
	packrawarray (data,count);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1) {
		ioctl(fd, UNIXinterruptWait, &i);
		return 0;
	}
	else
		return 1;
}

ivasMOUSEreset ()
{
	initbuffer (0, 0);
	packheadr (0, 5, 5, 0);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasMOUSEput (initX, initY, mode)
int initX;
int initY;
int mode;
{
	initbuffer (6, 6);
	packheadr (0, 5, 3, 0);
	packint (initX);
	packint (initY);
	packint (mode);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasMOUSEcursor (link)
int link;
{
	initbuffer (2, 2);
	packheadr (0, 5, 1, 0);
	packint (link);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasMOUSEcircle (centerX, centerY, radius, mode)
int centerX;
int centerY;
int radius;
int mode;
{
	initbuffer (8, 8);
	packheadr (0, 5, 7, 0);
	packint (centerX);
	packint (centerY);
	packint (radius);
	packint (mode);
	if (write(fd, outbuf, (bufptr - outbuf) << 1) != -1)
		return 0;
	else
		return 1;
}

ivasMOUSEstatus (button, xPos, yPos, mode, waitMask)
int *button;
int *xPos;
int *yPos;
int mode;
int waitMask;
{
	long i, j;

	initbuffer (6, 4);
	packheadr (0, 5, 4, 1);
	packint (mode);
	packint (waitMask);
	write(fd, outbuf, (bufptr - outbuf) << 1);
/*	for (i=0; i < 100000 ; i++)
		j = j+1; */
	i = 8;
	ioctl(fd, UNIXinterruptWait, &i);
	read(fd, inbuf, 12);
	bufptr = inbuf;
	unpackint (button);
	unpackint (xPos);
	unpackint (yPos);
	return(0);
}

initbuffer( size, actual_size)
int size, actual_size;
{

	actual = actual_size;
}

packheadr(class, module, routine, wait)
int class, module, routine, wait;
{

	bufptr = outbuf;
	*bufptr++ = (0xAA80 | BYTEORDER << 1 |(wait != 0));
	*bufptr++ = actual;		/* size passed into init buffer */
	*bufptr++ = module << 8 | routine;
	*bufptr++ = class;
}

packint(val)
unsigned int val;
{

	*bufptr++ = val >> 16;
	*bufptr++ = val & 0xffff;
}

packstring(val, length)
char *val;
int length;
{
	int i;

	for (i = length; i > 0; i -= 2, val += 2) {
		*bufptr++ = (val[0] << 8) | val[1];
	}
}

packrawarray(val, length)
short *val;
int length;
{
	int i;

	for (i = length; i > 0; i -= 1) {
		*bufptr++ = *val++;
	}
}

unpackint(ret)
unsigned int *ret;
{
	if (ret == NULL)
		bufptr += 2;
	else {
		*ret = bufptr[0] << 16 | bufptr[1];
		bufptr += 2;
	}
}
