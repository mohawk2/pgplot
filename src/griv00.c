#include <fcntl.h>
#include <sys/ioctl.h>
#include "gmdef.h"
#include "gphdef.h"
#include "gadef.h"

extern int fd;

griv00_(unit,ier)
int *unit,*ier;
{
	int i;

	if ((fd = open("/dev/ga0",O_RDWR)) == -1) {
		perror("Cannot access IVAS");
		*unit = -1;
		*ier = 0;
		return;
	}
	ioctl(fd, UNIXmasterClear, &i);
	ivasOPEN();
	*unit = fd;
	*ier = 1;
	return;
}

griv01_()
{
	close(fd);
}

griv02_()
{
	ivasGPHset(0, 0, 1024, 1024, 0);
	return;
}

griv03_(x,y)
float *x,*y;
{
	int ix = *x,iy = *y;

	ivasGPHmove(ix, iy, GPHabs);

	return;
}

griv04_(x,y)
float *x,*y;
{
	int ix = *x,iy = *y;

	ivasGPHline(ix, iy, GPHarEnSupp);

	return;
}

griv05_()
{
	ivasGPHdot(0);

	return;
}

griv06_()
{
	ivasPURGE();

	return;
}

griv07_(ix,iy)
int *ix,*iy;
{
	ivasPURGE();
	ivasCSshape(1, 32, 1, 1);
	ivasMOUSEput(*ix, *iy, 3);
	return;
}

griv08_(ix,iy)
float *ix,*iy;
{
	int button;
	ivasPURGE();
	ivasMOUSEstatus(&button, ix, iy, 7, 0);
	ivasCSshape(0, 128, 1, 0);
	return;
}

griv09_(c)
float *c;
{
	int i = *c;

	if (i < 0 || i > 15)
		i = 1;
	ivasGPHvalue(i, i);

	return;
}

griv10_(c,r,g,b)
float *c,*r,*g,*b;
{
	int i = *c, ir = *r*15, ig = *g*15, ib = *b*15;
        int color = ir + ig*16 + ib*256;

	if (i < 0 || i > 15) return;
	ivasPURGE();
	ivasGMdefGraphic(i, GMcolor, color, 0);

	return;
}

griv11_(x, y)
float *x, *y;
{
	int i = *x, j = *y;
	ivasGPHfill(i, j, GPHabs);

	return;
}

griv12_()		/* Initialize display */
{
	ivasINITall();

	ivasGPHorigin(0, 0);

	ivasGMfreeze(1);
	ivasGMdefGraphic(0, GMcolor,   0 +  0*16 +  0*256, 0);
	ivasGMdefGraphic(1, GMcolor,  15 + 15*16 + 15*256, 0);
	ivasGMdefGraphic(2, GMcolor,  15 +  0*16 +  0*256, 0);
	ivasGMdefGraphic(3, GMcolor,   0 + 15*16 +  0*256, 0);
	ivasGMdefGraphic(4, GMcolor,   0 +  0*16 + 15*256, 0);
	ivasGMdefGraphic(5, GMcolor,   0 + 15*16 + 15*256, 0);
	ivasGMdefGraphic(6, GMcolor,  15 +  0*16 + 15*256, 0);
	ivasGMdefGraphic(7, GMcolor,  15 + 15*16 +  0*256, 0);
	ivasGMdefGraphic(8, GMcolor,  15 +  7*16 +  0*256, 0);
	ivasGMdefGraphic(9, GMcolor,   7 + 15*16 +  0*256, 0);
	ivasGMdefGraphic(10, GMcolor,  0 + 15*16 +  7*256, 0);
	ivasGMdefGraphic(11, GMcolor,  0 +  7*16 + 15*256, 0);
	ivasGMdefGraphic(12, GMcolor,  7 +  0*16 + 15*256, 0);
	ivasGMdefGraphic(13, GMcolor, 15 +  0*16 +  7*256, 0);
	ivasGMdefGraphic(14, GMcolor,  5 +  5*16 +  5*256, 0);
	ivasGMdefGraphic(15, GMcolor, 10 + 10*16 + 10*256, 0);
	ivasGMfreeze(0);

	ivasGPHvalue(1, 1);
	/* erase cursor */
	ivasCSshape(0, 128, 1, 0);
}
