/* This routine simply draws a line in the specified color (0-255) between */
/* the two specified points (image coords, upper left is 0,0) using */
/* Bresenham's algorithm. */

/* Sam Southard, Jr. */
/* Created: 1-Aug-1991 */
/* Modification History: */
/* 14-Aug-1991	SNS/CIT	Hooks for xvideo removed */
/* 23-Aug-1991	SNS/CIT	Now updates the location window */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/* 13-Sep-1991	SNS/CIT	Stupid bug fixed (exchanging the two points made them */
/*			into the same point). */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */

#include "figdisp.h"
#include "globals.h"

void drawline(x1,y1,x2,y2,val)
int x1,y1;	/* the first point */
int x2,y2;	/* the second point */
int val;	/* the pixel value to use */
{
	int x,y;	/* the place we're on */
	int delx,dely;	/* the change from point 1 to point 2 */
	int dx,dy,de;	/* the amount to increment when x & y change */
	int npts;	/* number or points in the line */
	int e;		/* the error function (look up the algorithm) */
	int nx,ny,ne;	/* amount to change when only x or y changes */

	void redrawim();

	if (x2 < x1)
	{ /* we always use ascending x */
		x=x2;
		x2=x1;
		x1=x;
		y=y2;
		y2=y1;
		y1=y;
	}

	x=x1;
	y=y1;
	delx=x2-x1;
	dely=y2-y1;

	if (dely > 0) dy=1;
	else if (dely < 0)
	{
		dely= -dely;
		dy= -1;
	} else dy=0;

	if (delx) dx=1;
	else dx=0;

	if (delx < dely)
	{
		npts=dely;
		nx=0;
		ny=dy;
		ne= delx<<1;
		e=(delx<<1)-dely;
		de= (delx<<1)-(dely<<1);
	} else {
		npts=delx;
		nx=dx;
		ny=0;
		ne= dely<<1;
		e= (dely<<1)-delx;
		de=(dely<<1)-(delx<<1);
	}
	++npts;

	val <<= 8;

	while (npts--)
	{
		if (x >= bm.imwidth || y >= bm.imheight) break;
		if (x >= 0 && y >= 0)
		{
			if (bppix == 16) rimdat.b16[x+bm.imwidth*y]=val;
			else rimdat.b8[x+bm.imwidth*y]=val;
		}
		if (e < 0)
		{
			x += nx;
			y += ny;
			e += ne;
		} else {
			x += dx;
			y += dy;
			e += de;
		}
	}

	if (y1 > y2)
	{
		y=y1;
		y1=y2;
		y2=y;
	}
	redrawim(x1,y1,x2,y2);
	(void)updateloc();
	
	return;
}
