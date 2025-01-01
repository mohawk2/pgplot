/* The resizeloc routine resizes the location window to the given size.  If */
/* the given size is greater than half the total image size the location */
/* window is reduced to that size.  Note that this routine is not only called */
/* when the user resizes an event.  It is also called to make sure that the */
/* current window size is valid. */

/* Return Values: */
/* FAIL		Something went wrong and the program should exit */
/* SUCCEED	Everything's fine */

/* Sam Southard, Jr. */
/* Created: 22-Aug-1991 */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT	Modified to force the scaling factors to be the same */
/*			(the lesser one - for a larger location window - is */
/*			chosen) if the user has set the forceSquare resource. */
/* 10-Apr-1992	SNS/CIT	Modified to round scaling factors up (for a smaller */
/*			window).  Also uses the larger scaling factor (for */
/*			a smaller window) if the user has set the forceSquare */
/*			resource.  This is because we need to deal with */
/*			window managers that don't let us resize the window */
/*			from the program.  If we go down in size, then the */
/*			worst thing that will happen is there will be more */
/*			blank space around the image. */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>
#ifndef VMS
#include <malloc.h>
#endif

int resizeloc(x,y)
int x,y;	/* the new size of the window */
{
	unsigned char *newdat;		/* new data area */
	int datx,daty;			/* the data widths */

	char *realloc();

	/* The usable data area - at least BLANK_WIDTH border all around */
	datx= x-2*BLANK_WIDTH;
	daty= y-2*BLANK_WIDTH;
	if (datx < 1) datx=1;
	if (daty < 1) daty=1;

	loc.xzoom = bm.imwidth/datx;
	if (loc.xzoom*datx < bm.imwidth) ++loc.xzoom;
	loc.yzoom = bm.imheight/daty;
	if (loc.yzoom*daty < bm.imheight) ++loc.yzoom;

	if (loc.xzoom < 2) loc.xzoom=2;
	if (loc.yzoom < 2) loc.yzoom=2;

	if (res.forcesquare)
	{
		if (loc.xzoom < loc.yzoom) loc.xzoom=loc.yzoom;
		else loc.yzoom=loc.xzoom;
	}

	if ((datx= bm.imwidth/loc.xzoom) < 1) datx=1;
	if ((daty= bm.imheight/loc.yzoom) < 1) daty=1;

	if (datx != loc.imwidth || daty != loc.imheight)
	{
		/* we actually have to get new data */
		if ((newdat=(unsigned char *)realloc((char *)loc.imdat,
			(unsigned)datx*daty)) == NULL)
		{
			(void)fprintf(stderr,MSG_MALLOC);
			return(FAIL);
		}
		loc.image->data=NULL;
		XDestroyImage(loc.image);
		loc.imdat=newdat;
		loc.imwidth=datx;
		loc.imheight=daty;
		loc.image=XCreateImage(display, bitvisual, bitdepth, ZPixmap, 0,
			(char *)loc.imdat, loc.imwidth, loc.imheight, 8, 0);
	}
	loc.width=x;
	loc.height=y;
	loc.winxoff=(loc.width-loc.imwidth)/2;
	loc.winyoff=(loc.height-loc.imheight)/2;

	return(SUCCEED);
}
