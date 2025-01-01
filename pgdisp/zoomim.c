/* The zoomim routine zooms the bitmap graphics to the specified zoom factor. */
/* The zoom factor is specified as a power of two. */

/* Sam Southard, Jr. */
/* Created: 1-Aug-1991 */
/* Modification History: */
/*  4-Aug-1991	SNS/CIT	Now uses slow zoom method if the zoomed image doesn't */
/*			fit in memory. */
/*  5-Aug-1991	SNS/CIT	Slow zoom now the only zoom algorithm. */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo */
/* 23-Aug-1991	SNS/CIT	Now checks to see if the x & y offsets are valid */
/*			after the zoom. */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 25-Nov-1991	SNS/CIT	Modified to handle separate X & Y zoom factors */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>
#include <limits.h>
#ifndef VMS
#include <malloc.h>
#endif

void zoomim(xzoom,yzoom)
int xzoom;	/* the new X zoom factor */
int yzoom;	/* the new Y zoom factor */
{
	int ix,iy;			/* the size of the new image */

	void copyzoom();	/* update the zoomed image */
	void exposebmwin();	/* expose the image */
	void checkoff();	/* check and update the image offsets */
	void redrawim();	/* redraw the image */

	/* we shift things by these, so make sure they don't get out of hand */
	if (xzoom > 30 || yzoom > 30)
	{
		(void)fprintf(stderr,"Give me a break!\n");
		return;
	}

	/* update the offsets so the center remains constant */
	ix=display_to_imagecol((int)bm.width>>1);
	iy=display_to_imagerow((int)bm.height>>1);

	if (xzoom < 0) bm.xoff= ix - (((int)bm.width >> 1) << -xzoom);
	else if (xzoom > 0) bm.xoff = ix - (((int)bm.width>>1) >> xzoom);
	else bm.xoff= ix - ((int)bm.width>>1);

	if (yzoom < 0) bm.yoff= iy - (((int)bm.height >> 1) << -yzoom);
	else if (yzoom > 0) bm.yoff = iy -  (((int)bm.height>>1) >> yzoom);
	else bm.yoff= iy - ((int)bm.height>>1);

	bm.xzoom=xzoom;
	bm.yzoom=yzoom;

	/* make sure the offsets are valid */
	checkoff();
	
	/* we'll need to clear the screen */
	XFillRectangle(display, bm.win, bitgcclear, 0, 0, bm.width, bm.height);

	/* now update and expose the image */
	redrawim(display_to_imagecol(0), display_to_imagerow(0),
		display_to_imagecol(bm.width-1),
		display_to_imagerow(bm.height-1));

	return;
}
