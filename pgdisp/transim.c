/* The transim routine translates the image in response to the given button */
/* press event.  This means that it moves the point under the cursor to the */
/* center of the window */

/* Sam Southard, Jr. */
/* Created: 31-Jul-1991 */
/*  1-Aug-1991	SNS/CIT	Modified to work with zoomed images */
/*  7-Aug-1991	SNS/CIT	Now must move the cursor at least one (possibly */
/*			zoomed pixel) and updates the cursor readout. */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo */
/* 20-Aug-1991	SNS/CIT	updatetitle now checks for bm.showcur, so we don't */
/*			have to */
/* 23-Aug-1991	SNS/CIT	Now updates the location window */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 25-Nov-1991	SNS/CIT	Now handles separate X & Y zoom factors */

#include "figdisp.h"
#include "globals.h"

void transim(event)
XButtonEvent event;
{
	int dx,dy;	/* the change in offset */

	void checkoff();	/* check and change the image offsets */
	void redrawim();	/* redraw the bitmap image */
	void updatetitle();	/* update the title bar to show cursor */
	void showloc();		/* update the location window */

	dx= (event.x - ((int)bm.width >> 1));
	dy= (event.y - ((int)bm.height >> 1));

	/* correct for zoom factors */
	if (bm.xzoom > 0) dx >>= bm.xzoom;
	else if (bm.xzoom < 0) dx <<= -bm.xzoom;
	
	if (bm.yzoom > 0) dy >>= bm.yzoom;
	else if (bm.yzoom < 0) dy <<= -bm.yzoom;

	if (bm.xzoom > 0 && dx == 0 && event.x != ((int)bm.width >> 1))
	{
		if (event.x > ((int)bm.width >> 1)) dx=1;
		else dx= -1;
	}
	if (bm.yzoom && dy == 0 && event.y != ((int)bm.height >> 1))
	{
		if (event.y > ((int)bm.height >> 1)) dy=1;
		else dy= -1;
	}

	bm.xoff += dx;
	bm.yoff += dy;

	checkoff();

	/* now we clear the window and expose everything */
	XFillRectangle(display, bm.win, bitgcclear, 0, 0, bm.width, bm.height);
	redrawim(display_to_imagecol(0), display_to_imagerow(0),
		display_to_imagecol((int)bm.width-1),
		display_to_imagerow((int)bm.height-1));

	updatetitle(event.x,event.y,0);
	showloc();

	return;
}
