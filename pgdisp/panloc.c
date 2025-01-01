/* The panloc routine moves the pixel under the cursor in the location window */
/* to the center of the main window. */

/* Sam Southard, Jr. */
/* Created: 23-Aug-1991 */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 25-Nov-1991	SNS/CIT	Now handles separate X & Y zoom factors */
/* 10-Apr-1992	SNS/CIT	Now handles variable width borders */

#include "figdisp.h"
#include "globals.h"

void panloc(event)
XButtonEvent event;
{
	int x,y;

	void checkoff();	/* check and change the image offsets */
	void redrawim();	/* redraw the bitmap image */
	void showloc();		/* update the location window */

	/* only the left button matters */
	if (event.button != Button1) return;

	x = (event.x-loc.winxoff)*loc.xzoom;
	y = (event.y-loc.winyoff)*loc.yzoom;

	if (bm.xzoom > 0) bm.xoff= x - (((int)bm.width >> 1) >> bm.xzoom);
	else if (bm.xzoom < 0) bm.xoff= x - (((int)bm.width >> 1) << -bm.xzoom);
	else bm.xoff= x - ((int)bm.width >> 1);

	if (bm.yzoom > 0) bm.yoff= y - (((int)bm.height >> 1) >> bm.yzoom);
	else if (bm.yzoom < 0) bm.yoff= y - (((int)bm.height >> 1) <<-bm.yzoom);
	else bm.yoff= y - ((int)bm.height >> 1);

	checkoff();
	XFillRectangle(display, bm.win, bitgcclear, 0, 0, bm.width, bm.height);
	redrawim(display_to_imagecol(0), display_to_imagerow(0),
		display_to_imagecol(bm.width-1),
		display_to_imagerow(bm.imheight-1));
	showloc();

	return;
}
