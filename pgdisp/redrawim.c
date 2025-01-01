/* The redrawim routine updates the window after the raw data has changed. */
/* the input points are the boundaries, in unzoomed image coordinates, of */
/* the area which changed.  It's really just a wrapper around copyzoom and */
/* exposebmwin, which is only needed because copyzoom assumes that its */
/* arguments are in the correct order (ascending coords). */

/* Sam Southard, Jr. */
/* Created: 5-Aug-1991 */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 29-Jan-1992	SNS/CIT	Now retains the original coordinate (non-clipped) so */
/*			that the image will be cleared if necessary */
/* 10-Apr-1992	SNS/CIT	Updated to deal with winxoff & winyoff */

#include "figdisp.h"
#include "globals.h"

void redrawim(x1,y1,x2,y2)
int x1,y1;	/* the first point */
int x2,y2;	/* the second point */
{
	int i,j;
	int rx1=x1,rx2=x2,ry1=y1,ry2=y2;

	void copyzoom();	/* update the zoomed image */
	void exposebmwin();	/* expose the bitmap image */

	/* first make sure that everything's ascending */
	if (x1 > x2)
	{
		i=x2;
		x2=x1;
		x1=i;
	}
	if (y1 > y2)
	{
		i=y2;
		y2=y1;
		y1=i;
	}

	/* now make sure we're inside the image */
	if (x1 < 0) x1=0;
	if (x2 < 0) x2=0;
	if (x1 >= bm.imwidth) x1=bm.imwidth-1;
	if (x2 >= bm.imwidth) x2=bm.imwidth-1;
	if (y1 < 0) y1=0;
	if (y2 < 0) y2=0;
	if (y1 >= bm.imheight) y1=bm.imheight-1;
	if (y2 >= bm.imheight) y2=bm.imheight-1;

	copyzoom(x1,y1,x2,y2);

	/* update, using the original coords - that way the display gets */
	/* cleared */
	i=imagecol_to_display(rx1);
	j=imagerow_to_display(ry1);
	exposebmwin(i, j, imagecol_to_display(rx2+1)-i,
		imagerow_to_display(ry2+1)-j);
	return;
}
