/* The checkoff routine checks makes sure that the bitmap's x and y offsets */
/* are within reasonable ranges.  Specifically, at least 10% of the window's */
/* width and height must be used by the image, so that the image never goes */
/* off the screen. */

/* Sam Southard, Jr. */
/* Created: 4-Aug-1991 */
/* 12-Sep-1991	SNS/CIT	xoff & yoff now relative to unzoomed image */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 25-Nov-1991	SNS/CIT	Now has separate X & Y zoom factors */
/* 10-Apr-1992	SNS/CIT	OLD code removed. */

#include "figdisp.h"
#include "globals.h"

void checkoff()
{
	double xscale,yscale;	/* scaling factors due to zoom */

	if (bm.xzoom > 0) xscale= 1.0/(1<<bm.xzoom);
	else if (bm.xzoom < 0) xscale= (1 << -bm.xzoom);
	else xscale=1;
		
	if (bm.yzoom > 0) yscale= 1.0/(1<<bm.yzoom);
	else if (bm.yzoom < 0) yscale= (1 << -bm.yzoom);
	else yscale=1;
		
	/* Now make sure that at least 10% of the window in each dimension */
	/* contains image data */
	if (bm.xoff < -0.9*(int)bm.width*xscale)
		bm.xoff= -0.9*(int)bm.width*xscale;
	if (bm.xoff > bm.imwidth-0.1*xscale*(int)bm.width)
		bm.xoff= bm.imwidth-0.1*xscale*(int)bm.width;
	if (bm.yoff < -0.9*(int)bm.height*yscale)
		bm.yoff= -0.9*(int)bm.height*yscale;
	if (bm.yoff > bm.imheight-0.1*yscale*(int)bm.height)
		bm.yoff= bm.imheight-0.1*yscale*(int)bm.height;
}
