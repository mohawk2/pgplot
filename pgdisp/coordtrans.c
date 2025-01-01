/* These routines translate display to image coordinates and the other way */
/* around.  Note that now that the beginning of the Ximage might not be at */
/* the same place as the window, these are not the same as the ximage */
/* coordinates.  The routines xim_to_xxx and xxx_to_xim have been added to */
/* deal with the other translations. */

/* Sam Southard, Jr. */
/* Created: 30-Jul-1991 */
/*  1-Aug-1991	SNS/CIT	Modified to work with zoom factors */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 25-Nov-1991	SNS/CIT	Now has separate X & Y zoom factors */
/* 10-Apr-1992	SNS/CIT	Now handles winxoff and winyoff.  xim_to_xxx and */
/*			xxx_ti_xim routines added. */

#include "figdisp.h"
#include "globals.h"

int display_to_imagecol(x)
int x;
{
	if (bm.xzoom > 0) return(bm.xoff+ (x-bm.winxoff >> bm.xzoom));
	else if (bm.xzoom < 0) return(bm.xoff+ (x-bm.winxoff << -bm.xzoom));
	else return(bm.xoff+x-bm.winxoff);
}

int display_to_imagerow(y)
int y;
{
	if (bm.yzoom > 0) return(bm.yoff+ (y-bm.winyoff >> bm.yzoom));
	else if (bm.yzoom < 0) return(bm.yoff+ (y-bm.winyoff << -bm.yzoom));
	else return(bm.yoff+y-bm.winyoff);
}

int imagecol_to_display(x)
int x;
{
	if (bm.xzoom > 0) return(((x - bm.xoff) << bm.xzoom)+bm.winxoff);
	else if (bm.xzoom < 0) return(((x - bm.xoff) >> -bm.xzoom)+bm.winxoff);
	else return(x - bm.xoff + bm.winxoff);
}

int imagerow_to_display(y)
int y;
{
	if (bm.yzoom > 0) return(((y -bm.yoff) << bm.yzoom)+bm.winyoff);
	else if (bm.yzoom < 0) return(((y - bm.yoff) >> -bm.yzoom)+bm.winyoff);
	else return(y - bm.yoff + bm.winyoff);
}

int xim_to_imagecol(x)
int x;
{
	return(display_to_imagecol(x+bm.winxoff));
}

int xim_to_imagerow(y)
int y;
{
	return(display_to_imagerow(y+bm.winyoff));
}

int imagecol_to_xim(x)
int x;
{
	return(imagecol_to_display(x)-bm.winxoff);
}

int imagerow_to_xim(y)
{
	return(imagerow_to_display(y)-bm.winyoff);
}

int xim_to_displaycol(x)
int x;
{
	return(x+bm.winxoff);
}

int xim_to_displayrow(y)
int y;
{
	return(y+bm.winyoff);
}

int displaycol_to_xim(x)
int x;
{
	return(x-bm.winxoff);
}

int displayrow_to_xim(y)
int y;
{
	return(y-bm.winyoff);
}
