/* The exposebmwin routine handles an expose event on the bitmap graphics */
/* window.  The argument is the expose event. */

/* Sam Southard, Jr. */
/* Created: 31-Jul-1991 (from exposelgwin) */
/*  1-Aug-1991	SNS/CIT	Now uses fastdisp macros */
/*  2-Aug-1991	SNS/CIT	fastdisp macros now return/take values relative to */
/*		the unzoomed image. */
/*  4-Aug-1991	SNS/CIT	Now deals with the slow zoom algorithm */
/*  5-Aug-1991	SNS/CIT	Slow zoom algorithm is now the only one */
/* 15-Aug-1991	SNS/CIT No longer includes vista hooks. */
/* 19-Aug-1991	SNS/CIT	Now treats bm.width & bm.height as unsigned */
/* 20-Aug-1991	SNS/CIT	Now clears the exposed area (to deal with servers */
/*			that do backing store) */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 10-Apr-1992	SNS/CIT	Now respects a border around the Ximage structure */
/* 23-Apr-1992	SNS/CIT	Initial checks for negative x & y done first. */

/* The program include files */
#include "figdisp.h"
#include "globals.h"

void exposebmwin(x,y,width,height)
int x,y;		/* starting position of the area of the window */
int width,height;	/* size to expose */
{
	if (x < 0)
	{
		width += x;
		x=0;
	}
	if (y < 0)
	{
		height += y;
		y=0;
	}

	/* make sure we're respecting the border, if any, around the Ximage */
	if (displaycol_to_xim(x) < 0)
	{
		XFillRectangle(display, bitgcclear, x, y,
			xim_to_displaycol(0)-x, (unsigned)height);
		width -= (xim_to_displaycol(0)-x);
		x=xim_to_displaycol(0);
	}
	if (displayrow_to_xim(y) < 0)
	{
		XFillRectangle(display, bitgcclear, x, y, (unsigned)width,
			xim_to_displayrow(0)-y);
		height -= (xim_to_displayrow(0)-y);
		y=xim_to_displayrow(0);
	}
	/* no sense refreshing before the image */
	if (x < imagecol_to_display(0))
	{
		XFillRectangle(display, bm.win, bitgcclear, x, y,
			(unsigned)(imagecol_to_display(0)-x), (unsigned)height);
		width -= (imagecol_to_display(0)-x);
		x = imagecol_to_display(0);
	}
	if (y < imagerow_to_display(0))
	{
		XFillRectangle(display, bm.win, bitgcclear, x, y,
			(unsigned)width, (unsigned)(imagerow_to_display(0)-y));
		height -= (imagerow_to_display(0)-y);
		y = imagerow_to_display(0);
	}

	/* make sure we don't pass the end of the window */
	if (x+width > (int)bm.width) width= (int)bm.width-x;
	if (y+height > (int)bm.height) height= (int)bm.height-y;

	/* make sure we're not going past the image */
	if (display_to_imagecol(x+width) >= bm.imwidth)
	{
		XFillRectangle(display, bm.win, bitgcclear,
			imagecol_to_display(bm.imwidth), y,
			(unsigned)(x+width-imagecol_to_display(bm.imwidth)),
			(unsigned)height);
		width -= (x+width - imagecol_to_display(bm.imwidth));
	}
	if (display_to_imagerow(y+height) >= bm.imheight)
	{
		XFillRectangle(display, bm.win, bitgcclear, x,
			imagerow_to_display(bm.imheight), (unsigned)width,
			(unsigned)(y+height-imagerow_to_display(bm.imheight)));
		height -= (y+height - imagerow_to_display(bm.imheight));
	}

	/* make sure we're respecting the border around the Ximage */
	if (displaycol_to_xim(x+width) >= bm.image->width)
	{
		XFillRectangle(display, bm.win, bitgcclear,
			xim_to_displaycol(bm.image->width), y,
			(unsigned)(x+width-xim_to_displaycol(bm.image->width)),
			(unsigned)height);
		width -= (x+width-xim_to_displaycol(bm.image->width));
	}
	if (displayrow_to_xim(y+height) >= bm.image->height)
	{
		XFillRectangle(display, bm.win, bitgcclear, x,
			xim_to_displayrow(bm.image->height), (unsigned)width,
			(unsigned)(y+height-
				xim_to_displayrow(bm.image->height)));
		height -= (y+height-xim_to_displayrow(bm.image->height));
	}

	if (width <= 0 || height <= 0 || x >= bm.width || y >= bm.height ||
	    displaycol_to_xim(x) >= bm.image->width ||
	    displayrow_to_xim(y) >= bm.image->height)
		return;

	/* finally we can expose the main part of the window */
	XPutImage(display, bm.win, bitgc, bm.image, displaycol_to_xim(x),
		displayrow_to_xim(y), x, y, (unsigned)width, (unsigned)height);

	XFlush(display);

	return;
}
