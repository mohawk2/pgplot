/* The update patch routine updates the patch window to show the points */
/* centered around the given x and y */

/* Sam Southard, Jr. */
/* Created: 20-Aug-1991 */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */

/* Wish List: */
/*	Add code to determine how many chars will be needed to display any */
/* data point, instead of shoehorning the data into a pre-defined format */

#include "figdisp.h"
#include "globals.h"

#include <stdio.h>

void updatepatch(x,y)
int x,y;	/* the new patch center (image coordinates) */
{
	char str[8];	/* the display string */
	int i,j;	/* silly loop variables */
	int xpix,ypix;	/* the number of x and y pixels */
	int sx,sy;	/* the starting x and y values */
	double val;	/* a temporary value for printing */
	GC gc;		/* the graphics context to use */

	xpix=patch.width/(8*res.textfont->max_bounds.width);
	ypix=patch.height/(res.textfont->ascent+res.textfont->descent);

	/* don't even bother */
	if (xpix < 2 || ypix < 2) return;

	/* first print the diagonal label */

	XClearWindow(display, patch.win);
	XDrawString(display, patch.win, textgc, 0, res.textfont->ascent,
		"  Y \\ X ",7);
	XDrawLine(display, patch.win, textgc, 0, res.textfont->ascent+2,
		(int)patch.width-1, res.textfont->ascent+2);
	XDrawLine(display, patch.win, textgc,
		(int)(7.5*res.textfont->max_bounds.width), 0,
		(int)(7.5*res.textfont->max_bounds.width), (int)patch.height-1);
	
	sx= x - ((xpix-1) >> 1);
	sy= y - ((ypix-1) >> 1);

	if (sx+xpix < 2) sx= -xpix+2;
	if (sy+ypix < 2) sy= -ypix+2;
	if (sx >= (int)bm.imwidth) sx=bm.imwidth-1;
	if (sy >= (int)bm.imheight) sy=bm.imheight-1;

	x= sx+((xpix-1) >> 1);
	y= sy+((ypix-1) >> 1);

	patch.xoff=x;
	patch.yoff=y;

	/* now print the column labels */
	for (j=0 ; j < xpix-1 ; ++j)
	{
		if (sx+j < 0 || sx+j >= bm.imwidth) continue;
		(void)sprintf(str,"%7.1f",(sx+j+bm.curxoff)*bm.curxsc);
		XDrawString(display, patch.win, textgc,
			(j+1)*8*res.textfont->max_bounds.width,
			res.textfont->ascent, str, 7);
	}

	/* now print the rest of the data */
	for (i=0 ; i < ypix-1 ; ++i)
	{
		if (sy+i < 0 || sy+i >= (int)bm.imheight) continue;

		/* take care of the row label */
		(void)sprintf(str,"%7.1f",(sy+i+bm.curyoff)*bm.curysc);
		XDrawString(display, patch.win, textgc, 0, res.textfont->ascent+
			(i+1)*(res.textfont->ascent+res.textfont->descent),
			str, 7);
		
		/* now the actual data */
		for (j=0 ; j < xpix-1 ; ++j)
		{
			if (sx+j == x && sy+i == y) gc=textgcclear;
			else gc=textgc;
			if (sx+j < 0 || sx+j >= (int)bm.imwidth) continue;
			if (bppix == 16) val=rimdat.b16[sx+j+(sy+i)*bm.imwidth];
			else val=rimdat.b8[sx+j+(sy+i)*bm.imwidth];
			(void)sprintf(str,"%7.1f",val*bm.dsc+bm.doff);
			XDrawImageString(display, patch.win, gc,
				(j+1)*8*res.textfont->max_bounds.width,
				res.textfont->ascent + (i+1)*
				(res.textfont->ascent+res.textfont->descent),
				str, 7);
		}
	}
	XFlush(display);
	
	return;
}

void exposepatch()
{
	updatepatch(patch.xoff, patch.yoff);
}
