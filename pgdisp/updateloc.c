/* The updateloc routine updates the data in the location window.  The */
/* showloc routine shows the location window, with the lines defining the */
/* border of the main window. */

/* Return Values: */
/* SUCCEED	Everything went fine */
/* FAIL		There was some serious error and the program should end */

/* Sam Southard, Jr. */
/* Created: 22-Aug-1991 */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 10-Apr-1992	SNS/CIT	Modified to deal with bm.winxoff & bm.winyoff */
/* 25-Jun-1992	SNS/CIT	Showloc now flushes the display before returning. */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>

static XImage *horline;		/* A horizontal line structure */
static XImage *vertline;	/* A vertical line structure */
static unsigned char *linedat=NULL;	/* the line data */

/* A trivial little macro */
#define max(x,y)	((x) > (y) ? (x) : (y))

int updateloc()
{
	int linesize;		/* the new maximum line size */
	int i,j;		/* silly loop variables */

	char *malloc();
	void showloc();

	/* the location window can be no larger than half the image size */
	/* The resizeloc routine will do whatever's necessary to size the */
	/* window appropriately, including getting a new image */

	if (resizeloc((int)loc.width,(int)loc.height)) return(FAIL);

	/* we may need to get a bigger line size */
	if (linedat != NULL && horline->width < loc.width)
	{
		free((char *)linedat);
		horline->data=vertline->data=NULL;
		XDestroyImage(horline);
		XDestroyImage(vertline);
		linedat=NULL;
	}

	/* if this is the first time through, or we need to increase the */
	/* line size, set up all the structures */
	if  (linedat == NULL)
	{
		linesize=max(BM_MAX_WIDTH,loc.width);
		linesize=max(linesize,loc.height);

#ifdef lint
		if (malloc((unsigned)linesize) == NULL)
#else
		if ((linedat=(unsigned char *)malloc((unsigned)linesize))
		    == NULL)
#endif
		{
			(void)fprintf(stderr,MSG_MALLOC);
			return(FAIL);
		}

		horline=XCreateImage(display, bitvisual, bitdepth, ZPixmap, 0,
			(char *)linedat, (unsigned)linesize, 1, 8, 0);
		vertline=XCreateImage(display, bitvisual, bitdepth, ZPixmap, 0,
			(char *)linedat, 1, (unsigned)linesize, 8, 0);
		
		/* set the pixel to the outline pixel */
		while (linesize-- > 0)
			linedat[linesize]=locline.pixel;
	}

	/* now we can actually copy the data */

	/* at this point everything should be set up to copy the image */
	if (bppix == 16)
	{
		for (i = 0 ; i < loc.imheight ; ++i)
			for (j=0 ; j < loc.imwidth ; ++j)
				loc.imdat[i*loc.imwidth+j]=
					allcells[rimdat.b16[j*loc.xzoom+
						i*loc.yzoom*bm.imwidth]].pixel;
	} else {
		for (i = 0 ; i < loc.imheight ; ++i)
			for (j=0 ; j < loc.imwidth ; ++j)
				loc.imdat[i*loc.imwidth+j]=
					allcells[rimdat.b8[j*loc.xzoom+
						i*loc.yzoom*bm.imwidth]].pixel;
	}
	
	showloc();
	return(SUCCEED);
}

/* The showloc routine displays the scaled image if the user has requested it */

void showloc()
{
	int x1,x2,y1,y2;	/* the corners of the outline */
	int cx1,cx2,cy1,cy2;	/* the chopped corners */

	/* don't bother if the user can't see it */
	if (!loc.mapped) return;

	/* clear the border */
	XFillRectangle(display, loc.win, bitgcclear, 0, 0, loc.width,
		loc.winyoff);
	XFillRectangle(display, loc.win, bitgcclear, 0, 0, loc.winxoff,
		loc.height);
	XFillRectangle(display, loc.win, bitgcclear,
		(int)loc.imwidth+loc.winxoff, 0,
		loc.width-loc.imwidth-loc.winxoff+1,loc.height);
	XFillRectangle(display, loc.win, bitgcclear, 0,
		(int)loc.imheight+loc.winyoff, loc.width,
		loc.height-loc.imheight-loc.winyoff+1);

	/* first the actual image data */
	XPutImage(display, loc.win, bitgc, loc.image, 0, 0, loc.winxoff,
		loc.winyoff, loc.imwidth, loc.imheight);

	/* figure out where the corners are */
	cx1=x1 = xim_to_imagecol(0)/loc.xzoom+loc.winxoff;
	cx2=x2 = xim_to_imagecol((int)bm.image->width-1)/loc.xzoom+loc.winxoff;
	cy1=y1 = xim_to_imagerow(0)/loc.yzoom+loc.winyoff;
	cy2=y2 = xim_to_imagerow((int)bm.image->height-1)/loc.yzoom+loc.winyoff;

	/* chop the coordinates if necessary */
	if (cx1 < 0) cx1=0;
	if (cx2 >= loc.width) cx2=loc.width-1;
	if (cy1 < 0) cy1=0;
	if (cy2 >= loc.height) cy2=loc.height-1;

	/* the top line */
	if (y1 >= 0)
		XPutImage(display, loc.win, bitgc, horline, 0, 0, cx1, cy1,
			(unsigned)(cx2-cx1+1), 1);
	/* the bottom line */
	if (y2 < loc.height)
		XPutImage(display, loc.win, bitgc, horline, 0, 0, cx1, cy2,
			(unsigned)(cx2-cx1+1), 1);
	/* the left line */
	if (x1 >= 0)
		XPutImage(display, loc.win, bitgc, vertline, 0, 0, cx1, cy1, 1,
			(unsigned)(cy2-cy1+1));
	/* the right line */
	if (x2 < loc.width)
		XPutImage(display, loc.win, bitgc, vertline, 0, 0, cx2, cy1, 1,
			(unsigned)(cy2-cy1+1));
	
	XFlush(display);
	return;
}
