/* The resizebmwin routine updates the bitmap graphics window in response to */
/* a ConfigureNotify event. */

/* Sam Southard, Jr. */
/* Created: 31-Jul-1991 (from resizelgwin) */
/*  1-Aug-1991	SNS/CIT	Now uses fastdisp macros */
/*  2-Aug-1991	SNS/CIT	Can no longer cause image to go off the window when */
/*			shrinking the window. */
/*  4-Aug-1991	SNS/CIT	Now handles slow zoom algorithm */
/*  5-Aug-1991	SNS/CIT	Slow zoom algorithm now the only one used. */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo. */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/*  8-Apr-1992	SNS/CIT	SIlly bug in call to XCreateImage fixed. */
/*  9-Apr-1992	SNS/CIT	Now longer uses maximum or minimum window dimensions */
/* 10-Apr-1992	SNS/CIT	Now deals with winxoff & winyoff */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>
#ifndef VMS
#include <malloc.h>
#endif

void resizebmwin(event)
XConfigureEvent event;
{
	int changed=0;	/* whether or not we need to resize the window again */
	int ow,oh;	/* the old width & height */
	unsigned char *newdat;
	static int lastchanged=0;	/* whether or not the last time */
					/* through we changed */

	void checkoff();	/* check and change the image offsets */
	void redrawim();	/* redraw the image */
	void showloc();		/* update the location window */

	ow=bm.width;
	oh=bm.height;

	bm.width=event.width;
	bm.height=event.height;

	/* calculate a new image offset (keep image centered in the window) */
	bm.xoff += (display_to_imagecol(ow)-
		display_to_imagecol((int)bm.width-1))/2;
	bm.yoff += (display_to_imagerow(oh)-
		display_to_imagerow((int)bm.height-1))/2;

	checkoff();

	if (bm.width != bm.image->width || bm.height != bm.image->height)
	{
		if ((newdat=(unsigned char *)realloc((char *)bm.imdat,
			bm.width*bm.height)) == NULL)
		{
			(void)fprintf(stderr,MSG_MALLOC);
			(void)fprintf(stderr,MSG_TRYRESIZE);
			bm.winxoff=(bm.width-bm.image->width)/2;
			bm.winyoff=(bm.height-bm.image->height)/2;
			changed=1;
		} else {
			bm.image->data=NULL;
			XDestroyImage(bm.image);
			bm.imdat=newdat;
			bm.image=XCreateImage(display, bitvisual, 8, ZPixmap,
				0, (char *)bm.imdat, (unsigned)bm.width,
				(unsigned)bm.height, 8, 0);
			bm.winxoff=bm.winyoff=0;
		}
	}

	/* I'd like to change, but the last time through I may have tried, */
	/* and I don;t want to get into an infinite loop. */
	if (changed && !lastchanged) {
		lastchanged=1;
		XResizeWindow(display,bm.win,bm.image->width,bm.image->height);
	} else {
		lastchanged=0;
		redrawim(display_to_imagecol(0), display_to_imagerow(0),
			display_to_imagecol((int)bm.width-1),
			display_to_imagerow((int)bm.height-1));
		showloc();
	}

	return;
}
