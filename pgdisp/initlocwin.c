/* The initlocwin routine initializes the location window */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 8-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 10-Oct-1991	SNS/CIT	Window manager hints now in initwmattr */
/* 17-Oct-1991	SNS/CIT Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT No longer sets the border pixel */
/*  6-Mar-1992	SNS/CIT	Now works on all visual depths */
/* 10-Apr-1992	SNS/CIT	Now sets the maximum width & height and possibly the */
/*			aspect ratio.  Also uses imwidth for the width of */
/*			the bitmap & width for the width of the window */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/* 13-May-1992	SNS/CIT	Now respects the requested X & Y position. */

/* The program include files */
#include "figdisp.h"
#include "figdisp.icon"
#include "globals.h"
#include "messages.h"

/* The X Window include files */
#include <X11/cursorfont.h>

/* The system include files */
#include <stdio.h>
#ifndef VMS
#include <malloc.h>
#endif

int initlocwin()
{
	XSetWindowAttributes winattr;	/* window attributes */
	XSizeHints hints;
	Cursor cursor;

	char *malloc();
	void initwmattr();

	/* create the window data and structure */
	if ((loc.width=res.lgeo.w) < 2*BLANK_WIDTH+1)
		loc.width=2*BLANK_WIDTH+1;
	if ((loc.height=res.lgeo.h) < 2*BLANK_WIDTH+1)
		loc.height=2*BLANK_WIDTH+1;
	loc.imwidth=loc.width-2*BLANK_WIDTH;
	loc.imheight=loc.height-2*BLANK_WIDTH;
	if ((loc.imdat=(unsigned char *)malloc(loc.imheight*loc.imwidth))
	    == NULL)
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}
	loc.xzoom=loc.yzoom=1;

	loc.image=XCreateImage(display, bitvisual, bitdepth, ZPixmap, 0,
		(char *)loc.imdat, loc.imwidth, loc.imheight, 8, 0);
	
	/* create a window for the location */
	winattr.background_pixel=bm.pix[0];
	winattr.border_pixel=bm.pix[0];
	winattr.colormap=bitcmap;
	loc.win=XCreateWindow(display, RootWindow(display,screen), res.lgeo.x,
		res.lgeo.y, loc.width, loc.height, BORDER_WIDTH, (int)bitdepth,
		InputOutput, bitvisual,
		CWBackPixel | CWColormap | CWBorderPixel, &winattr);
	loc.mapped=0;	/* it's not mapped yet */

	/* now set the cursor in the location window */
	cursor = XCreateFontCursor(display, XC_plus);
	XDefineCursor(display, loc.win, cursor);

	/* Load the icon to use */
	loc.icon=XCreateBitmapFromData(display, loc.win, figdisp_bits,
		figdisp_width, figdisp_height);
	
	/* set up the window manager hints */
	initwmattr(loc, "location", "location", &res.lgeo);

	/* set up some additional hints */
	hints.max_width=bm.imwidth/2 + 2*BLANK_WIDTH;
	hints.max_height=bm.imheight/2 + 2*BLANK_WIDTH;
	hints.min_width=2*BLANK_WIDTH+1;
	hints.min_height=2*BLANK_WIDTH+1;
	hints.flags = PMaxSize | PMinSize;
	if (hints.x != -1)
	{
		hints.x=res.lgeo.x;
		hints.y=res.lggeo.y;
		hints.flags |= PPosition;
	}
	if (res.forcesquare) 
	{
		hints.max_aspect.x=bm.imwidth/2+2*BLANK_WIDTH;
		hints.max_aspect.y=bm.imheight/2;
		hints.min_aspect.x=bm.imwidth/2;
		hints.min_aspect.y=bm.imheight/2+2*BLANK_WIDTH;
		hints.flags |= PAspect;
	}
	XSetWMNormalHints(display, loc.win, &hints);

	/* location window needs resizes, exposes, buttons and keys */
	XSelectInput(display, loc.win, ExposureMask | StructureNotifyMask |
		ButtonPressMask | KeyPressMask);

	return(SUCCEED);
}
