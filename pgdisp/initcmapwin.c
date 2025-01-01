/* The initcmapwin routine initializes the color map window */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 9-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 10-Oct-1991	SNS/CIT	Window manager hints now in initwmattr */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT No longer changes the border pixel */
/*  6-Mar-1992	SNS/CIT	Now works on all visual depths */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/* 13-May-1992	SNS/CIT	Now respects the requested X & Y position. */

/* The system include files */
#include <stdio.h>

/* the X Window include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"
#include "figdisp.icon"

#ifndef VMS
#include <malloc.h>
#endif

int initcmapwin()
{
	XSetWindowAttributes winattr;	/* window attributes */

	char *malloc();
	void initwmattr();

	/* create the color map window data and structure */
	cwin.width=res.cgeo.w;
	cwin.height=res.cgeo.h;
	if ((cwin.imdat=(unsigned char *)malloc(cwin.width*cwin.height))
	    == NULL)
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}
		
	cwin.image=XCreateImage(display, bitvisual, bitdepth, ZPixmap, 0,
		(char *)cwin.imdat, cwin.width, cwin.height, 8, 0);

	/* create a window for the color map */
	winattr.background_pixel=bm.pix[0];
	winattr.border_pixel=bm.pix[0];
	winattr.colormap=bitcmap;
	cwin.win=XCreateWindow(display, RootWindow(display,screen), res.cgeo.x,
		res.cgeo.y, cwin.width, cwin.height, BORDER_WIDTH,
		(int)bitdepth, InputOutput, bitvisual,
		CWBackPixel | CWColormap| CWBorderPixel, &winattr);
	cwin.mapped=0;

	/* get the icon loaded */
	cwin.icon=XCreateBitmapFromData(display, cwin.win, figdisp_bits,
		figdisp_width, figdisp_height);

	/* set up the window manager hints */
	initwmattr(cwin, "color map", "color map", &res.cgeo);

	/* we need to listen for new data */
	XSelectInput(display, cwin.win, ExposureMask | StructureNotifyMask |
		KeyPressMask);

	return(SUCCEED);
}
