/* The initseewin routine initializes the seeing window */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 9-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 22-Nov-1991	SNS/CIT No longer sets the border pixel */
/* 10-Apr-1992	SNS/CIT	Now sets size hints so the window manager can */
/*			prevent the user from sizing this window */

/* The system include files */
#include <stdio.h>

/* the X Window include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "figdisp.icon"

int initseewin()
{
	XSizeHints hints;

	void initwmattr();

	seeing.width=37*res.textfont->max_bounds.width;
	seeing.height=3*(res.textfont->ascent+res.textfont->descent);
	seeing.win=XCreateSimpleWindow(display, RootWindow(display, screen), 0,
		0, seeing.width, seeing.height, BORDER_WIDTH, CopyFromParent,
		WhitePixel(display,screen));
	seeing.mapped=0;
	
	/* Create the pixmap */
	seeing.pixmap=XCreatePixmap(display, RootWindow(display,screen),
		seeing.width, seeing.height, bitdepth);
	
	/* set up the icon */
	seeing.icon=XCreateBitmapFromData(display, seeing.win, figdisp_bits,
		figdisp_width, figdisp_height);

	/* set up the window manager hints */
	initwmattr(seeing, "seeing", "seeing", (struct geometry *)NULL);

	/* set up some extra hints */
	hints.min_width=hints.max_width=seeing.width;
	hints.min_height=hints.max_height=seeing.height;
	hints.flags=PMinSize | PMaxSize;
	XSetWMNormalHints(display, seeing.win, &hints);

	/* we need to listen for new data */
	XSelectInput(display, seeing.win, ExposureMask | StructureNotifyMask |
		ButtonPressMask | KeyPressMask);

	return(SUCCEED);
}
