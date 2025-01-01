/* The initbmwin routine initializes the patch window */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 8-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 10-Oct-1991	SNS/CIT	Window manager hints now set in initwmattr */
/* 22-Nov-1991	SNS/CIT	No longer sets the border pixel color */
/* 10-Apr-1992	SNS/CIT	Now sets hints to control how to resize the patch */
/*			window */

/* The system include files */
#include <stdio.h>

/* the X Window include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "figdisp.icon"

int initpatchwin()
{
	XSizeHints hints;

	void initwmattr();

	patch.xoff=patch.yoff=0;

	/* only resize by 8 char widths (7 numbers & a space) */
	hints.width_inc=8*res.textfont->max_bounds.width;
	/* only resize by a single line */
	hints.height_inc=res.textfont->ascent+res.textfont->descent;

	if (res.pgeo.w <= 0) patch.width=96*res.textfont->max_bounds.width;
	else patch.width=res.pgeo.w*hints.width_inc;
	if (res.pgeo.h <= 0)
		patch.height=12*(res.textfont->ascent+res.textfont->descent);
	else patch.height=res.pgeo.h*hints.height_inc;
	hints.width=patch.width;
	hints.height=patch.height;
	patch.win=XCreateSimpleWindow(display, RootWindow(display, screen),
		res.pgeo.x, res.pgeo.y, patch.width, patch.height, BORDER_WIDTH,
		CopyFromParent, WhitePixel(display,screen));
	
	/* set up the patch window icon */
	patch.icon=XCreateBitmapFromData(display, patch.win, figdisp_bits,
		figdisp_width, figdisp_height);

	/* set up the hints for the window manager */
	initwmattr(patch, "patch", "patch", &res.pgeo);

	/* try to control how we get resized */
	hints.flags=PResizeInc | USSize;
	XSetWMNormalHints(display, patch.win, &hints);

	/* listen for exposures, resizes, and button and key presses */
	XSelectInput(display, patch.win, ExposureMask | StructureNotifyMask |
		ButtonPressMask | KeyPressMask);

	return(SUCCEED);
}
