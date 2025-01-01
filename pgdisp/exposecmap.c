/* The exposecmap routine handles an expose event on the color map window.  */

/* Sam Southard, Jr. */
/* Created: 29-Mar-1991 (from figdisp/exposewin) */
/* Modification History: */
/*  2-Apr-1991	SNS/CIT	Pixmap and window are now the same size. */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */

/* The program include files */
#include "figdisp.h"
#include "globals.h"

void exposecmap(event)
XExposeEvent event;
{
	/* It's really quite simple */

	XPutImage(display, cwin.win, bitgc, cwin.image, event.x, event.y,
		event.x, event.y, (unsigned)event.width,
		(unsigned)event.height);

	return;
}
