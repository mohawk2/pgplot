/* The initgc routine initializes the graphics contexts needed for the */
/* bitmap windows.  The bitmap and patch windows should be created before */
/* this routine is called. */

/* Sam Southard, Jr. */
/* Created: 10-Oct-1991 (from initbmwin) */

/* The program include files */
#include "figdisp.h"
#include "globals.h"

/* The system include files */
#include <stdio.h>

void initgc()
{
	XGCValues gcvals;		/* for setting the graphics context */

	/* first the main gcs */
	gcvals.foreground=BlackPixel(display,screen);
	bitgcclear=XCreateGC(display,bm.win,GCForeground,&gcvals);
	gcvals.background=bm.pix[0];
	gcvals.foreground=bm.pix[1];
	gcvals.fill_style=FillSolid;
	gcvals.fill_rule=WindingRule;
	bitgc=XCreateGC(display,bm.win,
		GCForeground|GCBackground|GCFillStyle|GCFillRule,&gcvals);
	
	/* now one to draw an XOR line */
	gcvals.background=bm.pix[0];
	gcvals.foreground=0xFFFFFFFF;
	gcvals.function=GXxor;
	xorgc=XCreateGC(display,bm.win,GCForeground|GCBackground|GCFunction,
		&gcvals);

	/* finally the text graphics context */
	textgc=XCreateGC(display, patch.win, (unsigned long) 0,
		(XGCValues *)NULL);
	XSetFont(display, textgc, res.textfont->fid);
	XSetForeground(display, textgc, BlackPixel(display,screen));
	XSetBackground(display, textgc, WhitePixel(display,screen));
	textgcclear=XCreateGC(display, patch.win, (unsigned long) 0,
		(XGCValues *)NULL);
	XSetFont(display, textgcclear, res.textfont->fid);
	XSetForeground(display, textgcclear, WhitePixel(display,screen));
	XSetBackground(display, textgcclear, BlackPixel(display,screen));

	return;
}
