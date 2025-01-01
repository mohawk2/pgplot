/* The doseeing routine calculates the seeing (center of star and FWHM) at a */
/* given point and displays the values in a pop-up window. */

/* Sam Southard, Jr. */
/* Created: 27-Aug-1991 */
/*  3-Sep-1991	SNS/CIT	Modified to do nothing (waiting on an algorithm which */
/*			does not use NAG) */
/*  4-Sep-1991	SNS/CIT	Modified to use Jeff Hester's algorithm. */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/* 17-Sep-1991	SNS/CIT	Now takes image coordinate */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */

#define FWHM_GUESS	4	/* guess at number of pixels for FWHM */
#define XPIX		14	/* number of points to fit along x */
#define YPIX		14	/* number of points to fit along y */
#define AVGX		2	/* number of columns to average */
#define AVGY		2	/* number of rows to average */
#define SKYMETH		0	/* method for calculating sky.  0=constant, */
				/* 1=linear, 2=quadratic */

#include "figdisp.h"
#include "globals.h"

#include <stdio.h>
#include <string.h>

void doseeing(x, y)
int x,y;	/* the estimated x & y coordinates */
{
	char str[80];			/* A temporary string */

	double fwhm=FWHM_GUESS;
	double xc,yc,xfw,yfw;	/* the various outputs */

	void exposesee();

	if (x < 0 || x >= bm.imwidth || y < 0 || y >= bm.imheight) return;

	starfit(x, y, fwhm, XPIX, YPIX, AVGX, AVGY, SKYMETH, &xc, &yc, &xfw,
		&yfw);
	
	xc=(xc+bm.curxoff)*bm.curxsc;
	yc=(yc+bm.curyoff)*bm.curysc;
	xfw *= bm.curxsc;
	yfw *= bm.curysc;
	if (xfw < 0) xfw = -xfw;
	if (yfw < 0) yfw = -yfw;

	XFillRectangle(display, seeing.pixmap, textgcclear, 0, 0, seeing.width,
		seeing.height);

	(void)sprintf(str,"X Center: %7.1f Y Center: %7.1f", xc, yc);
	XDrawString(display, seeing.pixmap, textgc,
		res.textfont->max_bounds.width >> 1, (res.textfont->ascent
			+ res.textfont->descent >> 1) + res.textfont->ascent,
		&str[0], strlen(str));
	(void)sprintf(str,"X FWHM  : %7.1f Y FWHM  : %7.1f", xfw, yfw);
	XDrawString(display, seeing.pixmap, textgc,
		res.textfont->max_bounds.width >> 1, (res.textfont->ascent +
		res.textfont->descent >> 1) + (res.textfont->ascent << 1) +
		res.textfont->descent, &str[0], strlen(str));
	
	exposesee();
		
	return;
}
