/* The labelplot routine labels the plot of a line of data.  Only the plot */
/* title is produced (see labelyaxis() and labelxaxis()).  It returns the */
/* minimum Y coordinate (in pixmap space) which can be used for data. */

/* Sam Southard, Jr. */
/* Created: 26-Jun-1992 (from doslit.c) */

#include "figdisp.h"
#include "globals.h"

#include <math.h>

void labelplot(xs, ys, xe, ye, ytop)
int xs, ys;	/* The starting point */
int xe, ye;	/* The ending point */
int *ytop;	/* The minimum Y coordinate for the data graph */
{
	int navg;		/* The number of pixels averaged together */
	double dtmp,dtmp2;	/* temporary doubles */
	int prec;		/* The precision necessary for this print */
	char label[100];	/* The plot label */
	int font_height;	/* The height of the line graphics font */
	double slope;		/* The slope of the line */

	double fabs();

	if (lg.xzoom <= 0) navg=0;
	else navg= (1 << lg.xzoom);

	if (xe == xs)
	{
		dtmp=(xs+bm.curxoff)*bm.curxsc;
		prec=getprec(dtmp-(navg/2)*fabs(bm.curxsc),
			dtmp+(navg/2)*fabs(bm.curxsc));
		if (navg) (void)sprintf(label,
			"Column Plot, Average of Columns %.*f to %*.f", prec,
			dtmp-(navg/2)*fabs(bm.curxsc), prec,
			dtmp+(navg/2)*fabs(bm.curxsc));
		else (void)sprintf(label,"Column Plot, X=%g", dtmp);
	} else if (ye == ys) {
		dtmp=(ys+bm.curyoff)*bm.curysc;
		prec=getprec(dtmp-(navg/2)*fabs(bm.curysc),
			dtmp+(navg/2)*fabs(bm.curysc));
		if (navg) (void)sprintf(label,
			"Row Plot, Average of Rows %.*f to %.*f", prec,
			dtmp-(navg/2)*fabs(bm.curysc), prec,
			dtmp+(navg/2)*fabs(bm.curysc));
		else (void)sprintf(label,"Row Plot, Y=%g",dtmp);
	} else {
		dtmp=(xs+bm.curxoff)*bm.curxsc;
		dtmp2=(ys+bm.curyoff)*bm.curysc;
		/* The Y coordinates are reversed so the slope is right */
		slope= (ys-ye)*fabs(bm.curysc/bm.curxsc)/(xe-xs);
		if (navg) (void)sprintf(label,
				"Starting X: %g, Starting Y: %g, Slope: %g, \
Average of %d Pixels",
				dtmp, dtmp2, slope, navg+1);
		else (void)sprintf(label,
			"Starting X: %g, Starting Y: %g, Slope: %g", dtmp,
			dtmp, dtmp2);
	}

	font_height=res.textfont->ascent+res.textfont->descent;

	XDrawString(display, lg.pixmap, linegc,
		((int)lg.width - XTextWidth(res.textfont, label,
		strlen(label)))/2, font_height, label, strlen(label));
	
	*ytop=2*font_height;

	return;
}
