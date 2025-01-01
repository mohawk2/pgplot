/* The labelyaxis routine labels the Y axis of a line plot.  It sets xleft */
/* to the leftmost X11 pixel which can be used to plot data. */

/* Sam Southard, Jr. */
/* Created: 26-Jun-1992 */

#include "figdisp.h"
#include "globals.h"

#define MIN_TICKS	5	/* The minimum number of ticks */

void labelyaxis(dmin, dmax, ybot, ytop, xleft)
double dmin;	/* The minimum data point */
double dmax;	/* The maximum data point */
int ybot,ytop;	/* The range of Y values we can use */
int *xleft;	/* The leftmost point we can use for data */
{
	int nticks;	/* The number of ticks we'll need */
	int font_height;	/* The height of the labling font */
	int prec;	/* The precision we need. */
	int lwid;	/* The maximum label width */
	double scale;	/* scale from tick number to pixel number */
	double dscale;	/* scale from tick number to data value */
	char label[100];	/* The label for the points. */
	int i;		/* silly loop variable */

	font_height=res.textfont->ascent+res.textfont->descent;

	/* Put tick marks every other line */
	nticks=(ybot-ytop+1)/(2*font_height) + 1;
	/* don't use too many ticks */
	if (nticks > dmax-dmin) nticks=dmax-dmin;
	/* don't use too few. */
	if (nticks < MIN_TICKS) nticks=MIN_TICKS;

	prec=getprec(dmin, dmin+(dmax-dmin)/(nticks+1));

	/* find out how big the labels will be, assuming that the label for */
	/* either dmin or dmax will be the largest */
	(void)sprintf(label,"%.*f", prec, dmin);
	lwid = XTextWidth(res.textfont, label, strlen(label));
	(void)sprintf(label,"%.*f", prec, dmax);
	if (XTextWidth(res.textfont, label, strlen(label)) > lwid)
		lwid=XTextWidth(res.textfont, label, strlen(label));
	
	/* Leave space for the label, two more characters, and half a tick */
	*xleft=lwid+2*res.textfont->max_bounds.width+HALF_TICK;

	/* from tick number to X11 pixel number */
	scale= ((double)(ybot-ytop+1))/nticks;
	/* from tick number to data value */
	dscale= (dmax-dmin)/nticks;

	/* now do the actual labling for the ticks */
	for (i=0 ; i <= nticks ; ++i)
	{
		/* make the tick */
		XDrawLine(display, lg.pixmap, linegc, *xleft-HALF_TICK,
			(int)(ybot - i*scale), *xleft+HALF_TICK,
			(int)(ybot - i*scale));
		
		/* label the tick */
		(void)sprintf(label, "%.*f", prec, dmin+dscale*i);
		XDrawString(display, lg.pixmap, linegc,
			2*res.textfont->max_bounds.width-1,
			(int)(ybot - i*scale + res.textfont->ascent/2), label,
			strlen(label));
	}

	/* Now a simple axis label */
	XDrawString(display, lg.pixmap, linegc, 1,
		(int)lg.height/2 - 2*font_height, "D", 1);
	XDrawString(display, lg.pixmap, linegc, 1,
		(int)lg.height/2 - font_height, "a", 1);
	XDrawString(display, lg.pixmap, linegc, 1, (int)lg.height/2, "t", 1);
	XDrawString(display, lg.pixmap, linegc, 1,
		(int)lg.height/2 + font_height, "a", 1);
	
	return;
}
