/* The labelyaxis routine labels the X axis of a line plot.  Note that the */
/* coordinates are assumed to be evenly spaced (which will always be true */
/* for a line plot, but we may need to re-write this routine if we ever */
/* want to plto arbitrary data). */

/* Sam Southard, Jr. */
/* Created: 26-Jun-1992 */

#include "figdisp.h"
#include "globals.h"

#define MIN_TICKS	5	/* The minimum number of ticks */

#include <math.h>

void labelxaxis(cmin, cmax, usey, xleft, xright, ybot)
double cmin;	/* The leftmost coordinate */
double cmax;	/* The rightmost coordinate */
int usey;	/* True if these are Y coordinates, false if X */
int xleft;	/* The leftmost point we can use for data */
int xright;	/* The rightmost point we can use for data */
int ybot;	/* The bottom pixel we can use for data */
{
	int nticks;	/* The number of ticks we'll need */
	int font_height;	/* The height of the labling font */
	int prec;	/* The precision we need. */
	int lwid;	/* The maximum label width */
	int dprec;	/* The decimal precision needed */
	double scale;	/* scale from tick number to pixel number */
	double dscale;	/* scale from tick number to data value */
	char label[100];	/* The label for the points. */
	int i;		/* silly loop variable */

	double fabs();

	prec=0;

	/* find out how many ticks to use.  This is awkward because the */
	/* number of ticks affects the necessary precision, and the */
	/* necessary precision could affect the number of ticks possible */
	/* If no decimal places are needed we may break out of this loop */
	for (i=0 ; i < 2 ; ++i)
	{
		/* first we try to use integer labels (prec starts as 0) */
		(void)sprintf(label, "%.*f", prec, cmin);
		lwid = XTextWidth(res.textfont, label, strlen(label));
		(void)sprintf(label, "%.*f", prec, cmax);
		if (XTextWidth(res.textfont, label, strlen(label)) > lwid)
			lwid = XTextWidth(res.textfont, label, strlen(label));
		
		nticks=(xright-xleft+1)/(2*lwid) +1;
		/* don't use too many ticks */
		if (nticks > fabs(cmax-cmin)) nticks=fabs(cmax-cmin);
		/* then again, we should have a minimum */
		if (nticks < MIN_TICKS) nticks=MIN_TICKS;

		/* do we need to use floating-point numbers? */
		if (!prec)
		{
			dprec=getprec(cmin, cmin+(cmax-cmin)/(nticks+1));
			/* no decimals is fine */
			if (!dprec) break;
			prec=dprec;
		}
	}

	/* scale gets us from tick number to X11 pixel number */
	scale=(double)(xright-xleft+1)/nticks;
	/* dscale gets use from tick number to coordinate value */
	dscale=(cmax-cmin)/nticks;

	font_height=res.textfont->ascent+res.textfont->descent;

	/* now label the ticks */
	for (i=0 ; i <= nticks ; ++i)
	{
		/* make the tick */
		XDrawLine(display, lg.pixmap, linegc, (int)(xleft+i*scale),
			ybot+HALF_TICK, (int)(xleft+i*scale), ybot-HALF_TICK);
		
		/* label the tick */
		(void)sprintf(label, "%.*f", prec, cmin+i*dscale);
		XDrawString(display, lg.pixmap, linegc, (int)(xleft+i*scale
			-XTextWidth(res.textfont, label, strlen(label))/2),
			(int)lg.height-font_height, label, strlen(label));
	}

	/* Now a simple axis label */
	if (usey) (void)strcpy(label,"Row");
	else (void)strcpy(label,"Column");
	XDrawString(display, lg.pixmap, linegc,
		((int)lg.width - XTextWidth(res.textfont, label,
		strlen(label)))/2, (int)lg.height-2, label, strlen(label));
	
	return;
}
