/* The graphline routine takes a set of transformed data and coordinate */
/* values and graphs them in the line graphics window.  The plot is labeled */
/* appropriately, according to the given starting and ending points. */

/* Sam Southard, Jr. */
/* Created: 25-Jun-1992 (from doslit.c) */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>

void graphline(data,coord,npts,xs,ys,xe,ye)
double *data;	/* The data values (Y axis) */
double *coord;	/* The coordinate values (X axis) */
int npts;	/* The number of data points */
int xs,ys;	/* The starting point for the graph */
int xe,ye;	/* The ending point for the graph */
{
	int xleft,xright;	/* The X11 coordinates of the area to use for */
	int ybot,ytop;		/* graphing data. */
	double cscale, dscale;	/* The scaling factors to go from coordinates */
				/* and data values to X11 X & Y coords */
	double cmin;		/* The leftmost coordinate in the graph */
	double dtmp;		/* A temporary double */
	double dmax,dmin;	/* min & max data points */
	int i;			/* silly loop variable */
	int tpt;		/* The point we're on. */
	int maxpts;		/* The maximum points in a single transfer */
	XPoint *points;		/* The points to draw */

	/* Not enough points to be worth it. */
	if (xs == xe && ys == ye || npts < 3) return;

	/* get the memory for the points */
#ifdef lint
	points=(XPoint *)0;
	if (malloc((unsigned)npts*sizeof(XPoint)) == NULL)
#else
	/* histogram plots need twice the number of points (one per side) */
	if (res.plothist) points=(XPoint *)malloc(2*npts*sizeof(XPoint));
	else points=(XPoint *)malloc(npts*sizeof(XPoint));
	if (points == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return;
	}

	/* Determine the maximum and minimum data values */
	dmin=dmax=data[0];
	for (i=1 ; i < npts ; ++i)
	{
		if (dmin > data[i]) dmin=data[i];
		if (dmax < data[i]) dmax=data[i];
	}
	 /* leave a little space on each side */
	 if (dmax == dmin)
	 {
	 	--dmin;
	 	++dmax;
	 } else {
	 	dtmp=(dmax-dmin);
	 	dmin -= dtmp*0.05;
	 	dmax += dtmp*0.05;
	 }

	/* Clear the pixmap */
	XSetForeground(display, linegcclear, lg.pix[0]);
	XFillRectangle(display, lg.pixmap, linegcclear, 0, 0, lg.width,
		lg.height);

	/* Draw the main plot labels */
	labelplot(xs, ys, xe, ye, &ytop);

	/* leave enough room for a couple at the right-hand side */
	xright=lg.width-(2*res.textfont->max_bounds.width)-1;

	/* leave room for 2 lines of text and half a tick on the bottom */
	ybot=lg.height-2*(res.textfont->ascent+res.textfont->descent)-HALF_TICK
		-1;

	if (ytop >= ybot)
	{
		fprintf(stderr,MSG_LGWINTOOSMALL);
		return;
	}

	/* Draw the Y axis label */
	labelyaxis(dmin, dmax, ybot, ytop, &xleft);

	if (xright <= xleft)
	{
		fprintf(stderr,MSG_LGWINTOOSMALL);
		return;
	}

	/* Allow space for the histogram if necessary */
	cscale=coord[npts-1]-coord[0];
	cmin=coord[0];
	if (res.plothist)
	{
		cscale += (coord[npts-1]-coord[npts-2]+coord[1]-coord[0])/2;
		cmin -= (coord[1]-coord[0])/2;
		/* Draw the X axis label */
		labelxaxis(cmin, coord[npts-1]+(coord[npts-1]-coord[npts-2])/2,
			abs(ye-ys) > abs(xs-xe), xleft, xright, ybot);
	} else {
		/* Draw the X axis label */
		labelxaxis(coord[0], coord[npts-1], abs(ye-ys) > abs(xs-xe),
			xleft, xright, ybot);
	}

	cscale = (xright - xleft)/cscale;
	if (dmax == dmin)
	{
		if (dmin != 0) dscale=(lg.height>>1)/dmin;
		else dscale=1;
	} else dscale = (ybot - ytop)/(dmax - dmin);

	/* draw the border */
	points[0].x=xleft;
	points[0].y=ytop;
	points[1].x=xleft;
	points[1].y=ybot;
	points[2].x=xright;
	points[2].y=ybot;
	XDrawLines(display, lg.pixmap, linegc, points, 3, CoordModeOrigin);

	/* update the ouput X & Y cursor scaling values */
	if (cscale != 0) lg.curxsc= cscale;
	else lg.curxsc=1;
	lg.curxoff=xleft-cmin*lg.curxsc;
	if (dscale != 0) lg.curysc= dscale;
	else lg.curysc=1;
	lg.curyoff=lg.height-ybot-dmin*lg.curysc;

	maxpts=(XMaxRequestSize(display)-3) >> 1;
	if (res.plothist)
	{
		/* make sure we have an even number */
		if (maxpts & 0x1) maxpts &= ~0x1;

		/* we need the number of data points we can handle at once */
		if (maxpts > 2*npts) maxpts=npts;
		else maxpts /= 2;

		/* handle the first point separately */
		XDrawLine(display, lg.pixmap, linegc, (int)(xleft+
			(coord[0]-cmin-(coord[1]-coord[0])/2)*cscale),
			ybot, (int)(xleft+
			(coord[0]-cmin-(coord[1]-coord[0])/2)*cscale),
			(int)(ybot-(data[0]-dmin)*dscale));
		points[0].x=xleft+(coord[0]-cmin-(coord[1]-coord[0])/2)*cscale;
		points[0].y=ybot-(data[0]-dmin)*dscale;
		points[1].x=xleft+((coord[0]+coord[1])/2 - cmin)*cscale;
		points[1].y=ybot-(data[0]-dmin)*dscale;
		tpt=0;
		while (tpt < npts)
		{
			/* now we fill up the histogram.  The even points are */
			/* the left side of the histogram and the odd points */
			/* are the right side */
			if (tpt == 0) i=1;
			else i=0;
			for ( ; i < maxpts && tpt+i < npts-1 ; ++i)
			{
				points[2*i].x=xleft + ((coord[tpt+i-1]+
					coord[tpt+i])/2 - cmin)*cscale;
				points[2*i+1].x=xleft + ((coord[tpt+i+1]+
					coord[tpt+i])/2 - cmin)*cscale;
				points[2*i].y=points[2*i+1].y=
					ybot-(data[tpt+i]-dmin)*dscale;
			}
			XDrawLines(display, lg.pixmap, linegc, points, 2*i,
				CoordModeOrigin);
			tpt += maxpts;
		}
		/* now we take care of the last point */
		if (npts > 3)
		{
			/* only if we have enough room */
			points[0].x=points[1].x= xleft+
				((coord[npts-1]+coord[npts-2])/2 - cmin)*cscale;
			points[0].y=ybot-(data[npts-2]-dmin)*dscale;
			points[1].y=points[2].y=ybot-(data[npts-1]-dmin)*dscale;
			points[2].x=points[3].x=xleft+(coord[npts-1]-cmin+
				(coord[npts-1]-coord[npts-2])/2)*cscale;
			points[3].y=ybot;
			XDrawLines(display, lg.pixmap, linegc, points, 4,
				CoordModeOrigin);
		}
	} else {
		tpt=0;
		if (maxpts > npts) maxpts=npts;
		while (tpt < npts)
		{
			for (i = 0 ; i < maxpts && tpt+i < npts ; ++i)
			{
				points[i].x=xleft + (coord[tpt+i]-cmin)*cscale;
				points[i].y=ybot-(data[tpt+i]-dmin)*dscale;
			}
			XDrawLines(display, lg.pixmap, linegc, points, i,
				CoordModeOrigin);
			tpt += maxpts;
		}
	}
	
	/* now update the window */
	XClearWindow(display,lg.win);
	if (!lg.mapped)
	{
		XMapWindow(display,lg.win);
		XRaiseWindow(display,lg.win);
		lg.mapped=1;
	}
	XCopyArea(display, lg.pixmap, lg.win, linegc, 0, 0, lg.imwidth,
		lg.imheight, lg.winxoff, lg.winyoff);

	XFlush(display);
	return;
}
