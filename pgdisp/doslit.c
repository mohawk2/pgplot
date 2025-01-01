/* The doslit routine takes image coordinates and produces a graph of the */
/* data values between the two points, which it displays in the line graphics */
/* window. */

/* Sam Southard, Jr. */
/* Created: 1-Oct-1991 */
/*  2-Oct-1991	SNS/CIT	Now leaves a blank border around the graph and draws */
/*			axis lines */
/*  3-Oct-1991	SNS/CIT	Now draws all labels and ticks.  Now knows about */
/*			multiple-pixel averaging.  Data values changed to */
/*			double to make pixel averaging better. */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 25-Nov-1991	SNS/CIT	Modified to deal with separate X & Y zoom factors, */
/*			the need for more than 1 decimal place in labels, and */
/*			flipping the graph so that it's always from left to */
/*			right if that's what the user wants. */
/* 27-Nov-1991	SNS/CIT	Now handles res.ascendcoord and !res.ascendx && */
/*			!res.rowltor as meaning to make row plots go from */
/*			right to left */
/* 14-Feb-1992	SNS/CIT	Now clears the line graphics window to lg.pix[0] */
/*			instead of BlackPixel */
/*  6-Apr-1992	SNS/CIT	Now updates the line graphics cursor scaling values */
/* 10-Apr-1992	SNS/CIT	Now handles winxoff & winyoff */
/* 14-Apr-1992	SNS/CIT	Now works on VMS. */
/* 25-Jun-1992	SNS/CIT	Now gets and plots the data in separate subroutines */

#include "figdisp.h"
#include "globals.h"

void doslit(xs,ys,xe,ye)
int xs,ys;	/* The starting point for the graph */
int xe,ye;	/* The ending point for the graph */
{
	double *data;	/* The data array for this line */
	double *coord;	/* The coordinate array for this line */
	int npts;	/* The number of points in the data array */
	int usey;	/* True if coordinates are Y coords, false otherwise */
	int flip;	/* True if we need to reverse the order of the points */
	int i;		/* silly loop variable */
	double dtmp;	/* A temporary double */

	/* This is a point, not a line. */
	if (xs == xe && ys == ye) return;

	/* get the data */
	if (calcslit(xs, ys, xe, ye, &data, &coord, &npts)) return;

	/* Are we using the Y coordinates for the line plots? */
	if (abs(ye-ys) > abs(xe-xs)) usey=1;
	else usey=0;

	/* The user may want us to flip the plot to a specific order. */
	flip=0;
	/* flip a row plot? */
	if (ys == ye &&
	    (res.ascendx && coord[0] > coord[1] || 
	     !res.ascendx && res.rowltor && xs > xe ||
	     !res.ascendx && !res.rowltor && xe > xs)) flip=1;
	/* flip a column plot? */
	if (xs == xe &&
	    (res.ascendy && coord[0] > coord[1] ||
	     !res.ascendy && res.bottotop && ys < ye ||
	     !res.ascendy && !res.bottotop && ys > ye)) flip=1;
	/* flip an arbitrary line plot */
	if ((usey && xs != xe || !usey && ys != ye) &&
	    (res.ascendcoord && coord[0] > coord[1] ||
	    !res.ascendcoord && res.lefttoright && xs < xe)) flip=1;
	
	if (flip)
	{
		for (i=0 ; i < (npts >> 1) ; ++i)
		{
			dtmp=coord[i];
			coord[i]=coord[npts-i-1];
			coord[npts-i-1]=dtmp;
			dtmp=data[i];
			data[i]=data[npts-i-1];
			data[npts-i-1]=dtmp;
		}
		flip=xs;
		xs=xe;
		xe=flip;
		flip=ys;
		ys=ye;
		ye=flip;
	}

	/* now graph the points */
	graphline(data,coord,npts,xs,ys,xe,ye);

	/* free up the data and return */
	free((char *)coord);
	free((char *)data);
	return;
}
