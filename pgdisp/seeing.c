/* The seeing routine calculates the seeing at the given point.  It is simply */
/* a translation of the SEEING1 routine.  The parameter mappings from a call */
/* to SEEING1 are as follows: */
/* SEEING1(PIX, NX, NY, XCENTER, YCENTER, VM, XSEE, YSEE, STATUS) */
/* PIX = rimdat */
/* NX = bm.imwidth */
/* NY = bm.imheight */
/* XCENTER = xc */
/* YCENTER = yc */
/* VM = max */
/* XSEE = xsee */
/* YSEE = ysee */
/* STATUS = return value of seeing1 */
/* All variables local to the SEEING1 routine have the same name and */
/* function.  The comments from the original routine are retained. */
/* Return Values: */
/* 0		OK */
/* 1		Star is too close to the edge */
/* 2		Negative sky */
/* other	unable to calculate seeing value */

/* Sam Southard, Jr. */
/* Created: 28-Aug-1991 (From SEEING1) */
/* Modification History: */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */

#include "figdisp.h"
#include "globals.h"

#include <math.h>

/* A trivial macro */
#define min(x,y)	((x) > (y) ? (y) : (x))

#define RSKY	5
#define RADIUS	(RSKY*RSKY+1)
#define SKYBOX	40
#define IMAGEBOX 20

seeing1(x, y, max, xsee, ysee)
double xc,yc;	/* The coordinates of the center */
double *max, *xsee, *ysee;	/* the desired values */
{
	int jlast, rsky, radius, xc, yc, i, j, jsize, jindex, jj, ndata, k;
	int mdata, nstat;
	double delta, kmax, x[4*RSKY*RSKY+1], y[4*RSKY*RSKY+1], sum1, sum2, sky;
	double h, med[81*81], xdata, ydata, del, dx[100], dy[100], w[100];
	double wk[300], w2[100], datax[100], datay[100], ycut[100], xcut[100];
	double maxbright

	double sqrt();
	double fabs();
	double qfmed();

	xc=xcenter;
	yc=ycenter;

	/* first find the sky and the intensity at the point of maximum */
	/* brightness (max) */

	if (bppix == 16) *max=rimdat.b16[xc+yc*bm.imwidth];
	else *max=rimdat.b8[xc+yc*bm.imwidth];

	if (bppix == 16)
	{
		for (i=xc-1 ; i <= xc+1 ; ++i)
			for (j=yc-1 ; j < yc+i ; ++j)
				if (rimdat.b16[i+j*bm.imwidth] > *max)
					*max=rimdat.b16[i+j*bm.imwidth];
	} else {
		for (i=xc-1 ; i <= xc+1 ; ++i)
			for (j=yc-1 ; j < yc+i ; ++j)
				if (rimdat.b8[i+j*bm.imwidth] > *max)
					*max=rimdat.b8[i+j*bm.imwidth];
	}

	/* error message if star too close to edge of frame */

	if (xc < SKYBOX || yc < SKYBOX || xc+SKYBOX >= bm.imwidth ||
		yc+SKYBOX >= bm.imheight) return(1);
	
	k=1;
	for (i=xc-SKYBOX, i <= xc+SKYBOX ; ++i)
	{
		if (i > xc-IMAGEBOX && i < xc+IMAGEBOX) continue;
		for (j=yc-SKYBOX ; j <= yc+SKYBOX; ++j)
		{
			if (j > yc-IMAGEBOX && j< yc+IMAGEBOX) continue;
			if (bppix == 16) med[k]=rimdat.b16[i+j*bm.imwidth];
			else med[k]=rimdat.b8[i+j*bm.imwidth];
			++k;
		}
	}

	sky=qfmed(med, k);
	maxbright= *vm-sky;

	/* routine will fail (log of negative number) if sky is negative */

	if (sky <= 0) return(2);

	/* Now subtract the sky and find proper profile out to RSKY.  Values */
	/* of dn-sky must exceed 3/4 of sky of 500 DN, whichever is smaller. */
	/* Fill y array with profile - this is cut in x.  It has the true */
	/* radial distance, and x runs from negative to positive through 0. */

	/* The weighting scheme adopted is that weights are 1 for intensities */
	/* more than 1/3 of maximum, and proportional to */
	/* intensity/max intensity in the wings of the seeing profile. */

	ndata=0;
	j=yc;
	delta=(ycenter-yc)*(ycenter-yc);
	del=min(500, sky*0.75);

	for (i=xc-IMAGEBOX ; i <= xc+IMAGEBOX ; ++i)
	{
		if (bppix == 16) ydata=rimdat.b16[i+j*bm.imwidth]-sky;
		else ydata=rimdat.b8[i+j*bm.imwidth]-sky;
		if (ydata < del) continue;
		xdata=sqrt((i-xcenter)*(i-xcenter)+delta);
		if (i <= xc) xdata = -xdata;
		if (fabs(xdata) > rsky) continue;
		y[ndata]=ydata;
		x[ndata]=xdata;
		w[ndata]=ydata/maxbright;
		if (ydata/maxbright > 0.333) w[ndata]=1;
		++ndata;
	}

	/* cut in y */

	mdata=0;
	i=xc;
	delta=(xcenter-xc)*(xcenter-xc);

	for (j=yc-IMAGEBOX ; j <= yc+IMAGEBOX ; ++j)
	{
		if (bppix == 16) ydata=rimdat.b16[i+j*bm.imwidth]-sky;
		else ydata=rimdat.b8[i+j*bm.imwidth]-sky;
		if (ydata < del) continue;
		xdata=sqrt((j-ycenter)*(j-ycenter)+delta);
		if (j <= yc) xdata = -xdata;
		if (fabs(xdata) > rsky) continue;
		ycut[mdata]=ydata;
		xcut[mdata]=xdata;
		w2[mdata]=ydata/maxbright;
		if (ydata/maxbright > 0.333) w2[mdata]=1;
		++mdata;
	}

	/* fill array for fitting routine - only need array with data.  logs */
	/* taken inside spfit2 itself */

	if (ndata)
	{
		spifit2(y, x, ndata, dx, dy, w, wk, h);
		*xsee = 2*h;
	}
