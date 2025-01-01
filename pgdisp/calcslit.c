/* The calcslit routine takes image coordinates and returns two arrays, one */
/* of the data values on a line between two points (possibly averaging */
/* multiple points), and the other of the coordinates to use (may be X or Y, */
/* depending on the relationship between the two points).  The returned data */
/* values are already translated as appropriate (they are ready to be */
/* graphed).  After the graph is plotted the returned arrays should be */
/* free()ed.  The most efficient use of this routine will have xs <= xe.  If */
/* this is not the case, this routine will ahve to flip the data values. */

/* Return Values: */
/* 0	Everything's fine */
/* 1	Something went wrong and the data is invalid.  If this happens, the */
/*	pointers will not point to anything (so they should not be free()ed. */

/* Sam Southard, Jr. */
/* Created: 25-Jun-1992 (from doslit.c) */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>

int calcslit(xs,ys,xe,ye,dataptr,coordptr,numpoints)
int xs,ys;	/* The starting point for the graph */
int xe,ye;	/* The ending point for the graph */
double **dataptr;	/* The returned data array. */
double **coordptr;	/* The returned coordinates. */
int *numpoints;	/* The number of valid data points */
{
	int navg;	/* number of pixels to average in with central pix */
	int *avgx;	/* the relative X coords of the pixels to avg. in */
	int *avgy;	/* the relative Y coords of the pixels to avg. in */
	double *data;	/* The data values for the points */
	double *coords;	/* The coordinate values for the points. */
	int npts;	/* The number of points in the graph */
	int x,y;	/* The corods of the point we're on */
	int tpt;	/* The number of the point we're on */
	int i;		/* silly loop variable */
	int usey;	/* True if coords are Y, false if they're X */
	int count;	/* The number of points actually contributing */
	int x1,y1;	/* temporary coordinates */
	double dtmp;	/* A temporary double */

	/* The remaining variables are used for Bresenham's line drawing */
	/* algorithm.  If you want more details, look at the code and */
	/* compare it to an all-integer version of the algorithm */
	int inc;	/* The increment to use in Bresenham's alg. */
	int delx, dely;	/* the change from point 1 to point 2 */
	int dx,dy,de;	/* the amount to increment when x & y change */
	int e;		/* the error function */
	int nx,ny,ne;	/* amount to change when onle x or y changes */

	/* correct the pixel count (<0 is not meaningful) */
	if (lg.xzoom < 0) lg.xzoom=0;

	/* figure out how many pixels to average together */
	/* lg.xzoom is a power of two, and the central pixel does not count */
	if (lg.xzoom == 0) navg=0;
	else navg= (1 << lg.xzoom);

	/* for simplicity, we use ascending x (but we may flip it later) */
	if (xs > xe)
	{
		inc=xs;
		xs=xe;
		xe=inc;
		inc=ys;
		ys=ye;
		ye=inc;
		inc= -1;
	} else inc=1;

	delx=xe-xs;

	if (ye < ys) dely= ys - ye;
	else dely= ye - ys;

	if (delx < dely) npts=dely+1;
	else npts=delx+1;

	if (npts < 2) return(1);

	/* get space for the points. */
#ifdef lint
	data= &dtmp;
	if (malloc((unsigned)npts*sizeof(double)) == NULL)
#else
	if ((data=(double *)malloc(npts*sizeof(double))) == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(1);
	}

	/* get space for the coordinates. */
#ifdef lint
	coords= &dtmp;
	if (malloc((unsigned)npts*sizeof(double)) == NULL)
#else
	if ((coords=(double *)malloc(npts*sizeof(double))) == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		free((char *)data);
		return(1);
	}

	/* we may need to get space for other pixels */
	if (navg)
	{ /* we are using pixels in addition to the central pixel */
		/* get space for the relative x coordinates of the other */
		/* pixels (from the central pixel) */
#ifdef lint
		if (malloc((unsigned)navg*sizeof(int)) == NULL)
#else
		if ((avgx=(int *)malloc(navg*sizeof(int))) == NULL)
#endif
		{
			(void)fprintf(stderr,MSG_MALLOC);
			free((char *)data);
			free((char *)coords);
			return(1);
		}
		/* get space for the relative Y coordinates */
#ifdef lint
		if (malloc((unsigned)navg*sizeof(int)) == NULL)
#else
		if ((avgy=(int *)malloc(navg*sizeof(int))) == NULL)
#endif
		{
			(void)fprintf(stderr,MSG_MALLOC);
			free((char *)data);
			free((char *)coords);
			free((char *)avgx);
			return(1);
		}

		/* use bresenham's algorithm to get the line from the origin */
		/* which is perpendicular to a line of the given slope */

		/* the inverse */
		delx=ye-ys;
		dely=xe-xs;
		/* the negative */
		if (delx < 0) delx= -delx;
		else dely= -dely;

		if (dely > 0) dy=1;
		else if (dely < 0)
		{
			dely= -dely;
			dy= -1;
		} else dy=0;

		if (delx) dx=1;
		else dx=0;

		if (delx < dely)
		{
			npts=dely+1;
			nx=0;
			ny=dy;
			ne= delx<<1;
			e= (delx<<1) - dely;
			de= (delx << 1) - (dely << 1);
		} else {
			npts=delx+1;
			nx=dx;
			ny=0;
			ne= dely << 1;
			e= (dely << 1) - delx;
			de= (dely << 1) - (delx << 1);
		}
		x=y=0;
		/* we only need half the points.  Remember that navg is a */
		/* power of two. */
		for (tpt=0 ; tpt < navg/2 ; ++tpt)
		{
			if (e < 0)
			{
				x += nx;
				y += ny;
				e += ne;
			} else {
				x += dx;
				y += dy;
				e += de;
			}
			avgx[tpt]=x;
			avgy[tpt]=y;
		}
		/* Now just flip the sign to get the other side of the line */
		for (npts=0, tpt = navg/2 ; tpt < navg ; ++tpt, ++npts)
		{
			avgx[tpt] = -avgx[npts];
			avgy[tpt] = -avgy[npts];
		}
	}

	/* good old bresen */
	delx=xe-xs;
	dely=ye-ys;

	if (dely > 0) dy=1;
	else if (dely < 0)
	{
		dely= -dely;
		dy= -1;
	} else dy=0;

	if (delx) dx=1;
	else dx=0;

	/* figure out which coordinate to use for the graph (the one with the */
	/* most variance). */
	if (delx < dely)
	{
		npts=dely+1;
		nx=0;
		ny=dy;
		ne= delx<<1;
		e=(delx<<1)-dely;
		de= (delx<<1)-(dely<<1);
		usey=1;
	} else {
		npts=delx+1;
		nx=dx;
		ny=0;
		ne= dely<<1;
		e= (dely<<1)-delx;
		de=(dely<<1)-(delx<<1);
		usey=0;
	}

	x=xs;
	y=ys;

	/* we want to extract the first used x and y coordinate */
	ys=xs= -1;

	/* zero out the data array */
	for (i=0 ; i < npts ; )
		data[i++]=0.0;

	tpt=0;
	while (tpt < npts)
	{
		if (x >= bm.imwidth) break;
		if (x >= 0 && y >= 0 && y < bm.imheight)
		{
			/* we know that this will be the first valid x point */
			if (xs < 0) xs=x;
			if (ys < 0) ys=y;
			xe=x;
			ye=y;
			/* save this value */
			if (bppix == 16) data[tpt] = rimdat.b16[x+bm.imwidth*y];
			else data[tpt] = rimdat.b8[x+bm.imwidth*y];
			if (usey) coords[tpt]=y;
			else coords[tpt]=x;
			/* add any surrounding points. */
			count=1;
			for (i=0 ; i < navg ; ++i)
			{
				x1=x+avgx[i];
				y1=y+avgy[i];
				/* if outside image, continue to next point */
				if (x1 < 0 || x1 >= bm.imwidth || y1 < 0 ||
				    y1 >= bm.imheight) continue;
				if (bppix == 16) data[tpt] +=
					rimdat.b16[x1+bm.imwidth*y1];
				else data[tpt] += rimdat.b8[x1+bm.imwidth*y1];
				++count;
			}
			if (navg) data[tpt] /= count;
			++tpt;
		} else --npts;
		if (e < 0)
		{
			x += nx;
			y += ny;
			e += ne;
		} else {
			x += dx;
			y += dy;
			e += de;
		}
	}

	/* how many points did we actually use? */
	npts=tpt;

	/* we no longer need the arrays used to average */
	if (navg)
	{
		free((char *)avgx);
		free((char *)avgy);
	}

	/* we might not have enough points to use */
	if (npts < 2)
	{
		free((char *)data);
		free((char *)coords);
		return(1);
	}

	/* now we scale the coordinates and data values */
	for (i=0 ; i < npts ; ++i)
	{
		if (usey) coords[i]= (coords[i]+bm.curyoff)*bm.curysc;
		else coords[i]= (coords[i]+bm.curxoff)*bm.curxsc;
		data[i]= (data[i]*bm.dsc)+bm.doff;
	}

	/* did we flip the data before? */
	if (inc == -1)
	{
		for (i=0 ; i < (npts >> 1) ; ++i)
		{
			dtmp=coords[i];
			coords[i]=coords[npts-1-i];
			coords[npts-1-i]=dtmp;
			dtmp=data[i];
			data[i]=data[npts-1-i];
			data[npts-1-i]=dtmp;
		}
	}

	/* we've got everything we need, just return */
	*dataptr=data;
	*coordptr=coords;
	*numpoints=npts;
	return(0);
}
