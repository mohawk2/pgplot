/* The resizeim routine resizes the image */
/* Return Values: */
/* 0		Everything went fine */
/* MALLOC_ERR	We couldn't allocate space for the new image */

/* Sam Southard, Jr. */
/* Created: 1-Aug-1991 */
/*  2-Aug-1991	SNS/CIT	Now clears the image properly */
/* 22-Aug-1991	SNS/CIT	Now copies the old image correctly */
/* 23-Aug-1991	SNS/CIT	Now updates the location window */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/* 19-Sep-1991	SNS/CIT	No longer resets the zoom factor or recenters the */
/*			image */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	Usecells no longer in bm structure */
/* 16-Oct-1991	SNS/CIT	Renamed from fastresizeim to resizeim */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 10-Apr-1992	SNS/CIT	Now changes the location window's size hints. */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/* 24-Jun-1992	SNS/CIT	Now invalidates the histogram equalization data */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#ifndef VMS
#include <malloc.h>
#endif
#include <stdio.h>

/* A trivial macro */
#define min(x,y) (((x) > (y)) ? (y) : (x))

#define LUT_WRAP 1

int resizeim(x,y,bits)
int x;	/* the new width */
int y;	/* the new height */
int bits;	/* The number of bits per pixel */
{
	int docopy=1;	/* are we trying to save the old data? */
	unsigned short *newim16;	/* the new image data buffer */
	unsigned char *newim8;	/* the new image data buffer */
	int cx,cy;	/* amount of data to copy */
	int i,j;	/* silly loop variable */
	unsigned short *im16a;	/* temporary image pointer */
	unsigned short *im16b;	/* temporary image pointer */
	unsigned char *im8a;	/* temporary image pointer */
	unsigned char *im8b;	/* temporary image pointer */
	double scale;		/* a scaling factor */
	XSizeHints hints;

	char *malloc();
	void redrawim();	/* the routine to redraw the bitmap image */
	void checkoff();	/* check the offsets */
	void transluts();

	/* see if this is a NOP */
	if (x == bm.imwidth && y == bm.imheight && bppix == bits) return(0);

	if (x < 0 || y < 0)
	{
		(void)fprintf(stderr,"Invalid new size: %d %d\n",x,y);
		return(0);	/* just ignore it - we can use old image */
	}

	/* histogram data is no longer valid (the image data has changed) */
	goodhist=0;

	newim8=(unsigned char *)NULL;
	newim16=(unsigned short *)NULL;
#ifndef lint
	if (bits == 16) newim16=(unsigned short *)malloc(
		(unsigned)x*y*sizeof(unsigned short));
	else newim8=(unsigned char *)malloc((unsigned)x*y);
#endif
	while (newim8 == NULL && newim16 == NULL)
	{ /* chop at the image until we can fit it into memory */
		if (docopy)
		{ /* first try: discard old data */
			docopy=0;
			(void)fprintf(stderr,MSG_OLD_LOST);
			if (bppix == 16)
			{
				free((char *)rimdat.b16);
				rimdat.b16=NULL;
			} else {
				free((char *)rimdat.b8);
				rimdat.b8=NULL;
			}
		} else if (x > BM_MAX_WIDTH) { /* try slimming down width */
			(void)fprintf(stderr,MSG_CHOP_X);
			x=BM_MAX_WIDTH;
		} else if (y > BM_MAX_HEIGHT) { /* try slimming down height */
			(void)fprintf(stderr,MSG_CHOP_Y);
			y=BM_MAX_HEIGHT;
		} else if (x > BM_WIDTH) { /* try slimming down width */
			(void)fprintf(stderr,MSG_CHOP_X);
			x=BM_WIDTH;
		} else if (y > BM_HEIGHT) { /* try slimming down height */
			(void)fprintf(stderr,MSG_CHOP_Y);
			y=BM_HEIGHT;
		} else { /* nothing more we can do */
			(void)fprintf(stderr,MSG_MALLOC);
			return(MALLOC_ERR);
		}
		if (bits == 8 && newim8 == NULL)
			newim8=(unsigned char *)malloc((unsigned)x*y);
		else if (bits == 16 && newim16 == NULL)
#ifdef lint
			(void)malloc((unsigned)x*y*sizeof(unsigned short));
#else
			newim16=(unsigned short *)malloc(
				(unsigned)x*y*sizeof(unsigned short));
#endif
	}

	/* copy what data we can */
	if (docopy)
	{
		cx=min(bm.imwidth,x);
		cy=min(bm.imheight,y);
		if (bits == 8 && bppix == 8)
		{
			for (i=0 , im8a=newim8, im8b=rimdat.b8 ; i < cy ;
			     ++i, im8a += x, im8b += bm.imwidth)
				memcpy((char *)im8a, (char *)im8b, cx);
		} else if (bits == 16 && bppix == 16) {
			for (i=0 , im16a=newim16, im16b=rimdat.b16 ; i < cy ;
			     ++i, im16a += x, im16b += bm.imwidth)
				memcpy((char *)im16a, (char *)im16b,
					cx*sizeof(unsigned short));
		} else if (bits == 8 && bppix == 16) {
			for (i=0, im8a=newim8, im16b=rimdat.b16 ; i < cy ;
			     ++i, im8a += x, im16b += bm.imwidth)
			{
				for (j=0 ; j < cx ; ++j)
					im8a[j] = (im16b[j] >> 8);
			}
		} else { /* new must be 16 and old must be 8 */
			for (i=0, im16a=newim16, im8b=rimdat.b8 ; i < cy ;
			     ++i, im16a += x, im8b += bm.imwidth)
			{
				for (j=0 ; j < cx ; ++j)
					im16a[j] = (im8b[j] << 8);
			}
		}

		/* blank out pixels beyond the old image */
		if (bits == 16)
		{
			if (x > bm.imwidth)
			{
				im16a=newim16+bm.imwidth;
				for (i=0 ; i < cy ; ++i, im16a += x)
					for (j=0 ; j < x-bm.imwidth ; ++j)
						im16a[j]=0;
			}
			/* blank out any extra rows */
			if (y > bm.imheight)
				for (i=0, im16a=newim16+bm.imheight*x ;
				     i < x*(y-bm.imheight) ; ++i) *im16a++ = 0;
		} else {
			if (x > bm.imwidth)
			{
				im8a=newim8+bm.imwidth;
				for (i=0 ; i < cy ; ++i, im8a += x)
					for (j=0 ; j < x-bm.imwidth ; ++j)
						im8a[j]=0;
			}
			/* blank out any extra rows */
			if (y > bm.imheight)
				for (i=0, im8a=newim8+bm.imheight*x ;
				     i < x*(y-bm.imheight) ; ++i) *im8a++ = 0;
		}
	} else { /* just blank out all data */
		if (bits == 16)
			for (i=0, im16a=newim16 ; i < x*y ; ++i)
				*im16a++ = 0;
		else for (i=0, im8a=newim8 ; i < x*y ; ++i) *im8a++ = 0;
	}
		
	if (bppix == 16) if (rimdat.b16 != NULL) free((char *)rimdat.b16);
	else if (rimdat.b8 != NULL) free((char *)rimdat.b8);

	if (bits == 16) rimdat.b16=newim16;
	else rimdat.b8=newim8;

	bm.imwidth=x;
	bm.imheight=y;

	/* take care of the location window's size hints */
	hints.max_width=bm.imwidth/2+2*BLANK_WIDTH;
	hints.max_height=bm.imheight/2+2*BLANK_WIDTH;
	hints.flags=PMaxSize;
	if (res.forcesquare)
	{
		hints.min_aspect.x=bm.imwidth/2;
		hints.min_aspect.y=bm.imheight/2+2*BLANK_WIDTH;
		hints.max_aspect.x=bm.imwidth/2+2*BLANK_WIDTH;
		hints.max_aspect.x=bm.imheight/2;
		hints.flags |= PAspect;
	}
	XSetWMNormalHints(display, loc.win, &hints);

	/* now take care of any LUT changes necessary. */
	if (bits == 8 && bppix == 16)
	{
		for (i=1 ; i < 256 ; ++i)
		{
			allcells[i].red=allcells[i << 8].red;
			allcells[i].green=allcells[i << 8].green;
			allcells[i].blue=allcells[i << 8].blue;
			allcells[i].pixel=allcells[i << 8].pixel;
		}
	} else if (bits == 16 && bppix == 8) {
		for (i=0 ; i < 256 ; ++i)
		{
			for (j=0 ; j < 256 ; ++j)
			{
				allcells[(i << 8)+j].red=allcells[i].red;
				allcells[(i << 8)+j].green=allcells[i].green;
				allcells[(i << 8)+j].blue=allcells[i].blue;
				allcells[(i << 8)+j].pixel= -1;
			}
		}
		scale=(double)(BM_COLORS-1)/(double)(bm.colors*LUT_WRAP-1);
		for (i=0 ; i < bm.colors*LUT_WRAP ; ++i)
			allcells[(int)(i*scale)].pixel=bm.pix[i%bm.colors];

		/* now go back and finish the mapping */
		for (i=1 ; i < BM_COLORS ; ++i)
			if (allcells[i].pixel == -1)
				allcells[i].pixel=allcells[i-1].pixel;
	}

	if (bits != bppix)
	{
		if (bits == 16) bm.slope= 65535/(double)(bm.colors-1);
		else bm.slope= 255/(double)(bm.colors-1);
		bppix=bits;
		transluts();
	} else bppix=bits;

	checkoff();

	/* re-draws thing with the new window */
	XFillRectangle(display, bm.win, bitgcclear, 0, 0, bm.width, bm.height);
	redrawim(display_to_imagecol(0), display_to_imagerow(0),
		display_to_imagecol((int)bm.width),
		display_to_imagerow((int)bm.height));

	(void)updateloc();

	return(0);
}
