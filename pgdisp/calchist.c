/* The routines in this file calculate the histogram equalization data, */
/* store the results in the array histpix, and update allcells[].pixel as */
/* necessary.  Histpix has bm.colors entries.  Data values from */
/* histpix[i] through histpix[i+1]-1 (inclusive) are mapped onto the pixel */
/* usecells[i].pixel, which has RGB values from allcells[histpix[i]], */
/* unless the color map is being inverted, in which case the RGB values */
/* are taken from allcells[histpix[bm.colors-i-1]+1].  histpix[bm.colors] is */
/* defined to be either 256 or 65536, depending on bppix. */

/* Note that this algorithm will only work for integer data and memory */
/* requirements will make things difficult for anything more than 16-bits per */
/* pixel.  However, it is very fast. */

/* Sam Southard, Jr. */
/* Created: 24-Jun-1992 */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>

#define LUT_WRAP	1

void calchist()
{
	int counts[BM_COLORS];	/* The number of counts at each value */
	unsigned short *sptr;	/* A 16-bit pointer */
	unsigned char *cptr;	/* An 8-bit pointer */
	double cpercell;	/* number of counts per cell */
	int csofar;		/* number of counts for values smaller than */
				/* the current pixel value */
	int curval;		/* The current pixel value */
	int curcell;		/* The current color map entry */
	int i,j;		/* silly loop variables */
	int width,height;	/* size of the area for calculations */
	int max;		/* The maximum possible pixel value */
	int xoff,yoff;		/* The offset to the area used for histograms */
	int totcounts;		/* The total number of counts */
	int lutcount;		/* number of counts needed for a single LUT */
				/* entry */
	int oldlutcount;	/* The previous value of lutcount. */
	int nvals;		/* The number of pixel values */
	double cperval;		/* colors per pixel value. */
	int maxcount;		/* maximum number of pixels with same value */
	int maxloc;		/* The pixel value for maxcount */

	/* find out how much of the image we should use */
	if (res.histgeo.w <= 0 || res.histgeo.w > bm.imwidth) width=bm.imwidth;
	else width=res.histgeo.w;
	if (res.histgeo.h <= 0 || res.histgeo.h > bm.imheight)
		height=bm.imheight;
	else height=res.histgeo.h;

	/* find out the offsets into the image */
	if (height == bm.imheight) yoff=0;
	else yoff= ((bm.imheight - height) >> 1);
	if (width == bm.imwidth) xoff=0;
	else xoff= ((bm.imwidth - width) >> 1);

	if ((width*height)/(bm.colors*LUT_WRAP) < 1)
	{
		fprintf(stderr,MSG_SMALLHIST);
		usehist=0;
		return;
	}

	/* At first every pixel counts */
	totcounts=width*height;
	cpercell=((double)totcounts)/(bm.colors*LUT_WRAP);

	/* zero out the counts array */
	if (bppix == 16) max=65536;
	else max=256;

	for (i=0 ; i < max ; )
		counts[i++]=0;
	
	if (bppix == 16)
	{
		for (i=0 ; i < height ; ++i)
		{
			sptr=rimdat.b16+bm.imwidth*(i+yoff)+xoff;
			for (j=0 ; j < width ; ++j)
				++counts[*sptr++];
		}
	} else {
		for (i=0 ; i < height ; ++i)
		{
			cptr=rimdat.b8+bm.imwidth*(i+yoff)+xoff;
			for (j=0 ; j < width ; ++j)
				++counts[*cptr++];
		}
	}

	/* pixel value 0 always gets it's own LUT entry (so we have a nice */
	/* background value) */
	totcounts -= counts[0];

#ifdef BAR
	maxcount=counts[1];
	maxloc=1;
	for (i=2 ; i < max ; ++i)
	{
		if (counts[i] > maxcount)
		{
			maxcount=counts[i];
			maxloc=i;
		}
	}

#ifdef FOO
	/* now find the first pixel less than half maxcount */
	for (i=maxloc+1 ; i < max ; ++i)
		if (counts[i] < maxcount/2) break;
#else
#ifndef BAZ
	for (i=maxloc-1 ; i > 0 ; --i)
		if (counts[i] < maxcount/2) break;
#else
	i=maxloc;
#endif
#endif	
	
	/* OK, zero out all counts before i, if i's reasonable */
	if (i < max-1)
	{
		printf("maxloc is %d, counts is %d\n",maxloc,maxcount);
		printf("zeroing out before %d\n",i);
		--i;
		for ( ; i > 0 ; --i)
		{
			totcounts -= counts[i];
			counts[i]=0;
		}
	}
#endif

	/* find out how many cells have non-zero counts and reduce the */
	/* cells which have too many counts (so that no LUT entries are */
	/* wasted). */
	nvals=0;
	lutcount=totcounts/(bm.colors*LUT_WRAP-1);
	for (i=1 ; i < max ; ++i)
	{
		if (counts[i]) ++nvals;
		if (counts[i] > lutcount)
		{
			totcounts -= (counts[i]-lutcount);
			counts[i] = lutcount;
			lutcount=totcounts/(bm.colors*LUT_WRAP-1);
		}
	}

	/* OK, we've got more values than pixels, and some values had more */
	/* than enough pixels for a single LUT (so they may be allocated too */
	/* many), so let's divide the LUT entries up better */
	if (nvals >= bm.colors*LUT_WRAP &&
	    lutcount != width*height/(bm.colors*LUT_WRAP))
	{
		oldlutcount= -1;
		/* as long as we're changing, keep going */
		while (lutcount != oldlutcount && lutcount > 1)
		{
			oldlutcount=lutcount;
			for (i=1 ; i < max ; ++i)
			{
				if (counts[i] > lutcount)
				{
					/* chop this one down until it no */
					/* longer affects the average value */
					/* This is faster than looping */
					/* through the entire array again */
					j= -1;
					while (j != lutcount && lutcount > 1)
					{
						j=lutcount;
						totcounts -=
							(counts[i]-lutcount);
						counts[i] = lutcount;
						lutcount=totcounts/
							(bm.colors*LUT_WRAP-1);
					}
					if (lutcount <= 1) break;
				}
			}
		}
	}

	/* 0 stands alone for a nice background */
	/* remember that histpix[i] is the first pixel value to use for */
	/* color LUT entry i */
	histpix[0]=0;
	histpix[1]=1;

	if (nvals <= 1)
	{
		/* there's only one non-zero pixel value, so it's easy */
		for (i=1 ; i < bm.colors*LUT_WRAP - 1 ; )
			histpix[i++]=0;
		histpix[bm.colors*LUT_WRAP-1]=1;
	} else if (nvals < bm.colors*LUT_WRAP) {
		/* we've got more LUT than pixel values, but we should still */
		/* try to range over the entire color map (i.e., the unused */
		/* LUT entries should not all be bunched up at the beginning */
		/* or end of the color map) */
		/* set up the histpix array to something easily checkable */
		for (i=2 ; i < bm.colors*LUT_WRAP ; )
			histpix[i++]= -1;

		cperval= ((double)(bm.colors*LUT_WRAP - 2)/(nvals-1));
		for (i=0, j=1 ; i < nvals, j < max ; ++i)
		{
			/* consume the non-0 values */
			while (!counts[j])
				++j;
			if (i*cperval > bm.colors*LUT_WRAP - 2)
			{
				histpix[bm.colors*LUT_WRAP-1]=j;
				break;
			} else histpix[1+(int)(i*cperval)]=j++;
		}
		/* now we set the histpix values which didn't get set before */
		for (i=2 ; i < bm.colors*LUT_WRAP ; ++i)
			if (histpix[i] == -1) histpix[i]=histpix[i-1];
	} else {
		/* cpercell is double version of lutcount */
		cpercell= ((double)totcounts/(bm.colors*LUT_WRAP - 2));
		/* whenever the total number of counts so far exceeds */
		/* the current cell times cpercell we should go to the next */
		/* LUT cell. */
		for (curcell=1, csofar=0, curval=0 ;
		     curcell < bm.colors*LUT_WRAP-1 ; )
		{
			while (curcell*cpercell >= csofar && curval < max-1)
				csofar += counts[++curval];
			/* we're now at the cell which put us over the limit */
			/* or at the maximum cell */
			if (curval == max)
			{
				while (curcell < bm.colors*LUT_WRAP-1)
					histpix[++curcell]=curval;
			} else histpix[++curcell]=curval;
		}
	}

	goodhist=1;
	return;
}
