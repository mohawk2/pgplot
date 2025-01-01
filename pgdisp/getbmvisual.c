/* The getbmvisual routine gets a visual and allocates colors for the bitmap */
/* graphics window */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 9-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT	No longer attempts to allocate more than the maximum */
/*			number of colors in a colomap.  Now tries to copy */
/*			some color map entries from the default color map to */
/*			a private color map. */
/* 31-Jan-1992	SNS/CIT	Now deals with the leavecolors resource. */

/* The system include files */
#include <stdio.h>
#include <malloc.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

int getbmvisual()
{
	XVisualInfo vTemplate;		/* The template for our visual */
	int newmap;			/* do we need our own color map */
	unsigned long pmtmp[1];	/* temporary for plane masks */
	XColor color;		/* An X colorcell entry */
	int i;			/* silly loop variable */

	char *malloc();

#ifdef VMS
	/* I don't know how to figure it out in R3 */
	bitdepth=8;
#else
	/* first try to find a PseudoColor visual */
	vTemplate.screen=screen;
	vTemplate.class=PseudoColor;
	vTemplate.depth=8;
	bm.bw=0;		/* hope it's not black & white */

	if (!XMatchVisualInfo(display, screen, 8, PseudoColor, &vTemplate))
	{
		if (!XMatchVisualInfo(display, screen, 8, GrayScale,
			&vTemplate))
		{
			(void)fprintf(stderr,MSG_NOCOLORS);
			return(FAIL);
		}
	}

#ifdef lint
	if (malloc(BM_COLORS*sizeof(unsigned long)) == NULL)
#else
	if ((bm.pix=(unsigned long *)malloc(BM_COLORS*sizeof(unsigned long)))
	    == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}
	newmap=1;

	/* make sure we don't get into trouble trying to allocate too many */
	/* colors */
	if (res.maxcolors+res.leavecolors > vTemplate.visual->map_entries)
		res.maxcolors = vTemplate.visual->map_entries-res.leavecolors;
	if (res.maxpcolors > vTemplate.visual->map_entries)
		res.maxpcolors = vTemplate.visual->map_entries;

	if (DefaultVisual(display, screen) == vTemplate.visual)
	{ /* we may be able to share the color map */
		bm.colors=res.maxcolors;
		newmap=0;
		bitcmap=DefaultColormap(display, screen);
		while (bm.colors >= res.mincolors)
		{
			if (XAllocColorCells(display, bitcmap, True, &pmtmp[0],
				0, bm.pix,
				(unsigned)(bm.colors+res.leavecolors))) break;
			--bm.colors;
		}
		if (bm.colors < res.mincolors) newmap=1;
		else {
			/* we actually got res.leavecolors extra colors, so */
			/* free them up and get only what we need. */
			if (res.leavecolors > 0)
			    	XFreeColors(display, bitcmap, bm.pix+bm.colors,
					res.leavecolors, 0);
			printf("Got %d colors in default color map\n",
				bm.colors);
		}
	}

	if (newmap)
	{
		bitcmap=XCreateColormap(display, RootWindow(display, screen),
			vTemplate.visual, AllocNone);

		bm.colors=res.maxpcolors-res.savecolors;
		while (bm.colors >= res.minpcolors)
		{
			if (!XAllocColorCells(display, bitcmap, True,
				&pmtmp[0], 0, bm.pix,
				(unsigned)bm.colors+res.savecolors)) break;
			--bm.colors;
		}
		if (bm.colors < res.minpcolors)
		{
			(void)fprintf(stderr,MSG_NOCOLORS);
			return(FAIL);
		}
		/* Now copy the first few colormap entries from the default */
		for (i=0 ; i < res.savecolors ; ++i)
		{
			color.pixel=bm.pix[i];
			XQueryColor(display, DefaultColormap(display,screen),
				&color);
			color.flags= DoRed | DoGreen | DoBlue;
			XStoreColor(display, bitcmap, &color);
		}
		/* Now update the pixel table so we forget about the first */
		/* few */
		for (i=0 ; i < bm.colors ; ++i)
			bm.pix[i]=bm.pix[i+res.savecolors];

		(void)printf("Got %d colors in private color map\n",bm.colors);
	}
		
	bitvisual=vTemplate.visual;
	bitdepth=8;
#endif
	return(SUCCEED);
}
