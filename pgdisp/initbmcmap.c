/* The initbmcmap routine initializes color map used by the bitmap graphics */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 9-Oct-1991 (from initbmwin) */
/* Modification History: */
/* 14-Oct-1991	SNS/CIT	Usecells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 14-Apr-1992	SNS/CIT	Removed unnecessary declaration of sprintf */
/* 24-Jun-1992	SNS/CIT	Now initializes variables for histogram equalization */

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

/* The system include files */
#include <stdio.h>

#define LUT_WRAP 1

#ifndef UNBUGGY
static XColor *savecells=NULL;		/* color cells before we touch them */
#endif

int initbmcmap()
{
	int i;		/* silly loop variable */

	void initbmluts();	/* initialize the LUTs */
	char *malloc();

#ifdef lint
	if (malloc((unsigned)bm.colors*sizeof(XColor)) == NULL)
#else
	if ((usecells=(XColor *)malloc((unsigned)bm.colors*sizeof(XColor)))
	    == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}
#ifndef UNBUGGY
	/* get memory for the saved colors */
#ifdef lint
	if (malloc((unsigned)bm.colors*sizeof(XColor)) != NULL)
#else
	if ((savecells=(XColor *)malloc((unsigned)bm.colors*sizeof(XColor)))
	    != NULL)
#endif
	{
		for (i=0 ; i < bm.colors ; ++i) savecells[i].pixel=bm.pix[i];
		XQueryColors(display, bitcmap, savecells, bm.colors);
	}
#endif

	/* the histogram equalization is not yet valid and is not being used */
	usehist=goodhist=0;

	/* get one red entry for lines in the location window */
	locline.red=0xFFFF;
	locline.green=locline.blue=0;
	/* if we can't get our own, use the top one of the normal area */
	if (!XAllocColor(display, bitcmap, &locline))
	{
		locline.pixel= bm.pix[bm.colors-1];
		locline.flags = DoRed | DoGreen | DoBlue;
		XStoreColor(display, bitcmap, &locline);
		--bm.colors;
	}

	/* get memory for the histogram equaliztion array */
#ifdef lint
	if (malloc((unsigned)bm.colors*sizeof(int)) == NULL)
#else
	if ((histpix=(int *)malloc((unsigned)bm.colors*LUT_WRAP*sizeof(int)))
	    == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}

	return(SUCCEED);
}

#ifndef UNBUGGY
/* The restorecolors routine is necessary because OpenWindows seems to simply */
/* use a color map entry when it exits, without setting it to a known value. */
/* It simply restores the colors to what they were before this program did */
/* anything. */
void restorecolors()
{
	if (savecells != NULL)
		XStoreColors(display,bitcmap,savecells,bm.colors);
	return;
}
#endif
