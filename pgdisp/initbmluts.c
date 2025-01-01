/* The initluts routine initializes the LUTs for the bitmap graphics window */

/* Sam Southard, Jr. */
/* Created: 30-Jul-1991 (from initlgluts) */
/* Modification History: */
/* 31-Jul-1991	SNS/CIT	Routine scale down # of LUTS moved to updatebmluts */
/* 15-Aug-1991	SNS/CIT	No longer includes xvideo hooks */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells and usecells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/*  5-Mar-1992	SNS/CIT	Now works better on grayscale monitors */
/* 27-Jun-1992	SNS/CIT	Now uses a faster algorithm */

/* The X Window include files */
#include <X11/Xlib.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"

#define LUT_WRAP 1

void initbmluts()
{
	int i,j;			/* silly loop variables */
	int itmp;			/* a temporary integer */
	double scale;			/* scale factor */
	int lutent;

	void transluts();	/* take care of LUT mapping */

	/* first initialize the full table */

	scale=(double)0xFFFF/(double)(BM_COLORS-1);
	for (i=0 ; i < BM_COLORS; ++i)
	{
		allcells[i].red=i*scale;
		allcells[i].green=i*scale;
		allcells[i].blue=i*scale;
		if (bm.bw)
		{
			allcells[i].red=0.30*allcells[i].red+
				0.59*allcells[i].green+0.11*allcells[i].blue;
			allcells[i].green=allcells[i].blue=allcells[i].red;
		}
		lutent=((i*(bm.colors*LUT_WRAP-1))/(BM_COLORS-1) % bm.colors);
		allcells[i].pixel=bm.pix[lutent];
	}
		
	/* now set up the table small enough to fit in what we got */
	scale=(double)(BM_COLORS-1)/bm.colors;

	for (i=0, j=0 ; i < bm.colors ; ++i)
	{
		usecells[i].pixel=bm.pix[i];
		usecells[i].flags=DoRed | DoGreen | DoBlue;
	}

	/* now update the actual tables */
	transluts();

	return;
}
