/* The initbmdata routine initializes the bitmap graphics data to a test */
/* pattern */

/* Sam Southard, Jr. */
/* Created: 9-Oct-1991 (from initbmwin) */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS. */

/* The program include files */
#include "figdisp.h"
#include "globals.h"

void initbmdata()
{
	int j,k;	/* loop variables */
	int pval;	/* the value of this pixel */
	double scale;	/* to make the ramp fit across the display */
	double yscale;	/* to ramp it down the display */

	scale=(double)(BM_COLORS-1)/(bm.width-1);
	yscale=(double)(2*BM_COLORS-2)/(bm.height-1);
	for (j=0 ; j < bm.width; ++j)
	{
		for (k=0; k < bm.height ; ++k)
		{
			pval=(int)(k*yscale+j*scale) & 0xFFFF;
			rimdat.b16[j+k*bm.width]=pval;
		}
	}

	scale=(double)(BM_COLORS-1)/(cwin.width-1);
	for (j=0 ; j < cwin.width ; ++j)
	{
		pval=(int)(j*scale) & 0xFFFF;
		cwin.imdat[j]=allcells[pval].pixel;
	}
	for (j=1 ; j < cwin.height ; ++j)
		memcpy((char *)&(cwin.imdat[j*(int)cwin.width]),
			(char *)cwin.imdat, (int)cwin.width);

	return;
}
