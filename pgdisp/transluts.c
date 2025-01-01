/* The transluts routine changes the LUT entries based on the x and y */
/* position of the given motion event.   This is basically TVTRANS built into */
/* the display server. */

/* Sam Southard, Jr. */
/* Created: 31-Jul-1991 */
/* Modification History: */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo. */
/* 27-Aug-1991	SNS/CIT	Now handles inversion */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells and usecells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */

#include "figdisp.h"
#include "globals.h"

void transluts()
{
	register int i,j;		/* silly loop variables */
	register double rval;
	int nluts;

#ifdef NEW
	extern int luttransoff;
#endif

	rval=bm.offset;
	if (bppix == 16) nluts = 65536;
	else nluts=256;
	for (i=0 ; i < bm.colors ; ++i, rval += bm.slope)
	{
		j=rval;
		if (j < 0) j=0;
		if (j >= nluts) j=nluts-1;
#ifdef NEW
		j -= luttransoff;
		while (j < 0)
			j += nluts;
		j %= nluts;
#endif
		if (bm.invert) j=nluts-1-j;
		usecells[i].red=allcells[j].red;
		usecells[i].green=allcells[j].green;
		usecells[i].blue=allcells[j].blue;
	}

	XStoreColors(display, bitcmap, usecells, bm.colors);

	return;
}
