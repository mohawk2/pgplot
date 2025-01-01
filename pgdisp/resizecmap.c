/* The resizecmap routine resizes the color map window to the given size. */

/* Return Values: */
/* FAIL		Something went wrong and the program should exit */
/* SUCCEED	Everything's fine */

/* Sam Southard, Jr. */
/* Created: 3-Sep-1991 (from resizeloc) */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>
#ifndef VMS
#include <malloc.h>
#endif

int resizecmap(x,y)
int x,y;	/* the new size of the window */
{
	unsigned char *newdat;		/* the new data area */
	int j;		/* silly loop variable */
	double scale;	/* to make the ramp fit across the display */
	int nluts;	/* the number of LUTs in this size image */

#ifdef lint
	newdat=cwin.imdat;
	if (realloc((char *)cwin.imdat, (unsigned)x*y) == NULL)
#else
	if ((newdat=(unsigned char *)realloc((char *)cwin.imdat,(unsigned)x*y))
	    == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}
	if (bppix == 16) nluts=65536;
	else nluts=256;
	cwin.image->data=NULL;
	XDestroyImage(cwin.image);
	cwin.imdat=newdat;
	cwin.width=x;
	cwin.height=y;
	scale=(double)(nluts-1)/(cwin.width-1);
	for (j=0 ; j < cwin.width ; ++j)
		cwin.imdat[j]=allcells[(int)(j*scale) & 0xFFFF].pixel;
	for (j=1 ; j < cwin.height ; ++j)
		memcpy((char *)&(cwin.imdat[j*(int)cwin.width]),
			(char *)cwin.imdat, (int)cwin.width);

	cwin.image=XCreateImage(display, bitvisual, bitdepth, ZPixmap, 0,
		(char *)cwin.imdat, cwin.width, cwin.height, 8, 0);

	return(SUCCEED);
}
