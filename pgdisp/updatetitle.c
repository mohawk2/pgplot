/* The updatetitle image updates the bitmap window's title to reflect the */
/* position of the cursor.  The arguments are the window x and y position of */
/* the cursor event, and a logical variable writebad.  If writebad is true */
/* and the cursor is outside the image, the title will be changed to an error */
/* message.  If writebad is false, the window name will not be changed. */

/* Sam Southard, Jr. */
/* Created: 17-Sep-1991 (from mainloop.c) */
/* Modification History: */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 25-Nov-1991	SNS/CIT	Modified to deal with separate X & Y zoom factors */
/* 27-Nov-1991	SNS/CIT	Modified to lint cleanly. */
/* 14-Feb-1992	SNS/CIT	Now includes the id in the title */
/*  8-Apr-1992	SNS/CIT	Now has a better title for the Keck version */

#include "figdisp.h"
#include "globals.h"
#include <string.h>

void updatetitle(x,y,writebad)
int x,y;	/* cursor position */
int writebad;	/* update title when out of the image? */
{
	char newtitle[80];
	char tempstr[80];
	char *strptr= &newtitle[0];	/* for the call to set the name */
	double curx,cury,datval;	/* the cursor values */

#ifndef _AIX
	char *sprintf();
#endif

	if (!bm.showcur) return;

	/* get the image coordinates */
	x=display_to_imagecol(x);
	y=display_to_imagerow(y);
	if (x < 0 || x >= bm.imwidth || y < 0 || y >= bm.imheight)
	{
		if (writebad) (void)sprintf(&newtitle[0],
#ifdef KECK
			"Keck bitmap: Not inside image");
#else
			"Figdisp #%d: Not inside image",res.id);
#endif
		else return;
	} else { /* now we can get the transformed values */
		if (bppix == 16) datval=rimdat.b16[x+y*bm.imwidth];
		else datval=rimdat.b8[x+y*bm.imwidth];
		curx=(x+bm.curxoff)*bm.curxsc;
		cury=(y+bm.curyoff)*bm.curysc;
		datval = datval*bm.dsc+bm.doff;
		(void)sprintf(&newtitle[0],
#ifdef KECK
			"Keck bitmap: X: %.1f Y: %.1f Data: %.1f",
			curx, cury, datval);
#else
			"Figdisp #%d: X: %.1f Y: %.1f Data: %.1f", res.id,
			curx, cury, datval);
#endif
		if (bm.xzoom == bm.yzoom && bm.xzoom != 0)
		{
			if (bm.xzoom < 0) (void)sprintf(&tempstr[0],
				" Zoom: 1/%d", 1 << -bm.xzoom);
			else (void) sprintf(&tempstr[0], " Zoom: %d",
				1 << bm.xzoom);
			(void)strcat(&newtitle[0], &tempstr[0]);
		} else {
			if (bm.xzoom < 0) (void)sprintf(&tempstr[0],
				" X Zoom: 1/%d", 1 << -bm.xzoom);
			else if (bm.xzoom > 0) (void)sprintf(&tempstr[0],
				" X Zoom: %d", 1 <<bm.xzoom);
			if (bm.xzoom != 0)
				(void)strcat(&newtitle[0], &tempstr[0]);
			if (bm.yzoom < 0) (void)sprintf(&tempstr[0],
				" Y Zoom: 1/%d", 1 << -bm.yzoom);
			else if (bm.yzoom > 0) (void)sprintf(&tempstr[0],
				" Y Zoom: %d", 1 <<bm.yzoom);
			if (bm.yzoom != 0)
				(void)strcat(&newtitle[0], &tempstr[0]);
		}
	}
	if (XStringListToTextProperty(&strptr, 1, &bm.winname))
		XSetWMName(display, bm.win, &bm.winname);

	return;
}
