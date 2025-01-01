/* This is the version of writeluts used by the FASTDISP version of the */
/* server.  The return value is the number of shorts which must be saved for */
/* the next iteration.  If the return value is non-zero than none of the LUTS */
/* were affected. */

/* Sam Southard, Jr. */
/* Created: 31-Jul-1991 */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/*  5-Mar-1992	SNS/CIT	Now works better on grayscale monitors */
/* 25-Jun-1992	SNS/CIT	First argument is now number of bits per pixel */

#include "figdisp.h"
#include "globals.h"

int writeluts(buf,len)
unsigned short **buf;	/* the command/data buffer */
int *len;	/* the number of shorts remaining in buf */
{
	unsigned short *bufptr= *buf;	/* for easier access */
	int bitsperpix;	/* the number of bits per pixel */
	int thislut;	/* the lut we're on */
	int nlut;	/* the number of entries to change */
	int alut;	/* the affected luts */
	int maxluts;	/* The maximum number of luts for this command */
	int targetlut;	/* The actual LUT to be affected */
	int i;		/* Silly loop variable */

	void transluts();	/* translate the LUTS into what we use */

	if (*len < 4)
	{ /* we need all the arguments */
		--*buf;		/* include the command token */
		return(*len+1);	/* there may be some parameters */
	}

	bitsperpix= *bufptr++;
	thislut= *bufptr++;	/* first affected LUT */
	nlut= *bufptr++;	/* number of LUT entries */
	alut = *bufptr++ & 0x7;		/* the affect LUTs */
	*len -= 4;

	/* make sure that all the data is there */
	if (alut && *len < nlut || !alut && *len < 3*nlut)
	{ /* not enough data */
		--*buf; /* include the command token */
		return(*len+4); /* we've already read the parameters */
	}

	*buf += 4;

	if (bitsperpix == 16) maxluts=65536;
	else maxluts=256;

	if (alut)
	{ /* a single entry for all affectes LUTs */
		*buf += nlut;
		*len -= nlut;
		while (thislut < maxluts && nlut-- > 0)
		{
			if (bitsperpix == 16 && bppix == 8)
				targetlut= (thislut >> 8);
			else if (bitsperpix == 8 && bppix == 16)
				targetlut= (thislut << 8);
			else targetlut=thislut;
			if (alut & 0x1) allcells[targetlut].red= *bufptr<<8;
			if (alut & 0x2) allcells[targetlut].green= *bufptr<<8;
			if (alut & 0x4) allcells[targetlut].blue= *bufptr<<8;
			if (bm.bw)
			{
				allcells[targetlut].red=
					0.30*allcells[targetlut].red
					+0.59*allcells[targetlut].green
					+0.11*allcells[targetlut].blue;
				allcells[targetlut].green=
					allcells[targetlut].blue=
					allcells[targetlut].red;
			}
			++thislut;
			++bufptr;
			/* do we need to propagate this value? */
			if (bitsperpix == 8 && bppix == 16)
			{
				for (i=1 ; i < 256 ; ++i)
				{
					allcells[targetlut+i].red=
						allcells[targetlut].red;
					allcells[targetlut+i].green=
						allcells[targetlut].green;
					allcells[targetlut+i].blue=
						allcells[targetlut].blue;
				}
			}
		}
	} else {
		*buf += (3*nlut);
		*len -= (3*nlut);
		while (thislut < maxluts && nlut-- > 0)
		{
			if (bitsperpix == 16 && bppix == 8)
				targetlut= (thislut >> 8);
			else if (bitsperpix == 8 && bppix == 16)
				targetlut= (thislut << 8);
			else targetlut=thislut;
			allcells[targetlut].red= *bufptr++ << 8;
			allcells[targetlut].green= *bufptr++ << 8;
			allcells[targetlut].blue= *bufptr++ << 8;
			if (bm.bw)
			{
				allcells[targetlut].red=
					0.30*allcells[targetlut].red
					+0.59*allcells[targetlut].green
					+0.11*allcells[targetlut].blue;
				allcells[targetlut].green=
					allcells[targetlut].blue=
					allcells[targetlut].red;
			}
			++thislut;
			/* do we need to propagate this value? */
			if (bitsperpix == 8 && bppix == 16)
			{
				for (i=1 ; i < 256 ; ++i)
				{
					allcells[targetlut+i].red=
						allcells[targetlut].red;
					allcells[targetlut+i].green=
						allcells[targetlut].green;
					allcells[targetlut+i].blue=
						allcells[targetlut].blue;
				}
			}
		}
	}

	/* update the actual maps */
	transluts();

	/* we got all we need */
	return(0);
}
