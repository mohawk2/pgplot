/* The writeimage16 routine takes a buffer which begins with the BM_WRITE16 */
/* command and updates the image appropriately.  If this routine does not */
/* finish the image write on this invocation (if the write is split across */
/* X11 messages), then it mocks up the buffer so that the next call to this */
/* routine will work correctly.  This routine should not be called */
/* unless all the arguments are in the command buffer (unless this is a */
/* secondary invocation).  Giving this routine a NULL-pointer len tells it to */
/* reset all values and abort the current write, for use when the connection */
/* to the user program goes away without completing the transfer.  If len is */
/* a NULL-pointer no other values matter.  This routine returns the number of */
/* shorts which should be saved for inclusion with the next data buffer. */
/* The only difference between this routine and the writeimage routine is */
/* that this routine deals with 16-bit data.  I would have combined them, but */
/* it would just make for a lot of if statements */

/* Sam Southard, Jr. */
/* Created: 16-Oct-1991 (from writeimage) */
/* Modification History: */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/* 24-Jun-1992	SNS/CIT	Now invalidates the histogram equalization data */

/* The standard include files */
#include <stdio.h>
#ifndef VMS
#include <malloc.h>
#endif

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"
#include "commands.h"

int writeimage16(buf,len)
short **buf;	/* the command/data buffer */
int *len;	/* the length of the buffer */
{
	static unsigned short *incline;	/* incomplete line data */
	static int nsaved=0;	/* the number of characters saved */

	int rows;	/* the number of rows left in a transfer */
	int cols;	/* the number of columns in a transfer */
	int startx;	/* the starting column */
	int thisrow;	/* the row we're on */
	int leftcols=0;	/* columns off the left edge */
	int rightcols=0; /* columns off the right edge */
	short *bufptr;	/* A pointer to the data.  Used only to make the code */
			/* clearer. */
	unsigned short *im16;	/* A pointer to the image data */
	unsigned char *im8;	/* A pointer to the 8-bit image data */
	unsigned short *pixvals;	/* used only for pixels */
	int npix;	/* number of pixels in the transfer, assuming that */
			/* the rest of the transfer is all pixels */
	int pixperline;	/* number of pixels per line of data */
	int i;		/* silly loop variable */
	unsigned short *savepix;

	void redrawim();	/* redraw the bitmap image */

	/* First see if we should reset everything */
	if (len == NULL)
	{
		if (nsaved) free((char *)incline);
		return(nsaved=0);
	}

	bufptr= *buf;

	startx= *bufptr++;	/* first the starting x coord */
	thisrow= *bufptr++;	/* then the starting y coord */
	pixperline=cols= *bufptr++;	/* then the width */
	rows= *bufptr++;	/* then the height */

	/* clip the data to the region which we actually have */
	if (startx < 0) leftcols= -startx;
	startx += leftcols;
	cols -= leftcols;
	if (startx+cols > bm.imwidth) rightcols=(startx+cols-bm.imwidth);
	cols -= rightcols;

	/* update the outside variables */
	*buf= bufptr;
	*len -= 4;

	/* get the number of pixels left */
	npix= *len;

	if (bppix == 16) im16=(rimdat.b16+bm.imwidth*thisrow+startx);
	else im8=(rimdat.b8+bm.imwidth*thisrow+startx);

	savepix=(unsigned short *)bufptr;

	/* a write to the image makes histogram data invalid */
	goodhist=0;

	if (nsaved)
	{ /* there's data left over from last time */
		if (nsaved+npix < pixperline)
		{ /* that's a small buffer! */
#ifdef lint
			pixvals=NULL;
			if (realloc((char *)incline,
				sizeof(unsigned short)*(unsigned)nsaved+npix)
			    == NULL)
#else
			if ((pixvals=(unsigned short *)realloc((char *)incline,
				sizeof(unsigned short)*(unsigned)nsaved+npix))
			    == NULL)
#endif
			{
				(void)fprintf(stderr,MSG_MALLOC);
				free((char *)incline);
				incline=NULL;
				nsaved += npix;
			} else {
				incline=pixvals;
				for (i=0 ; i < npix ; )
					incline[nsaved++]= *savepix++;
			}
			npix = 0;
		} else { /* there's an entire line */
			if (thisrow >= bm.imheight || thisrow < 0)
			{ /* this row is out of range */
				pixvals += (pixperline-nsaved);
				if (bppix == 8) im8 += bm.imwidth;
				else im16 += bm.imwidth;
			} else {
				i=0;
				/* discard the pixels off the left edge */
				if (nsaved <= leftcols)
				{
					nsaved=0;
					pixvals += (leftcols-nsaved);
				}
				/* if some saved pixels should be used */
				/* note that if there weren't enough, this */
				/* would fall through (i would be at least 0) */
				/* Also get any pixels we need from the */
				/* current buffer */
				if (bppix == 16)
				{
					for (i=leftcols ;
					     i < cols  && i < nsaved ; )
						*im16++ = incline[i++];
					for ( ; i < cols ; ++i)
						*im16++ = *savepix++;
				} else {
					for (i=leftcols ;
					     i < cols  && i < nsaved ; )
						*im8++ = (incline[i++] >> 8);
					for ( ; i < cols ; ++i)
						*im8++ = (*savepix++ >> 8);
				}
			}
			npix -= (pixperline-nsaved);
			--rows;
			++thisrow;
			free((char *)incline);
			nsaved=0;
		}
	}

	if ((unsigned short *)bufptr != savepix)
		(void)printf("bufptr is %d, savepix is %d\n",bufptr, savepix);

	pixvals= (unsigned short *)bufptr;

	while(rows > 0 && npix >= pixperline)
	{ /* as long as we have enough data for one more line */
		if (thisrow >= bm.imheight || thisrow < 0)
		{ /* this row is out of range */
			pixvals += pixperline;
			if (bppix == 16) im16 += bm.imwidth;
			else im8 += bm.imwidth;
		} else {
			/* discard the ones off the left edge */
			pixvals += leftcols;
			if (bppix == 16)
			{
				for (i=0 ; i < cols ; ++i)
					*im16++ = *pixvals++;
				im16 += (bm.imwidth-cols);
			} else {
				for (i=0 ; i < cols ; ++i)
					*im8++ = (*pixvals++ >> 8);
				im8 += (bm.imwidth-cols);
			}
			pixvals += rightcols;
			/* The display will need to be updated */
			redrawim(0, thisrow, bm.imwidth-1, thisrow);
		}
		npix -= pixperline;
		--rows;
		++thisrow;
	}

	if (rows > 0)
	{ /* an incomplete line was found */
#ifdef lint
		if (malloc(sizeof(unsigned short)*(unsigned)npix) == NULL)
#else
		if ((incline=(unsigned short *)malloc(
		    sizeof(unsigned short)*(unsigned)npix)) == NULL)
#endif
			(void)fprintf(stderr,MSG_MALLOC);
		else for (i=0 ; i < npix ; ) incline[i++]= *pixvals++;
		npix = 0;
		nsaved=npix;
	}

	*buf += (*len - npix);
	*len -= (*len - npix);

	if (rows > 0)
	{ /* we need to save the header for the next buffer */
		*buf -= 6;
		bufptr= *buf;
		*bufptr++ =BM_WRITE;
		*bufptr++ =16;
		*bufptr++ =(startx-leftcols);
		*bufptr++ =thisrow;
		*bufptr++ =pixperline;
		*bufptr++ =rows;
		*len = 6; /* if this happens, we're at the end of a buffer */
		return(6);
	}

	return(0);
}
