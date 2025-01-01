/* The copyzoom routine updates the zoom plane corresponding to the specified */
/* rectangle, assumed to have 0 <= x1 <= x2 <= imwidth and corresponding y. */

/* Sam Southard, Jr. */
/* Created: 1-Aug-1991 */
/* Modification History: */
/*  4-Aug-1991	SNS/CIT	Now deals with the slow zoom algorithm. */
/*  5-Aug-1991	SNS/CIT	Slow zoom algorithm now the only one to use */
/*  7-Aug-1991	SNS/CIT	All known bugs worked out */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 25-Nov-1991	SNS/CIT	Modified to handle separate X & Y zoom factors */
/* 10-Apr-1992	SNS/CIT	Now uses xim translation routines */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

#include "figdisp.h"
#include "globals.h"

void copyzoom(x1,y1,x2,y2)
int x1,y1;	/* the upper left corner */
int x2,y2;	/* the lower right corner */
{
	int i,j,k,l;	/* silly loop variable */

	unsigned char *ptr1;	/* pointer to the window data */
	unsigned char *ptr8;	/* pointer to the 8-bit image data */
	unsigned short *ptr16;	/* pointer to the 16-bit image data */
	int nx,ny;	/* the number of pixels to be replicated or skipped, */
			/* depending on the sign of xzoom and yzoom */

	if ((x1=imagecol_to_xim(x1)) < 0) x1=0;
	if ((x2=imagecol_to_xim(x2+1)) >= bm.image->width)
		x2=bm.image->width-1;
	if ((y1=imagerow_to_xim(y1)) < 0) y1=0;
	if ((y2=imagerow_to_xim(y2+1)) >= bm.image->height)
		y2=bm.image->height-1;
	
	if (x1 >= bm.image->width || x2 < 0 || y1 >= bm.image->height || y2 < 0)
		return;
	
	/* chop the x coordinates to reasonable ranges */
	if (xim_to_imagecol(x1) < 0 || xim_to_imagecol(x2) >= bm.imwidth)
	{
		for (j=x1 ; j <= x2 ; ++j)
		{
			if ((l=xim_to_imagecol(j)) >= bm.imwidth)
			{
				x2= j-1;
				break;
			}
			if (l <= 0) x1=j;
		}
	}
	x1=imagecol_to_xim(xim_to_imagecol(x1));

	/* chop the y coords down as well */
	if (xim_to_imagerow(y1) < 0 || xim_to_imagerow(y2) >= bm.imwidth)
	{
		for (i=y1 ; i <= y2 ; ++i)
		{
			if ((k=xim_to_imagerow(i)) >= bm.imheight)
			{
				y2= i-1;
				break;
			}
			if (k < 0) y1=i-1;
		}
	}

	/* separate cases for speed */
	if (bm.xzoom > 0 && bm.yzoom > 0)
	{
		nx= 1 << bm.xzoom;
		ny= 1 << bm.yzoom;
		for (i=y1 ; i <= y2 ; )
		{
			k=xim_to_imagerow(i);
			ptr1=bm.imdat+i*bm.image->width+x1;
			/* copy one line */
			if (bppix == 8)
			{
				ptr8=rimdat.b8+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++ptr8)
					for (k=0 ; k < nx  && j <= x2; ++k, ++j)
						*ptr1++ = allcells[*ptr8].pixel;
			} else {
				ptr16=rimdat.b16+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++ptr16)
					for (k=0 ; k < nx  && j <= x2; ++k, ++j)
						*ptr1++ =allcells[*ptr16].pixel;
			}
			/* the first line is complete, bcopy the rest */
			ptr1=bm.imdat+i*bm.image->width+x1;
			++i;	/* we've done one line */
			k=x2-x1+1;
			for (j=1 ; j < ny && i <= y2 ; ++i, ++j)
				memcpy((char *)(ptr1+j*bm.image->width),
					(char *)ptr1, k);
		}
	} else if (bm.xzoom == 0 && bm.yzoom == 0) {
		ptr1=bm.imdat+y1*bm.image->width+x1;
		if (bppix == 8) ptr8=xim_to_imagerow(y1)*bm.imwidth+
			xim_to_imagecol(x1)+rimdat.b8;
		else ptr16=xim_to_imagerow(y1)*bm.imwidth+
			xim_to_imagecol(x1)+rimdat.b16;
		k= ((int)bm.width-(x2-x1+1));
		l= (bm.imwidth-(x2-x1+1));
		if (bppix == 8)
		{
			for (i=y1 ; i <= y2 ; ++i, ptr1 += k, ptr8 += l)
			{
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[*ptr8++].pixel;
			}
		} else {
			for (i=y1 ; i <= y2 ; ++i, ptr1 += k, ptr16 += l)
			{
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[*ptr16++].pixel;
			}
		}
	} else if (bm.xzoom < 0 && bm.yzoom <= 0) {
		for (i=y1 ; i <= y2 ; ++i)
		{
			if ((k=xim_to_imagerow(i)) >= bm.imheight) break;
			ptr1=bm.imdat+i*bm.image->width+x1;
			if (k < 0)
			{
				for (j=x1 ; j < x2 ; ++j)
					*ptr1++ = allcells[0].pixel;
				continue;
			}
			for (j=x1 ; xim_to_imagecol(j) < 0 ; ++j)
				*ptr1++ = allcells[0].pixel;
			if (bppix == 8)
			{
				ptr8=rimdat.b8+k*bm.imwidth;
				for ( ; j <= x2  &&
				     xim_to_imagecol(j) < bm.imwidth ;
				     ++j, ++ptr1)
					*ptr1 = allcells[*(ptr8+
						xim_to_imagecol(j))].pixel;
			} else {
				ptr16=rimdat.b16+k*bm.imwidth;
				for ( ; j <= x2 &&
				     xim_to_imagecol(j) < bm.imwidth ;
				     ++j, ++ptr1)
					*ptr1 = allcells[*(ptr16+
						xim_to_imagecol(j))].pixel;
			}
			for ( ; j < x2 ; ++j)
				*ptr1++ = allcells[0].pixel;
		}
	} else if (bm.xzoom > 0 && bm.yzoom <= 0) {
		nx= 1 << bm.xzoom;
		for (i=y1 ; i <= y2 ; ++i)
		{
			if ((k=xim_to_imagerow(i)) >= bm.imheight) break;
			if (k < 0)
			{
				for (j=x1 ; j < x2 ; ++j)
					*ptr1++ = allcells[0].pixel;
				continue;
			}
			ptr1=bm.imdat+i*bm.image->width+x1;
			if (bppix == 8)
			{
				ptr8=rimdat.b8+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++ptr8)
					for (k=0 ; k < nx  && j <= x2; ++k, ++j)
						*ptr1++ = allcells[*ptr8].pixel;
			} else {
				ptr16=rimdat.b16+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++ptr16)
					for (k=0 ; k < nx  && j <= x2; ++k, ++j)
						*ptr1++ =allcells[*ptr16].pixel;
			}
		}
	} else if (bm.xzoom < 0 && bm.yzoom > 0) {
		ny= 1 << bm.yzoom;
		for (i=y1 ; i <= y2 ; )
		{
			k=xim_to_imagerow(i);
			ptr1=bm.imdat+i*bm.image->width+x1;
			for (j=x1 ; xim_to_imagecol(j) < 0 ; ++j)
				*ptr1++ = allcells[0].pixel;
			/* copy one line */
			if (bppix == 8)
			{
				ptr8=rimdat.b8+k*bm.imwidth;
				for ( ; j <= x2  ; ++j, ++ptr1)
					*ptr1 = allcells[*(ptr8+
						xim_to_imagecol(j))].pixel;
			} else {
				ptr16=rimdat.b16+k*bm.imwidth;
				for ( ; j <= x2  ; ++j, ++ptr1)
					*ptr1 = allcells[*(ptr16+
						xim_to_imagecol(j))].pixel;
			}
			/* the first line is complete, bcopy the rest */
			ptr1=bm.imdat+i*bm.image->width+x1;
			++i;	/* we've done one line */
			k=x2-x1+1;
			for (j=1 ; j < ny && i <= y2 ; ++i, ++j)
				memcpy((char *)(ptr1+j*bm.image->width),
					(char *)ptr1, k);
		}
	} else if (bm.xzoom == 0 && bm.yzoom < 0) {
		for (i=y1 ; i <= y2 ; ++i)
		{
			if ((k=xim_to_imagerow(i)) >= bm.imheight) break;
			ptr1=bm.imdat+i*bm.image->width+x1;
			if (k < 0)
			{
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[0].pixel;
				continue;
			}
			if (bppix == 8)
			{
				ptr8=rimdat.b8+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[*ptr8++].pixel;
			} else {
				ptr16=rimdat.b16+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[*ptr16++].pixel;
			}
		}
	} else if (bm.xzoom == 0 && bm.yzoom > 0) {
		ny= 1 << bm.yzoom;
		for (i=y1 ; i <= y2 ; )
		{
			k=xim_to_imagerow(i);
			ptr1=bm.imdat+i*bm.image->width+x1;
			/* copy one line */
			if (bppix == 8)
			{
				ptr8=rimdat.b8+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[*ptr8++].pixel;
			} else {
				ptr16=rimdat.b16+k*bm.imwidth+
					xim_to_imagecol(x1);
				for (j=x1 ; j <= x2 ; ++j)
					*ptr1++ = allcells[*ptr16++].pixel;
			}
			/* the first line is complete, bcopy the rest */
			ptr1=bm.imdat+i*bm.image->width+x1;
			++i;	/* we've done one line */
			k=x2-x1+1;
			for (j=1 ; j < ny && i <= y2 ; ++i, ++j)
				memcpy((char *)(ptr1+j*bm.image->width),
					(char *)ptr1, k);
		}
	}

	
	return;
}
