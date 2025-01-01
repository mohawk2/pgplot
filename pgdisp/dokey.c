/* The dokey routine handles a keypress event. */
/* Return Values: */
/* 0	The program should exit becuase the user selected quit */
/* 1	This routine handled the key press */
/* 2	There was nothing to do for this key press, so treat it normally. */

/* Sam Southard, Jr. */
/* Created: 17-Sep-1991 (from mainloop.c) */
/*  1-Oct-1991	SNS/CIT	Now knows about row and column plots */
/*  3-Oct-1991	SNS/CIT	Now knows about increasing, decreasing, and resetting */
/*			the slit width. */
/*  7-Oct-1991	SNS/CIT	Now looks up all keys in a table.  Key to inhibit */
/*			interpretation of other keys added. */
/*  8-Oct-1991	SNS/CIT	Now gets global variables from globals.h */
/* 21-Nov-1991	SNS/CIT	Now recenters properly at non-zero zoom factors */
/* 25-Nov-1991	SNS/CIT	Now handles separate X & Y zoom factors */
/*  8-Apr-1992	SNS/CIT	Title string now updated. */
/* 10-Apr-1992	SNS/CIT	Now takes into account winxoff & winyoff */
/* 14-Apr-1992	SNS/CIT	Modified to run under VMS. */
/* 24-Jun-1992	SNS/CIT	Now knows about histogram equalization */
/* 25-Jun-1992	SNS/CIT	Now flushes the display if we handled the key press. */

/* The program include files */
#include "figdisp.h"
#include "commands.h"
#include "globals.h"

/* The system include files */
#include <stdio.h>

#define LUT_WRAP 1

int dokey(event)
XKeyEvent event;	/* the event which prompted this. */
{
	int imx, imy;	/* the image coordinates */
	char dum;	/* a dummy character */
	KeySym keysym;	/* the key that was pressed */
	char newtitle[80];		/* the new title window */
	char *strptr= &newtitle[0];	/* a pointer to the new title window */
	static int inhibit=0;	/* inhibit other key translations */
	int max;	/* The maximum number of colors */
	int i,j;	/* silly loop variables */
	int itmp;	/* an integer temporary */
	double scale;	/* a scaling factor */
	XExposeEvent expevent;	/* ane xpose event */
	int lutent;	/* A lut entry */

	void updatetitle();	/* update the title bar */
	void zoomim();		/* update the zoom factor */
	void showloc();		/* show the location windo */
	void printhelp();	/* print the help message */
	void redrawim();	/* redraw the image */
	void updatepatch();	/* update the patch window */
	void lwprintwin();	/* print an area of the image in PostScript */
	void transluts();	/* update the lookup tables */
	void doseeing();	/* calculate the seeing near a given point */
	void doslit();		/* make the line drawing */
	void calchist();	/* Calculate the histogram equalization data */

#ifndef _AIX
#ifndef VMS
	char *sprintf();
#endif
#endif

	/* first we handle the three function keys for toggle */
	/* cursor, zoom out, and zoom in */
	(void)XLookupString(&event, &dum, 1, &keysym, (XComposeStatus *)NULL);

	if (event.window == bm.win)
	{
		imx=display_to_imagecol(event.x);
		imy=display_to_imagerow(event.y);
	} else if (event.window == loc.win) {
		imx=(event.x-BLANK_WIDTH)*((double)bm.imwidth/loc.width);
		imy=(event.y-BLANK_WIDTH)*((double)bm.imheight/loc.height);
	} else imx=imy= -1;

	if (inhibit && keysym != res.keys[INHIBIT]) return(2);

	if (keysym == res.keys[CURSOR])
	{
		if (bm.showcur)
		{
			bm.showcur=0;
#ifdef KECK
			(void)sprintf(&newtitle[0],"Keck bitmap graphics");
#else
			(void)sprintf(&newtitle[0],"bitmap graphics #%d",
				res.id);
#endif
			if (XStringListToTextProperty(&strptr, 1, &bm.winname))
				XSetWMName(display, bm.win, &bm.winname);
		} else {
			bm.showcur=1;
			if (event.window == bm.win)
				updatetitle(event.x, event.y, 1);
			else updatetitle(-1, -1, 1);
		}
	} else if (keysym == res.keys[ZOOMOUT]) {
		zoomim(bm.xzoom-1, bm.yzoom-1);
		if (event.window == bm.win) updatetitle(event.x,event.y, 0);
		showloc();
	} else if (keysym == res.keys[ZOOMNORM]) {
		zoomim(0, 0);
		if (event.window == bm.win) updatetitle(event.x,event.y, 0);
		showloc();
	} else if (keysym == res.keys[ZOOMIN]) {
		zoomim(bm.xzoom+1, bm.yzoom+1);
		if (event.window == bm.win) updatetitle(event.x, event.y, 0);
		showloc();
	} else if (keysym == res.keys[ZOOMXIN]) {
		zoomim(bm.xzoom+1, bm.yzoom);
		if (event.window == bm.win) updatetitle(event.x, event.y, 0);
		showloc();
	} else if (keysym == res.keys[ZOOMXOUT]) {
		zoomim(bm.xzoom-1, bm.yzoom);
		if (event.window == bm.win) updatetitle(event.x, event.y, 0);
		showloc();
	} else if (keysym == res.keys[ZOOMYIN]) {
		zoomim(bm.xzoom, bm.yzoom+1);
		if (event.window == bm.win) updatetitle(event.x, event.y, 0);
		showloc();
	} else if (keysym == res.keys[ZOOMYOUT]) {
		zoomim(bm.xzoom, bm.yzoom-1);
		if (event.window == bm.win) updatetitle(event.x, event.y, 0);
		showloc();
	} else if (keysym == res.keys[HELP]) printhelp();
	else if (keysym == res.keys[RECENTER]) {
		bm.xoff= bm.imwidth >> 1;
		bm.yoff= bm.imheight >> 1;

		if (bm.xzoom >= 0) bm.xoff -= (bm.width >> (bm.xzoom+1));
		else if (bm.xzoom < -1) bm.xoff -= (bm.width << (-bm.xzoom-1));
		else bm.xoff -= bm.width;

		if (bm.yzoom >= 0) bm.yoff -= (bm.height >> (bm.yzoom+1));
		else if (bm.yzoom < -1) bm.yoff -= (bm.height << (-bm.yzoom-1));
		else bm.yoff -= bm.height;
		XFillRectangle(display, bm.win, bitgcclear, 0, 0, bm.width,
			bm.height);
		redrawim(display_to_imagecol(0), display_to_imagerow(0),
			display_to_imagecol(bm.width-1),
			display_to_imagerow(bm.height-1));
		if (event.window == bm.win)  updatetitle(event.x,event.y, 0);
		showloc();
	} else if (keysym == res.keys[SHOWPAT]) {
		if (patch.mapped && event.window == patch.win)
		{
			XUnmapWindow(display, patch.win);
			patch.mapped=0;
		} else if (event.window == bm.win || event.window == loc.win) {
			XMapWindow(display, patch.win);
			updatepatch(imx, imy);
			patch.mapped=1;
		}
	} else if (keysym == res.keys[SHOWLOC]) {
		if (loc.mapped)
		{
			XUnmapWindow(display, loc.win);
			loc.mapped=0;
		} else {
			XMapWindow(display, loc.win);
			loc.mapped=1;
		}
	} else if (keysym == res.keys[IMPS])
		lwprintwin(0, 0, bm.imwidth, bm.imwidth);
	else if (keysym == res.keys[WINPS]) lwprintwin(display_to_imagecol(0),
		display_to_imagerow(0), display_to_imagecol((int)bm.width-1)-
		display_to_imagecol(0)+1,
		display_to_imagerow((int)bm.height-1)-
		display_to_imagerow(0)+1);
	else if (keysym == res.keys[INVERT]) {
		bm.invert = !bm.invert;
		transluts();
	} else if (keysym == res.keys[HISTOGRAM]) {
		usehist= !usehist;
		if (usehist && !goodhist) calchist();
		if (bppix == 16) max=65536;
		else max=256;
		if (usehist)
		{ /* usehist may have been reset to 0 by calchist */
			/* I is pixel value, j is histogram results */
			for (i=0, j=1 ; j < bm.colors*LUT_WRAP-1 ; ++j)
			{
				/* get the highest possible pixel value */
				while (histpix[j] == histpix[j+1] &&
				       j++ < bm.colors*LUT_WRAP-1)
					;
				itmp=histpix[j];
				if (itmp > max) itmp=max;
				lutent= (j % bm.colors);
				if (--lutent < 0) lutent=bm.colors-1;
				while (i < itmp)
					allcells[i++].pixel=bm.pix[lutent];
			}
			while (i < max)
				allcells[i++].pixel=bm.pix[bm.colors-1];
		} else {
			scale=(double)(max-1)/(bm.colors*LUT_WRAP);
			/* i is pixel value, j is lut entry */
			for(i=0, j=0 ; j < bm.colors*LUT_WRAP ; ++j)
			{
				itmp=(j+1)*scale;
				if (itmp > max) itmp=max;
				lutent= (j%bm.colors);
				while (i < itmp)
					allcells[i++].pixel=bm.pix[lutent];
			}
			while (i < max)
				allcells[i++].pixel=bm.pix[bm.colors-1];
		}
		/* re-map the image with the new translation */
		redrawim(display_to_imagecol(0), display_to_imagerow(0),
			display_to_imagecol((int)bm.width),
			display_to_imagerow((int)bm.height));
		transluts();
		updateloc();
		resizecmap(cwin.width, cwin.height);
		expevent.x=expevent.y=0;
		expevent.width=cwin.width;
		expevent.height=cwin.height;
		exposecmap(expevent);
	} else if (keysym == res.keys[SEEING]) {
		if (seeing.mapped && event.window == seeing.win)
		{
			seeing.mapped=0;
			XUnmapWindow(display, seeing.win);
		} else if (event.window == bm.win || event.window == loc.win) {
			seeing.mapped=1;
			doseeing(imx,imy);
			XMapWindow(display, seeing.win);
		}
	} else if (keysym == res.keys[SHOWCM]) {
		if (cwin.mapped)
		{
			XUnmapWindow(display, cwin.win);
			cwin.mapped=0;
		} else {
			XMapWindow(display, cwin.win);
			cwin.mapped=1;
		}
	} else if (keysym == res.keys[ROW]) {
		if (event.window == bm.win || event.window == loc.win)
			doslit(display_to_imagecol(0), imy,
				display_to_imagecol((int)bm.width-1), imy);
	} else if (keysym == res.keys[COL]) {
		/* the plot goes from bottom to top, not top to bottom */
		if (event.window == bm.win || event.window == loc.win)
			doslit(imx, display_to_imagerow((int)bm.height-1), imx,
				display_to_imagerow(0));
	} else if (keysym == res.keys[INCSLIT]) {
		++lg.xzoom;
		(void)printf("Now averaging %d pixels\n", (1 << lg.xzoom)+1);
	} else if (keysym == res.keys[DECSLIT]) {
		--lg.xzoom;
		if (lg.xzoom < 0) lg.xzoom = 0;
		if (lg.xzoom > 0)
			(void)printf("Now averaging %d pixels\n", 1 <<lg.xzoom);
		else (void)printf("No longer averaging pixels\n");
	} else if (keysym == res.keys[RESSLIT]) {
		lg.xzoom=0;
		(void)printf("No longer averaging pixels\n");
	} else if (keysym == res.keys[QUIT]) return(0);
	else if (keysym == res.keys[INHIBIT]) {
		if (inhibit) inhibit=0;
		else inhibit=1;
	}
	/* we didn't handle it, so just treat it as normal */
	else return(2);

	/* we did handle it */
	XFlush(display);
	return(1);
}
