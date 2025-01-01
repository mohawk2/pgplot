/* The handlexevent routine takes care of a single X event for the figdisp */
/* display server. */
/* Return Values: */
/* FAIL		If something went wrong */
/* SUCCEED	If the user requested an exit */

/* Sam Southard, Jr. */
/* Created: 3-Dec-1991 (from figdisp mainloop.c) */
/* Modification History: */
/* 30-Jan-1992	SNS/CIT	Keck mods merged back into standard display server */
/* 31-Jan-1992	SNS/CIT	All motion events until a button event are now read */
/*			before any action is taken.  This drastically */
/*			improves response times on X terminals */
/*  6-Apr-1992	SNS/CIT	Now uses large crosshairs cursor for line graphics. */
/*			Now handles the case when a button release event is */
/*			lost. */
/*  8-Apr-1992	SNS/CIT	Buttonrelease lost recovery code added to LUT */
/*			manipulation button.  No longer sets active line */
/*			graphics color to 1 when drawing cursor. */
/* 10-Apr-1992	SNS/CIT	Now handles lg.winxoff & lg.winyoff */
/*  7-May-1992	SNS/CIT	Now handles res.lgcross */

/* The system include files */
#include <stdio.h>

#ifdef VMS
typedef char * caddr_t;
#endif

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

/* The program include files */
#include "commands.h"
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#define LUT_WRAP 1

int luttransoff=0;
int modluttransoff=0;

int handlexevent(event,go_on)
XEvent event;
int *go_on;	/* whether the calling routine shoudl exit successfully */
{
#ifndef PGDISP
	static int slitxs= -1,slitys;	/* the starting point for the slit */
	static int slitxe= -1,slitye;
	static int xorlinedrawn=0;
	int nluts;	/* the number of LUTs in the current image */
#endif
	int px,py;	/* The pointer X & Y position */
	XEvent event2;	/* A second event */
#ifndef KECK
	static short retbuf[7];		/* A buffer for return values */
	static int retbuflen=0;		/* the actual length of the buffer */
#endif
	static int lgx= -1 ,lgy;	/* current line graphics line pos */
	Window windum;
	int dummy;
	unsigned int keys;

#ifndef KECK
	void returnbuf();	/* return data to user program */
#endif
	void resizelgwin();	/* resize line graphics window */
	void exposelgwin();	/* expose the line graphics window */
	void updatelgtitle();	/* update the line graphics cursor position */
#ifndef PGDISP
	void updatetitle();
	void exposesee();	/* expose the seeing window */
	void resizebmwin();	/* resize the bitmap graphics window */
	void transluts();	/* map the LUT data into the LUTs we have */
	void transim();		/* translate the image window */
	void panpatch();	/* pan the patch window */
	void panloc();		/* pan the location window */
	void zoomim();		/* set the image zoom factor */
	void exposebmwin();	/* expose the bitmap graphics window */
	void exposepatch();	/* expose the patch window */
	void exposecmap();	/* expose the color map window */
	void doslit();		/* make a line plot */
#endif

	switch(event.type)
	{
#ifndef KECK
	case SelectionClear:
		/* Unless this is a selection atom, ignore it */
		if (event.xselectionclear.selection != selatom) break;

		/* someone's grabbed the selection, get ready for some data */
		if (!selset)
		{
			if ((srcwin=XGetSelectionOwner(display, selatom))
			    == None)
			{
				(void)fprintf(stderr,MSG_NOSELOWN);
				return(FAIL);
			}
			selset=1;
		}
		XConvertSelection(display, selatom, XA_STRING, dataatom,
			lg.win, event.xselectionclear.time);
		break;
	case SelectionNotify: /* someone sent us data! */
		*go_on=getdata(event.xselection, &retbuf[0], &retbuflen,
			srcwin, &selset);
		break;
#endif
	case Expose:
		if (event.xexpose.window == lg.win) exposelgwin(event.xexpose);
#ifndef PGDISP
		else if (event.xexpose.window == bm.win)
			exposebmwin(event.xexpose.x, event.xexpose.y,
				event.xexpose.width, event.xexpose.height);
		else if (event.xexpose.window == patch.win) exposepatch();
		else if (event.xexpose.window == loc.win) {
			if (updateloc()) return(FAIL);
		} else if (event.xexpose.window == cwin.win)
			exposecmap(event.xexpose);
		else if (event.xexpose.window == seeing.win) exposesee();
#endif
		break;
	case ConfigureNotify:
		if (event.xconfigure.window == lg.win)
			resizelgwin(event.xconfigure);
#ifndef PGDISP
		else if (event.xconfigure.window == bm.win)
			resizebmwin(event.xconfigure);
		else if (event.xconfigure.window == patch.win)
		{
			patch.width=event.xconfigure.width;
			patch.height=event.xconfigure.height;
		} else if (event.xconfigure.window == loc.win) {
			if (resizeloc(event.xconfigure.width,
				event.xconfigure.height)) return(FAIL);
		} else if (event.xconfigure.window == cwin.win)
			if (resizecmap(event.xconfigure.width,
				event.xconfigure.height)) return(FAIL);
#endif
		break;
#ifndef PGDISP
	case ButtonRelease:
		if (event.xbutton.window == bm.win)
		{
			if (event.xbutton.button == Button3)
			{
				bm.modlut=0;
				/* reset the color tables? */
				if (abs(event.xbutton.x -bm.mlx)< 10 &&
				    abs(event.xbutton.y - bm.mly) < 10)
				{
					if (bppix == 16) nluts=65536;
					else nluts=256;
					bm.slope=(nluts-1) /
						(double)(bm.colors-1);
					bm.offset=0;
					transluts();
				}
			} else if (event.xbutton.button == Button2) {
				/* it's possible that this has already been */
				/* taken care of, so drawing the line would */
				/* not be a good idea */
				if (slitxs < 0) break;
				/* erase the old line */
				if (xorlinedrawn) XDrawLine(display, bm.win,
					xorgc, slitxs, slitys, slitxe, slitye);
				doslit(display_to_imagecol(slitxs),
					 display_to_imagerow(slitys),
					 display_to_imagecol(event.xbutton.x),
					 display_to_imagerow(event.xbutton.y));
				slitxe=slitxs= -1;
				xorlinedrawn=0;
#ifdef NEW
			} else if (event.xbutton.button == Button1
				   && modluttransoff) {
				modluttransoff=0;
				if (bppix == 16) luttransoff= (65536*
					(event.xbutton.x -(int)bm.width/2))
					/((int)bm.width/2);
				else luttransoff= (256*
					(event.xbutton.x -(int)bm.width/2))
					/((int)bm.width/2);
				transluts();
#endif
			}
		}
		break;
#endif
	case MotionNotify:
		if (event.xbutton.window == lg.win)
		{
			/* absorb all motion events inside this window */
			px=event.xbutton.x;
			py=event.xbutton.y;
			while(XCheckMaskEvent(display, PointerMotionMask,
				&event2) != False)
			{
				if (event2.xbutton.window != lg.win) break;
				px=event2.xbutton.x;
				py=event2.xbutton.y;
			}
			updatelgtitle(px-lg.winxoff,py-lg.winyoff);
			/* don't go further unless using crosshair cursor */
			if (!res.lgcross)
				break;
			/* update the croshairs cursor */
			if (lgx != -1)
			{
				/* first we clear the old lines */
				XDrawLine(display, lg.win, linegcclear, 0, lgy,
					lg.width-1, lgy);
				XDrawLine(display, lg.win, linegcclear, lgx, 0,
					lgx, lg.height-1);
				/* we know that lgy and lgx are at least */
				/* non-negative */
				if (lg.winyoff < lgy &&
				    lgy-lg.winyoff < lg.imheight)
					XCopyArea(display, lg.pixmap, lg.win,
						linegc, 0, lgy-lg.winyoff,
						lg.width, 1, 0, lgy);
				if (lg.winxoff < lgx &&
				    lgx-lg.winxoff < lg.imwidth)
					XCopyArea(display, lg.pixmap, lg.win,
						linegc, lgx-lg.winxoff, 0, 1,
						lg.height, lgx, 0);
			}
			lgx=px;
			lgy=py;
			XSetForeground(display, linegcclear, lg.pix[1]);
			XDrawLine(display, lg.win, linegcclear, 0, lgy,
				lg.width-1, lgy);
			XDrawLine(display, lg.win, linegcclear, lgx, 0, lgx,
				lg.height-1);
			XSetForeground(display, linegcclear, lg.pix[0]);
		}
#ifndef PGDISP
		if (event.xbutton.window != bm.win) break;
		/* absorb all motion events until the next button press or */
		/* release */
		px=event.xbutton.x;
		py=event.xbutton.y;
		while (XCheckMaskEvent(display,
			ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
			&event2) != False)
		{
			switch(event2.type)
			{
			case ButtonPressMask:	/* press or release ends */
			case ButtonReleaseMask:	/* processing */
				XPutBackEvent(display,&event2);
				break;
			default:
				px=event2.xbutton.x;
				py=event2.xbutton.y;
				continue;	/* the while loop */
				break;
			}
			/* this will break out of the while loop if we got a */
			/* press or release event */
			break;
		}

#ifdef NEW
		if (modluttransoff)
		{
			modluttransoff=1;
			if (bppix == 16) luttransoff= (65536* (event.xbutton.x
				-(int)bm.width/2))/((int)bm.width/2);
			else luttransoff= (256* (event.xbutton.x
				-(int)bm.width/2))/((int)bm.width/2);
			transluts();
		}
#endif
		if (bm.modlut)
		{
			double scale;
			if (px < 0) px=0;
			if (px >= bm.width) px=bm.width-1;
			if ((py=bm.height-py-1) < 0) py=0;
			if (py >= bm.height) py=bm.height-1;
			if (bppix==16) nluts=65536;
			else nluts=256;
			bm.slope=nluts/(1.5*py);
			scale=2*nluts*((double)px/bm.width -0.5);
			bm.offset=(1.0-scale)*bm.slope+(nluts>>1);
			scale=(nluts-1)/(double)(bm.colors-1);
			bm.offset = (bm.offset+0.5)/scale;
			bm.offset *= LUT_WRAP;
			transluts();
			/* Make sure the pointer hasn't been released */
			if (XQueryPointer(display, bm.win, &windum, &windum,
				&dummy, &dummy, &px, &py, &keys) == True
			    && !(keys & Button3Mask))
			{
				bm.modlut=0;
				/* reset the color tables? */
				if (abs(event.xbutton.x -bm.mlx)< 10 &&
				    abs(event.xbutton.y - bm.mly) < 10)
				{
					if (bppix == 16) nluts=65536;
					else nluts=256;
					bm.slope=(nluts-1) /
						(double)(bm.colors-1);
					bm.offset=0;
					transluts();
				}
			}
		} else updatetitle(px,py,0);
		if (slitxs > 0)
		{
			/* erase old line */
			if (xorlinedrawn) XDrawLine(display, bm.win, xorgc,
				slitxs, slitys, slitxe, slitye);
			slitxe=px;
			slitye=py;
			if (XQueryPointer(display, bm.win, &windum, &windum,
				&dummy, &dummy, &px, &py, &keys) == True
			    && !(keys & Button2Mask))
			{ /* the button was released, so handle it */
				doslit(display_to_imagecol(slitxs),
					 display_to_imagerow(slitys),
					 display_to_imagecol(slitxe),
					 display_to_imagerow(slitye));
				slitxe=slitxs= -1;
				xorlinedrawn=0;
			} else {
				/* draw a new line */
				XDrawLine(display, bm.win, xorgc, slitxs,
					slitys, slitxe, slitye);
				xorlinedrawn=1;
			}
		}
#endif
		break;
	case ButtonPress:	/* currently used only for cursor */
#ifndef PGDISP
		if (event.xbutton.window == bm.win)
		{
			if (event.xbutton.button == Button3)
			{
				bm.modlut=1;
				bm.mlx=event.xbutton.x;
				bm.mly=event.xbutton.y;
			} else if (event.xbutton.button == Button2){
				if (slitxs >= 0) break;
				xorlinedrawn=0;
				slitxs=event.xbutton.x;
				slitys=event.xbutton.y;
			} else if (event.xbutton.button == Button1) {
#ifdef NEW
				if (event.xbutton.state & ControlMask)
				{
					modluttransoff=1;
					if (bppix == 16) luttransoff= (65536*
						(event.xbutton.x -
						(int)bm.width/2))
						/((int)bm.width/2);
					else luttransoff= (256*
						(event.xbutton.x
						-(int)bm.width/2))
						/((int)bm.width/2);
					transluts();
				} else transim(event.xbutton);
#else
				transim(event.xbutton);
#endif
			}
			break;
		} else if (event.xbutton.window == patch.win) {
			panpatch(event.xbutton);
			break;
		} else if (event.xbutton.window == loc.win) {
			panloc(event.xbutton);
			break;
		}
#ifdef KECK
		/* The keck version never needs to report a button press back */
		/* to a user program, so we should just exit.  The other */
		/* versions may need to tell the user which button was */
		/* pressed */
		break;
#endif
#endif
	case KeyPress:
#ifndef PGDISP
		/* we could have fallen through */
		if (event.type == KeyPress)
		{
			if ((*go_on=dokey(event.xkey)) == 1) break;
			if (*go_on == 0) return(SUCCEED);
		}
#endif
#ifndef KECK
		/* record this keypress for return to a user program if the */
		/* program is trying to get the cursor location */
		if (selset && (event.xany.window == lg.win && pgcursor(event)
#ifndef PGDISP
			|| event.xany.window == bm.win && bmcursor(event)
#endif
			))
		{
			if (retbuflen && (retbuf[0] == LG_CURS &&
				pggcurs(&retbuf[0])
#ifndef PGDISP
			    || retbuf[0] == BM_GET_CURS && bmgcurs(&retbuf[0])
#endif
			    ))
			{ /* the user's asked for one */
				returnbuf(&retbuf[0],4,srcwin);
				retbuflen=0;
			}
		}
#endif
		break;
	}

	return(SUCCEED);
}
