/* The routines in this file control the cursor access (setting and getting) */
/* for the Figaro server version of the vista display server. */

/* Sam Southard, Jr. */
/* Created: 20-Apr-1991 */
/* Modification History: */
/* 25-Apr-1991	SNS/CIT	clearcurs routine added. */
/* 10-May-1991	SNS/CIT	Modified to be shared between Xvideo and PGDISP */
/* 11-Aug-1991	SNS/CIT	xvideo hooks removed */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */

/* The standard include files */
#include <stdio.h>
#include <malloc.h>

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"

struct curpos {
	short x;	/* x position */
	short y;	/* y position */
	short val;	/* the value of the button/key pressed. */
	struct curpos *next;	/* the next position */
};

static struct curpos *lgcurses=NULL;	/* the line graphics cursor events */
static struct curpos *lastlg=NULL;	/* last in list of line graphics */
static int lgx,lgy;			/* line graphics cursor location */

#ifndef PGDISP
static struct curpos *bmcurses=NULL;	/* the bitmap graphics cursor events */
static struct curpos *lastbm=NULL;	/* last in list of bitmap graphics */
static int bmx,bmy;			/* bitmap graphics cursor location */
#endif

/* The pggcurs and bmcurs routines get the first cursor event in the line */
/* graphics or bitmap graphics list and returns it in the buffer buf, which */
/* has the format for the BM_GET_CURS and LG_CURS commands. */
/* Return Values: */
/* Whatever getcurs returns */

int pggcurs(buf)
short *buf;
{
	return(getcurs(buf,&lgcurses,&lastlg));
}

#ifndef PGDISP
int bmgcurs(buf)
short *buf;
{
	return(getcurs(buf,&bmcurses,&lastbm));
}
#endif

/* The getcurs routine gets the first cursor point from the given list, */
/* updates the buffer accordingly, and updates the given pointer. */
/* Return Values: */
/* 0	There are no cursor positions to return */
/* 1	The buffer was set properly */

int getcurs(buf,curlist,listend)
short *buf;
struct curpos **curlist;
struct curpos **listend;
{
	struct curpos *tmpptr;

	/* the cursor hasn't yet been set */
	if (*curlist == NULL) return(0);

	/* get the data */
	buf[1]= (*curlist)->x;
	buf[2]= (*curlist)->y;
	buf[3]= (*curlist)->val;

	/* free up the used cursor point */
	tmpptr= *curlist;
	if ((*curlist= (*curlist)->next) == NULL) *listend=NULL;
	free((char *)tmpptr);

	return(1);
}

/* The pgcursor routine adds the specified event to the list of cursor events */
/* on the line graphics window. */
/* Return Value: 1 (no matter what) */

int pgcursor(event)
XEvent event;
{
	void addbuttonval();
	void addkeyval();

/*	char *malloc();    */

	if (lastlg != NULL)
	{
		/* if we can't get space for the next one */
#ifdef lint
		if (malloc(sizeof(struct curpos)) == NULL)
#else
		if ((lastlg->next=malloc(sizeof(struct curpos))) == NULL)
#endif
			return(1);
		lastlg=lastlg->next;
	} else { /* This is the first one */
#ifdef lint
		if (malloc(sizeof(struct curpos)) == NULL)
#else
	    	if ((lgcurses=malloc(sizeof(struct curpos))) == NULL)
#endif
			return(1);
		lastlg=lgcurses;
	}
	lastlg->next=NULL;

	/* no translations are needed on the line graphics window */
	if (event.type == ButtonPress)
	{
		lgx=lastlg->x = event.xbutton.x;
		lgy=lastlg->y = event.xbutton.y;
		addbuttonval(event.xbutton.button,lastlg);
	} else {
		lgx=lastlg->x = event.xkey.x;
		lgy=lastlg->y = event.xkey.y;
		addkeyval(event,lastlg);
	}
	return(1);
}

#ifndef PGDISP
/* The bmcursor routine adds the specified event to the list of cursor events */
/* on the bitmap graphics window. */
/* Return Value: 1 (no matter what) */

int bmcursor(event)
XEvent event;
{
	void addbuttonval();
	void addkeyval();

/*	char *malloc();    */

	if (lastbm != NULL)
	{
		/* if we can't get space for the next one */
#ifdef lint
		if (malloc(sizeof(struct curpos)) == NULL)
#else
		if ((lastbm->next=malloc(sizeof(struct curpos))) == NULL)
#endif
			return(1);
		lastbm=lastbm->next;
	} else { /* This is the first one */
#ifdef lint
		if (malloc(sizeof(struct curpos)) == NULL)
#else
	    	if ((bmcurses=malloc(sizeof(struct curpos))) == NULL)
#endif
			return(1);
		lastbm=bmcurses;
	}
	lastbm->next=NULL;

	/* translations are needed on the bitmap graphics window */
	if (event.type == ButtonPress)
	{
		bmx=lastbm->x = display_to_imagecol(event.xbutton.x);
		bmy=lastbm->y = display_to_imagerow(event.xbutton.y);
		addbuttonval(event.xbutton.button,lastbm);
	} else {
		bmx=lastbm->x = display_to_imagecol(event.xkey.x);
		bmy=lastbm->y = display_to_imagerow(event.xkey.y);
		addkeyval(event,lastbm);
	}
	return(1);
}
#endif

/* The addbuttonval routine adds the code for the specified button value to */
/* the specified cursor event structure. */

void addbuttonval(button,thiscur)
unsigned int button;
struct curpos *thiscur;
{
	switch(button)
	{
	case Button1:
		thiscur->val=0x0100;
		break;
	case Button2:
		thiscur->val=0x0101;
		break;
	case Button3:
		thiscur->val=0x0102;
		break;
	case Button4:
		thiscur->val=0x0103;
		break;
	default:
		thiscur->val=0x0104;
		break;
	}

	return;
}

/* The addkeyval routine adds the code for the specified keypress event to */
/* the specified cursor event structure. */

void addkeyval(event,thiscur)
XEvent event;
struct curpos *thiscur;
{
	char tmpchr;

	(void)XLookupString((XKeyEvent *)&event,&tmpchr,1,(KeySym *)NULL,
		(XComposeStatus *)NULL);
	thiscur->val=tmpchr;
}

/* The pgscurs routine sets the current line graphics cursor position.  Note */
/* that this does not do anything unless there are no entries in the list of */
/* cursor events and does not do anything visible unless this position is */
/* different than the previousrecorded line graphics cursor location. */

void pgscurs(x,y)
int x,y;
{
	if (lgcurses != NULL) return;

	/* is this different enough to warp the cursor */
	if (x+1 < lgx || x-1 > lgx || y-1 > lgy || y+1 < lgy)
		XWarpPointer(display,None,lg.win,0,0,0,0,x,y);
	lgx=x;
	lgy=y;

	return;
}

#ifndef PGDISP
/* The bmscurs routine sets the current bitmap graphics cursor position.  */
/* Note that this does not do anything unless there are no entries in the */
/* list of cursor events and does not do anything visible unless this */
/* position is different than the previous recorded line graphics cursor */
/* location. */

void bmscurs(x,y)
int x,y;
{
	if (bmcurses != NULL) return;

	/* is this different enough to warp the cursor */
	if ((x+1 < bmx || x-1 > bmx || y-1 > bmy || y+1 < bmy) &&
	    imagecol_to_display(x) >= 0 && imagecol_to_display(x) < bm.width
	    && imagerow_to_display(y) >= 0 &&
	    imagerow_to_display(y) < bm.height)
		XWarpPointer(display,None,bm.win,0,0,0,0,
			imagecol_to_display(x),imagerow_to_display(y));
	bmx=x;
	bmy=y;

	return;
}
#endif

/* The clearcurs routine clears all the cursor events from both lists. */

void clearcurs()
{
	struct curpos *tmpptr;

	while (lgcurses != NULL)
	{
		tmpptr=lgcurses->next;
		free((char *)lgcurses);
		lgcurses=tmpptr;
	}
#ifndef PGDISP
	while (bmcurses != NULL)
	{
		tmpptr=bmcurses->next;
		free((char *)bmcurses);
		bmcurses=tmpptr;
	}
	lastbm=NULL;
	bmx=bmy= -1;
#endif
	lastlg=NULL;
	lgx=lgy -1;
	return;
}
