/* The initbmwin routine initializes the window we need to do bitmap graphics */
/* Return Values: */
/* FAIL		If something prevented us from doing all we needed to */
/* SUCCEED	If everything went fine */

/* Sam Southard, Jr. */
/* Created: 29-Jul-1991 (from figdisp/initlgwin) */
/* 31-Jul-1991	SNS/CIT	Now creates the XImage as well. */
/*  4-Aug-1991	SNS/CIT	Now initializes zoom variables. */
/*  7-Aug-1991	SNS/CIT	Now saves a copy of the old look up tables to get */
/*			around an OpenWindows bug. */
/* 16-Aug-1991	SNS/CIT	Now initializes the patch window */
/* 20-Aug-1991	SNS/CIT	Now reads in an icon file and forgets gravity */
/* 22-Aug-1991	SNS/CIT	Now initializes the location window */
/* 27-Aug-1991	SNS/CIT	Now initializes the seeing window */
/*  3-Sep-1991	SNS/CIT	Now sets the bitmap cursor to an X and initializes */
/*			the color map window */
/*  4-Sep-1991	SNS/CIT	Now creates the seeing pixmap */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  6-Sep-1991	SNS/CIT	Portability problems from SSL::TENNANT included */
/* 17-Sep-1991	SNS/CIT	All windows now accept key presses */
/*  4-Oct-1991	SNS/CIT	Now creates an xorgc to draw the line for line plots */
/*  7-Oct-1991	SNS/CIT	Now uses the resource database values for the various */
/*			window geometries and the max & min colors */
/*  8-Oct-1991	SNS/CIT	Location and patch window initialization moved into a */
/*			separate file. */
/*  9-Oct-1991	SNS/CIT	Seeing and color map window, LUT, window manager */
/*			hints, and test data initialization and visual */
/*			acquisition moved into separate files. */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT	No longer sets the border pixel. */
/* 25-Nov-1991	SNS/CIT	Now handles separate X & Y zoom factors */
/* 14-Feb-1992	SNS/CIT	Now includes the id in the title */
/* 24-Feb-1992	SNS/CIT	Visual allocation now done in main routine */
/*  6-Mar-1992	SNS/CIT	Modified to work better with non-default visuals */
/*  8-Apr-1992	SNS/CIT	Now has a better window title for Keck version */
/*  9-Apr-1992	SNS/CIT	No longer uses minimum or maximum dimensions for the */
/*			bitmap window */
/* 10-Apr-1992	SNS/CIT	Now initializes winxoff & winyoff. */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

/* The system include files */
#include <stdio.h>

/* the X Window include files */
#include <X11/Xlib.h>
#include <X11/cursorfont.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"
#include "figdisp.icon"

#ifndef VMS
#include <malloc.h>
#endif

int initbmwin()
{
	XSetWindowAttributes winattr;	/* window attributes */
	Cursor cursor;
	char wintitle[80];	/* the window title */
	Window foo;

	char *malloc();
	void initbmluts();
	void initwmattr();
	void initgc();
	void initbmdata();

	/* initialize the colormap */
	if (initbmcmap()) return(FAIL);

	/* initialize the bitmap graphics wininfo structure */
	bm.height=bm.imheight=res.bmgeo.h;
	bm.width=bm.imwidth=res.bmgeo.w;
	bm.cursx=bm.width>>1;
	bm.cursy=bm.height>>1;
	bm.xoff=bm.yoff=0;
	bm.modlut=0;
	bm.offset=0;
	bm.slope=(BM_COLORS-1)/(double)(bm.colors-1);
	bm.dsc=1.0;
	bm.curxsc=bm.curysc=1.0;
	bm.doff=bm.curxoff=bm.curyoff=0.0;
	bm.xzoom=bm.yzoom=0;		/* power of two */
	bm.invert=0;
	bppix=16;
	bm.winxoff=bm.winyoff=0;

	/* set up the LUT entries (requires bm.slope to be set) */
	initbmluts();

	/* Now we create the image data and structure */
#ifdef lint
	if (malloc(bm.height*bm.width*sizeof(unsigned short)) == NULL)
#else
	if ((rimdat.b16=(unsigned short *)malloc(
		bm.height*bm.width*sizeof(unsigned short))) == NULL)
#endif
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}

	/* create a image for the info inside the window */
	if ((bm.imdat=(unsigned char *)malloc(bm.width*bm.height)) == NULL)
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return(FAIL);
	}

	bm.image=XCreateImage(display, bitvisual, 8, ZPixmap, 0,
		(char *)bm.imdat, bm.width, bm.height, 8, 0);

	/* create a window for bitmap graphics */
	winattr.background_pixel=bm.pix[0];
	winattr.border_pixel=bm.pix[0];
	winattr.colormap=bitcmap;
	bm.win=XCreateWindow(display, RootWindow(display,screen), res.bmgeo.x,
		res.bmgeo.y, bm.width, bm.height, BORDER_WIDTH, (int)bitdepth,
		InputOutput, bitvisual, CWBackPixel | CWColormap| CWBorderPixel,
		&winattr);

	bm.mapped=0;	/* it's not mapped yet */

	bm.icon=XCreateBitmapFromData(display, bm.win, figdisp_bits,
		figdisp_width, figdisp_height);

	/* set up all the window manager hints */
#ifdef KECK
	(void)sprintf(&wintitle[0],"Keck bitmap graphics");
#else
	(void)sprintf(&wintitle[0],"bitmap graphics #%d",res.id);
#endif
	initwmattr(bm, &wintitle[0], "bitmap", &res.bmgeo);

	/* now set the cursor to the X */
	cursor = XCreateFontCursor(display, XC_plus);
	XDefineCursor(display, bm.win, cursor);

	/* we need to listen for new data */
	XSelectInput(display,bm.win,PropertyChangeMask | ExposureMask |
		StructureNotifyMask | ButtonPressMask | ButtonReleaseMask |
		PointerMotionMask | KeyPressMask);

	/* set up the other bitmap windows */
	if (initlocwin() || initpatchwin() || initseewin() || initcmapwin())
		return(FAIL);

	/* set up the graphics contexts */
	initgc();

	/* set up the phony data */
	initbmdata();

	/* now update the location window */
	if (updateloc()) return(FAIL);

	return(SUCCEED);
}
