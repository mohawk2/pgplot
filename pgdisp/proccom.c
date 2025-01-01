/* The proccom routine processes the data from the program controlling the */
/* display server.  Each message can contain more than one command.  If a */
/* values of 0 is passed for len than this is assumed to be the last time */
/* this routine is to be called in this particular incrementat transfer. */
/* Return Values: */
/* SUCCEED	If everything went fine */
/* BADCOM	If an invalid command was found */
/* INCCOM	If an incomplete command was sent.  This error code is only  */
/*		returned if this is the last transfer in an incremental */
/*		transfer. */
/* FAIL		If an X Window command fails */
/* MALLOC_ERR	If a malloc fails */

/* Sam Southard, Jr. */
/* Created: 6-Nov-1990 */
/* 15-Nov-1990	SNS/CIT	Modified to support commands being split across two */
/*			messages.  New format for TOK_DRAW_LINE and */
/*			TOK_DRAW_POLY (see commands.h) used.  All arguments */
/*			except for the pixel values for the bitmap write */
/*			command are now shorts. */
/* 16-Nov-1990	SNS/CIT	Code for TOK_SET_LG_SIZE, TOK_DRAW_DOT, */
/*			TOK_FILL_POLY, and TOK_FILL_RECT added. */
/* 19-Nov-1990	SNS/CIT	Code for TOK_LG_MAX_DIM, TOK_LG_SCALE, TOK_LG_CURS, */
/*			and TOK_LG_DEF_SIZE added. */
/*  8-Dec-1990	SNS/CIT	Now only does an XCopyArea once per call to proccom. */
/* 10-Dec-1990	SNS/CIT	Now uses wininfo structure.  VMS  changes merged in. */
/*			Now allows for 5 pixel border around graphics. */
/* 11-Dec-1990	SNS/CIT	TOK_RESET no longer clears the screen. */
/* 12-Dec-1990	SNS/CIT	Now handles bitmap graphics commands. */
/* 17-Dec-1990	SNS/CIT	Now uses the writeluts routine to handle the new */
/*			format for the TOK_SET_BM_LUT command. */
/*  2-Apr-1991	SNS/CIT	Image capbilities #ifdef'ed out for inclusion in the */
/*			Lick xvideo program. */
/*  3-Apr-1991	SNS/CIT	Image capabilities modified for use in Lick program. */
/*  8-Apr-1991	SNS/CIT	Modified to simply print an error message (instead of */
/*			exiting) when a bad command is encountered. */
/* 18-Apr-1991	SNS/CIT	Image LUT capabilities modified for Lick Xvideo. */
/* 20-Apr-1991	SNS/CIT	Cursor capabilities modified for Lick Xvideo. */
/* 22-Apr-1991	SNS/CIT	Now handles the TOK_SET_BM_CSCALE and */
/*			TOK_BM_SET_DSCALE commands. */
/*  6-May-1991	SNS/CIT	Added figim variable to help with color bar. */
/* 10-May-1991	SNS/CIT	Modified to be shared between Xvideo and pgdisp */
/* 17-Jun-1991	SNS/CIT	Now handles the TOK_BM_FLUSH command. */
/*			TOK_SET_BM_DSCALE modified to get a float param. */
/* 31-Jul-1991	SNS/CIT	Modified to be shared with fastdisp. */
/*  1-Aug-1991	SNS/CIT	Modified to handle the TOK_BM_LINE command. */
/*  7-Aug-1991	SNS/CIT	Now raises line graphics window when appropriate */
/* 14-Aug-1991	SNS/CIT	No longer contains hooks for xvideo */
/* 23-Aug-1991	SNS/CIT	Now updates the location window */
/*  5-Sep-1991	SNS/CIT	Modified to lint as cleanly as possible */
/*  8-Oct-1991	SNS/CIT	Globals moved into globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 11-Feb-1992	SNS/CIT	Fixed bug causing line graphics data to be lost when */
/*			the user resized while a PGPLOT program was being run */
/* 14-Feb-1992	SNS/CIT	Now clears the line graphics window to lg.pix[0] not */
/*			BlackPixel */
/* 25-Feb-1992	SNS/CIT	Now handles line graphics windows with > 16 colors */
/* 26-Feb-1992	SNS/CIT	Now handles recieving bitmap graphics commands in */
/*			pgdisp and ignores commands in a buffer after an */
/*			unknown command has occurred. */
/*  5-Mar-1992	SNS/CIT	Line graphics now works on read-only & grey-scale */
/*			visuals. */
/*  9-Apr-1992	SNS/CIT	No longer has minimum and maximum window sizes in the */
/*			wininfo structure. */
/* 10-Apr-1992	SNS/CIT	Now knows how to handle lg.winxoff & lg.winyoff */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */
/* 25-Jun-1992	SNS/CIT	SET_BM_LUT now has number of bits per pixel */
/*  9-Jul-1992	SNS/CIT	SET_LG_SIZE now takes the new size as claimed (it had */
/*			been taking the maximum coordinate, which is one */
/*			less) */

/* The system include files */
#include <stdio.h>
#ifdef sun
#include <memory.h>
#endif

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* The program include files */
#include "commands.h"
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

/* A trivial macro */
#define min(x,y) (((x) > (y)) ? (y) : (x))

int proccom(buf,len,retbuf,retbuflen)
short *buf;	/* the buffer of commands and arguments */
int len;	/* the length of the buffer */
short *retbuf;	/* a buffer for return values */
int *retbuflen;	/* the length of retbuf */
{
	static short bufcont[7];	/* the buffer contents while we're */
					/* working on things */
	static int buflen;		/* the return buffer length */
	static int lgcolor;		/* the current color */
	static unsigned int lglinewid;	/* line graphics line width */

	int i,j;	/* silly loop variables */
	int thislut;	/* the current lut entry */
	XColor color;	/* a structure for changing LUTs */
	XPoint *points;	/* for drawing a poly line */
	int minx,maxx,miny,maxy;	/* minimum and maximum x and y for */
					/* updating an effected area */
	static int savedshorts=0;	/* The number of saved shorts */
	static short *olddata;		/* the saved data */
	int usedolddata=0;	/* if we use data from a previous call */
	short *bothbuf;		/* buffer for sum of the two data parts */
		/* dimensions for updating screen */
	int cminx=lg.width,cmaxx=0,cminy=lg.height,cmaxy=0;
	Pixmap temppixmap;	/* a pixmap used to transfer from old size to */
				/* new size */
	int npix;		/* the number of pixels to write */
	short *sptr;		/* A pointer to a short */
#ifdef PGDISP
	int badbmcom=0;		/* if a bitmap command is recieved in PGDISP */
	int bppix,startx,starty,nline;	/* vars to decode commands */
#else
	unsigned short *im16;	/* used to access the image */
	unsigned char *im8;	/* used to access the 8-bit image */
	/* note that this should be short even in FASTDISP */
	short *tempim;		/* a pointer to  new image structure */
#endif
	void initlgluts();	/* initialize the LUTs */
	void pgscurs();		/* set the line graphics cursor position */
	void returnbuf();	/* return the answer to the client */
	void bmscurs();		/* set the bitmap graphics cursor position */
	void redrawim();	/* redraw the bitmap graphics window */
	void drawline();	/* draw a line in the bitmap window */
	Pixmap XCreatePixmap();

	char *malloc();

	if (!len & savedshorts)
	{ /* an incomplete command was sent! */
		savedshorts=0;
		free((char *)olddata);
		return(INCCOM);
	}
	if (savedshorts)
	{ /* There's some data left over from the old command */
		usedolddata=1;
#ifdef lint
		bothbuf=NULL;
		if (!(malloc(
#else
		if (!(bothbuf=(short *)malloc(
#endif
			(unsigned)(savedshorts+len)*sizeof(short))))
		{
			(void)fprintf(stderr,MSG_MALLOC);
			return(MALLOC_ERR);
		}
		/* first old data */
		(void)memcpy((char *)bothbuf,(char *)olddata,
			savedshorts*sizeof(short));
		/* then new data */
		(void)memcpy((char *)(bothbuf+savedshorts),(char *)buf,
			len*sizeof(short));
		len += savedshorts;
		buf=bothbuf;
		free((char *)olddata);
		savedshorts=0;
	}

	while (len-- > 0)
	{ /* until there are no more commands to process */
		switch(*buf++)
		{
		case RESET:	/* reset the display server */
			initlgluts();
			lgcolor=1;
			lglinewid=0;
			XSetLineAttributes(display, linegc, lglinewid,
				LineSolid, CapRound, JoinRound);
			break;
		case SHOW_LG_WIN:	/* conceal/reveal line graphics */
			if (!len)
			{ /* save this data for the next time around */
				savedshorts=1;
				--buf;
				break;
			}
			if (*buf++)
			{
				XMapWindow(display,lg.win);
				lg.mapped=1;
				XRaiseWindow(display,lg.win);
			} else {
				XUnmapWindow(display,lg.win);
				lg.mapped=0;
			}
			--len;
			break;
		case SET_LG_LUT:	/* set line graphics LUTs */
			if (len < 2)
			{ /* save the data */
				savedshorts=len+1;
				--buf;
				break;
			}
			thislut= *buf++;
			i= *buf++;
			len -= 2;
			if (len < 3*i)
			{
				savedshorts=len+3;
				buf -= 3;
				break;
			}
			color.flags = DoRed | DoGreen | DoBlue;
			while (thislut < lg.colors && i > 0)
			{
				color.pixel=lg.pix[thislut];
				color.red= (unsigned short)*buf++;
				color.green= (unsigned short)*buf++;
				color.blue= (unsigned short)*buf++;
				if (lg.bw)
				{
					color.red=0.30*color.red+
						0.59*color.green+
						0.11*color.blue;
					color.blue=color.green=color.red;
				}
				if (!lg.ro) XStoreColor(display, linecmap,
					&color);
				else {
					XAllocColor(display, linecmap, &color);
					lg.pix[thislut]=color.pixel;
					/* Did we change the foreground? */
					if (thislut == lgcolor)
						XSetForeground(display, linegc,
							lg.pix[lgcolor]);
					/* did we change the background? */
					if (thislut == 0)
						XSetBackground(display, linegc,
							0);
				}
				len -= 3;
				++thislut;
				--i;
			}
			/* eat up any extra LUT entries */
			buf += i*3;
			len -= i*3;
			break;
		case LG_CURS:	/* set and get cursor location */
			if (len < 2)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			/* find out where the cursor is now */
			bufcont[0]=LG_CURS;
			bufcont[1]= *buf++;
			bufcont[2]= *buf++;
			buflen=4;
			/* position the cursor */
			pgscurs(bufcont[1],bufcont[2]);
			/* is an event already there? */
			if (pggcurs(&bufcont[0]))
			{ /* There's already an event there */
				returnbuf(&bufcont[0],4,srcwin);
				buflen=0;
			}
			len -= 2;
			break;
		case SET_LG_COL:	/* set the line graphics color index */
			if (!len)
			{
				savedshorts=1;
				--buf;
				break;
			}
			lgcolor= *buf++;
			if (lgcolor >= lg.colors) lgcolor=1;
			XSetForeground(display, linegc, lg.pix[lgcolor]);
			--len;
			break;
		case DRAW_LINE:	/* draw a line */
			if (len < 4)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			minx= *buf++;
			miny= *buf++;
			maxx= *buf++;
			maxy= *buf++;
			XDrawLine(display, lg.pixmap, linegc, minx, miny, maxx,
				maxy);
			if (minx > maxx)
			{ /* swap for the XCopyArea */
				i=minx;
				minx=maxx;
				maxx=i;
			}
			if (miny > maxy)
			{ /* swap for the XCopyArea */
				i=miny;
				miny=maxy;
				maxy=i;
			}
			if (--minx < cminx) cminx=minx;
			if (++maxx > cmaxx) cmaxx=maxx;
			if (--miny < cminy) cminy=miny;
			if (++maxy > cmaxy) cmaxy=maxy;
			len -= 4;
			break;
		case DRAW_POLY:	/* draw a poly line */
			if (!len)
			{
				savedshorts=1;
				--buf;
				break;
			}
			i= *buf++;
			/* se if there's enough data */
			if (--len < i*2)
			{
				savedshorts=len+2;
				buf -= 2;
			}
#ifdef lint
			points=NULL;
			if (!malloc((unsigned)i*sizeof(XPoint)))
#else
			if (!(points=(XPoint *)malloc(
				(unsigned)i*sizeof(XPoint))))
#endif
			{
				(void)fprintf(stderr,MSG_MALLOC);
				return(MALLOC_ERR);
			}
			j=0;
			minx=lg.width;
			maxx=0;
			miny=lg.height;
			maxy=0;
			while (i-- > 0)
			{
				if (*buf < minx) minx= *buf;
				if (*buf > maxx) maxx= *buf;
				points[j].x= *buf++;
				if (*buf < miny) miny= *buf;
				if (*buf > maxy) maxy= *buf;
				points[j++].y= *buf++;
				len -= 2;
			}
			XDrawLines(display, lg.pixmap, linegc, points, j,
				CoordModeOrigin);
			free((char *)points);
			if (--minx < cminx) cminx=minx;
			if (++maxx > cmaxx) cmaxx=maxx;
			if (--miny < cminy) cminy=miny;
			if (++maxy > cmaxy) cmaxy=maxy;
			break;
		case CLR_LG_WIN:	/* clear the line graphics window */
			XClearWindow(display,lg.win);
			XSetForeground(display, linegcclear, lg.pix[0]);
			XFillRectangle(display, lg.pixmap, linegcclear, 0, 0,
				lg.width, lg.height);
			cminx=0;
			cminy=0;
			cmaxx=lg.width;
			cmaxy=lg.height;
			break;
		case LG_MAX_DIM:	/* return maximum lg dimensions */
			bufcont[0]=LG_MAX_DIM;
			bufcont[1]=0;
			bufcont[2]=LG_MAX_WIDTH-1;
			/* just so we're not too silly */
			if (bufcont[2] < lg.width-1) bufcont[2]=lg.width-1;
			bufcont[3]=0;
			bufcont[4]=LG_MAX_HEIGHT-1;
			if (bufcont[4] < lg.height-1) bufcont[4]=lg.height-1;
			bufcont[5]=0;
			bufcont[6]=lg.colors-1;
			buflen=7;
			break;
		case LG_SCALE:
			bufcont[0]=LG_SCALE;
			bufcont[1]=DisplayWidthMM(display,screen);
			bufcont[2]=DisplayHeightMM(display,screen);
			bufcont[3]=DisplayWidth(display,screen);
			bufcont[4]=DisplayHeight(display,screen);
			buflen=5;
			break;
		case LG_DEF_SIZE:
			bufcont[0]=LG_DEF_SIZE;
			bufcont[1]=0;
			bufcont[2]=lg.width-1;
			bufcont[3]=0;
			bufcont[4]=lg.height-1;
			buflen=5;
			break;
		case SET_LG_SIZE:	/* set size of line graphics window */
			if (len < 2)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			lg.imwidth= *buf++;
			lg.imheight= *buf++;
			len -= 2;
			/* now we get a pixmap for it.  Resizing the window */
			/* may override this, but if it does we won't lose */
			/* data we obtained in the mean time */
			temppixmap=XCreatePixmap(display,
				RootWindow(display,screen), lg.imwidth,
				lg.imheight, linedepth);
			XSetForeground(display, linegcclear, lg.pix[0]);
			XFillRectangle(display, temppixmap, linegcclear, 0, 0,
				lg.imwidth, lg.imheight);
			XCopyArea(display, lg.pixmap, temppixmap, linegc, 0, 0,
				lg.imwidth, lg.imheight, 0, 0);
			XFreePixmap(display, lg.pixmap);
			lg.pixmap=temppixmap;
			/* clear the window */
			XFillRectangle(display, lg.win, linegcclear, 0, 0,
				lg.width, lg.height, 0, 0);
			lg.winxoff=(lg.width-lg.imwidth)/2;
			lg.winyoff=(lg.height-lg.imheight)/2;
			XCopyArea(display, lg.pixmap, lg.win, linegc, 0, 0,
				lg.imwidth, lg.imheight, lg.winxoff,
				lg.winyoff);
			XResizeWindow(display,lg.win,lg.imwidth,lg.imheight);
			break;
		case DRAW_DOT:
			if (len < 2)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			minx= *buf++;
			miny= *buf++;
			maxy=lglinewid>>1;
			if (lglinewid < 2)
			{
				XDrawPoint(display, lg.pixmap, linegc, minx,
					miny);
				if (--minx < cminx) cminx=minx;
				if (--miny < cminy) cminy=miny;
				minx += 2;
				miny += 2;
				if (minx > cmaxx) cmaxx=minx;
				if (miny > cmaxy) cmaxy=miny;
			} else {
				XFillArc(display, lg.pixmap, linegc, minx+maxy,
					miny+maxy, lglinewid, lglinewid, 0,
					23040);
				if (--minx < cminx) cminx=minx;
				if (--miny < cminy) cminy=miny;
				minx += lglinewid+2;
				miny += lglinewid+2;
				if (minx > cmaxx) cmaxx=minx;
				if (miny > cmaxy) cmaxy=miny;
			}
			len -= 2;
			break;
		case FILL_POLY:
			if (!len)
			{
				savedshorts=1;
				--buf;
				break;
			}
			i= *buf++;
			/* se if there's enough data */
			if (--len < i*2)
			{
				savedshorts=len+2;
				buf -= 2;
			}
#ifdef lint
			if (!malloc((unsigned)i*sizeof(XPoint)))
#else
			if (!(points=(XPoint *)malloc(
				(unsigned)i*sizeof(XPoint))))
#endif
			{
				(void)fprintf(stderr,MSG_MALLOC);
				return(MALLOC_ERR);
			}
			j=0;
			minx=lg.width;
			maxx=0;
			miny=lg.height;
			maxy=0;
			while (i-- > 0)
			{
				if (*buf < minx) minx= *buf;
				if (*buf > maxx) maxx= *buf;
				points[j].x= *buf++;
				if (*buf < miny) miny= *buf;
				if (*buf > maxy) maxy= *buf;
				points[j++].y= *buf++;
				len -= 2;
			}
			XFillPolygon(display, lg.pixmap, linegc, points, j,
				Complex, CoordModeOrigin);
			free((char *)points);
			if (--minx < cminx) cminx=minx;
			if (++maxx > cmaxx) cmaxx=maxx;
			if (--miny < cminy) cminy=miny;
			if (++maxy > cmaxy) cmaxy=maxy;
			break;
		case FILL_RECT:
			if (len < 4)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			minx= *buf++;
			miny= *buf++;
			maxx= *buf++;
			maxy= *buf++;
			len -= 4;
			if (minx > maxx)
			{
				i=minx;
				minx=maxx;
				maxx=i;
			}
			if (miny > maxy)
			{
				i=miny;
				miny=maxy;
				maxy=i;
			}
			XFillRectangle(display, lg.pixmap, linegc, minx, miny,
				(unsigned)(maxx-minx), (unsigned)(maxy-miny));
			if (--minx < cminx) cminx=minx;
			if (++maxx > cmaxx) cmaxx=maxx;
			if (--miny < cminy) cminy=miny;
			if (++maxy > cmaxy) cmaxy=maxy;
			break;
		case LG_LINE_WID:
			if (!len)
			{
				savedshorts=1;
				--buf;
				break;
			}
			lglinewid= *buf++;
			XSetLineAttributes(display, linegc, lglinewid,
				LineSolid, CapRound, JoinRound);
			--len;
			break;
		case LG_PIXLINE:
			if (len < 4)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			npix= *buf++;
			minx= *buf++;
			miny= *buf++;
			len -= 3;
			if (len < npix)
			{
				savedshorts=len+4;
				buf -= 4;
				break;
			}
			for (i=0 ; i < npix; ++i)
			{
				XSetForeground(display, linegc,
					lg.pix[*buf++ % lg.colors]);
				XDrawPoint(display, lg.pixmap, linegc, minx++,
					miny);
			}
			len -= npix;
			if (--minx < cminx) cminx=minx;
			if (minx+npix+2 > cmaxx) cmaxx=minx+npix+2;
			if (--miny < cminy) cminy=miny;
			if (miny+2 > cmaxy) cmaxy=miny+2;
			break;
		case SET_LG_CSCALE:
			if (len < 8)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
			/* this is a horrid hack, but it works */
			sptr= (short *)&lg.curxoff;
			*sptr++ = *buf++;
			*sptr = *buf++;
			sptr= (short *)&lg.curxsc;
			*sptr++ = *buf++;
			*sptr = *buf++;
			sptr= (short *)&lg.curyoff;
			*sptr++ = *buf++;
			*sptr = *buf++;
			sptr= (short *)&lg.curysc;
			*sptr++ = *buf++;
			*sptr = *buf++;
			len -= 8;
			if (lg.curxsc == 0.0) lg.curxsc=1.0;
			if (lg.curysc == 0.0) lg.curysc=1.0;
			break;
		case SHOW_BM_WIN:	/* conceal/reveal bitmap graphics */
#ifdef PGDISP
			badbmcom=1;
#endif
			++buf;
			--len;
			/* isn't backwards compatibility nice? */
			break;
		case SET_BM_LUT:	/* set bitmap graphics LUTs */
#ifdef PGDISP
			badbmcom=1;
			if (len < 4)
			{
				savedshorts=len+1;
				--buf;
			} else {
				thislut= *buf++;
				i= *buf++;	/* bits-per-pixel, irrelvant */
				i= *buf++;		/* number affected */
				j= *buf++ & 0x7;	/* the affected luts */
				len -= 3;
				if (j && len < i || !j && len < 3*i)
				{
					savedshorts=len+4;
					buf -= 4;
				} else if (j) {
					len -= i;
					buf += i;
				} else {
					len -= 3*i;
					buf += 3*i;
				}
			}
#else
			savedshorts=writeluts(&buf,&len);
#endif
			break;
		case BM_SET_CURS:	/* set the cursor location */
#ifdef PGDISP
			buf += 2;
			len -= 2;
			badbmcom=1;
#else
			minx= *buf++;
			miny= *buf++;
			bmscurs(minx,miny);
			len -= 2;
#endif
			break;
		case BM_GET_CURS:	/* set and get cursor location */
#ifdef PGDISP
			badbmcom=1;
#else
			/* get cursor location (must be done in mainloop) */
			bufcont[0]=BM_GET_CURS;
			buflen=4;
			if (bmgcurs(&bufcont[0]))
			{ /* There's already an event there */
				returnbuf(&bufcont[0],4,srcwin);
				buflen=0;
			}
#endif
			break;
		case CLR_BM_WIN:	/* clear the bitmap graphics window */
#ifdef PGDISP
			badbmcom=1;
#else
			if (bppix == 16)
			{
				for (i=0, im16= rimdat.b16 ;
				     i < bm.imwidth*bm.imheight ; ++i)
					*im16++ = allcells[0].pixel;
			} else {
				for (i=0, im8= rimdat.b8 ;
				     i < bm.imwidth*bm.imheight ; ++i)
					*im8++ = allcells[0].pixel;
			}
			redrawim(0, 0, bm.imwidth-1, bm.imheight-1);
			(void)updateloc();
#endif
			break;
		case BM_MAX_DIM:	/* return maximum bitmap dimensions */
#ifdef PGDISP
			badbmcom=1;
#else
			/* This has no real relevance since it's only */
			/* dependant on memory, but we need to return */
			/* something. */
			bufcont[0]=BM_MAX_DIM;
			bufcont[1]=0;
			bufcont[2]=BM_MAX_WIDTH-1;
			/* don't be silly */
			if (bufcont[2] < bm.width-1) bufcont[2]=bm.width-1;
			bufcont[3]=0;
			bufcont[4]=BM_MAX_HEIGHT-1;
			if (bufcont[4] < bm.height-1) bufcont[4]=bm.height-1;
			bufcont[5]=0;
			bufcont[6]=BM_COLORS-1;
			buflen=7;
#endif
			break;
		case BM_DEF_SIZE:
#ifdef PGDISP
			badbmcom=1;
#else
			bufcont[0]=BM_DEF_SIZE;
			bufcont[1]=0;
			bufcont[2]=bm.imwidth-1;
			bufcont[3]=0;
			bufcont[4]=bm.imheight-1;
			buflen=5;
#endif
			break;
		case SET_BM_SIZE:	/* set size of image */
#ifdef PGDISP
			badbmcom=1;
#endif
			if (len < 2)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
#ifdef PGDISP
			buf += 3;
			len -= 3;
#else
			minx= *buf++;
			miny= *buf++;
			maxx= *buf++;
			if (maxx != 16 && maxx != 8) 
			{
				(void)fprintf(stderr,
					"Invalid bits-per-pixel (%d)!\n",maxx);
				(void)fprintf(stderr, "Assuming 8\n");
				(void)fprintf(stderr,
				  "Don't be surprised if things don't work\n");
				maxx=8;
			}
			len -= 3;
			if (minx=resizeim(minx,miny,maxx)) return(minx);
#endif
			break;
		case BM_WRITE:
#ifdef PGDISP
			badbmcom=1;
#endif
			if (len < 6)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
#ifdef PGDISP
			bppix= *buf++;
			startx= *buf++;
			starty= *buf++;
			npix= *buf++;
			nline= *buf++;
			len -= 5;
			if (bppix == 16 && len < npix*nline ||
			    len < ((npix*nline+1)>>1))
			{
				savedshorts=len+6;
				buf -= 6;
				break;
			} else if (bppix == 16) {
				buf += npix*nline;
				len -= npix*nline;
			} else {
				buf += ((npix*nline+1)>>1);
				len -= ((npix*nline+1)>>1);
			}
#else
			/* get the image size - writeimage takes care of */
			/* interpreting the rest of the arguments and */
			/* changing buf and len accordingly */
			minx= *buf++;
			--len;
			if (minx == 16) savedshorts=writeimage16(&buf,&len);
			else savedshorts=writeimage(&buf,&len);
#endif
			break;
		case SET_BM_CSCALE:
#ifdef PGDISP
			badbmcom=1;
#endif
			if (len < 6)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
#ifdef PGDISP
			buf += 6;
			len -= 6;
#else
			bm.curxsc= *buf++;
			bm.curxsc /= *buf++;
			bm.curxoff= *buf++;
			bm.curysc= *buf++;
			bm.curysc /= *buf++;
			bm.curyoff= -bm.imheight - *buf++;
			if (bm.curxsc == 0.0) bm.curxsc=1.0;
			if (bm.curysc == 0.0) bm.curysc=1.0;
			len -= 6;
#endif
			break;
		case SET_BM_DSCALE:
#ifdef PGDISP
			badbmcom=1;
#endif
			if (len < 4)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
#ifdef PGDISP
			buf += 4;
			len -= 4;
#else
			/* this is a horrid hack, but it works */
			tempim= (short *)&bm.dsc;
			*tempim++ = *buf++;
			*tempim = *buf++;
			tempim= (short *)&bm.doff;
			*tempim++ = *buf++;
			*tempim = *buf++;
			if (bm.dsc == 0.0) bm.dsc=1.0;
			len -= 4;
#endif
			break;
		case BM_FLUSH:
#ifdef PGDISP
			badbmcom=1;
#endif
			XFlush(display);
			break;
		case BM_LINE:
#ifdef PGDISP
			badbmcom=1;
#endif
			if (len < 5)
			{
				savedshorts=len+1;
				--buf;
				break;
			}
#ifdef PGDISP
			buf += 5;
			len -= 5;
#else
			minx= *buf++;
			miny= *buf++;
			maxx= *buf++;
			maxy= *buf++;
			drawline(minx,miny,maxx,maxy,*buf++);
			len -= 5;
#endif
			break;
		default: /* unknown command */
			(void)printf("Unknown command %d\n",*(buf-1));
			(void)printf("Ignoring rest of command buffer\n");
			(void)fflush(stdout);
			len=0;
			break;
		}
		/* do we need to save any values for the next call? */
		if (savedshorts)
		{
			printf("saving shorts\n");
			len=0;
#ifdef lint
			if (!(malloc(
#else
			if (!(olddata=(short *)malloc(
#endif
				(unsigned)savedshorts*sizeof(short))))
			{
				(void)fprintf(stderr,MSG_MALLOC);
				return(MALLOC_ERR);
			}
			/* copy data */
			for (i=0 ; i < savedshorts ; ++i) *olddata++ = *buf++;
			olddata -= savedshorts;
		}
	}

#ifdef PGDISP
	/* if we got a bitmap graphics command in pgdisp, print a warning */
	if (badbmcom)
	{
		(void)puts("Warning: received bitmap command in line ");
		(void)puts("graphics only mode!\n");
		(void)puts(
			"Should you be running figdisp instead of pgdisp?\n");
	}
#endif

	/* if we used old data, free up the buffer */
	if (usedolddata) free((char *)bothbuf);
	if (!savedshorts && buflen)
	{ /* if there's no incomplete command and we need to send a message */
		for (i=0 ; i < buflen ; ++i) retbuf[i]=bufcont[i];
		*retbuflen=buflen;
		buflen=0;
	}

#ifndef PGDISP
	/* now update the location window */
	(void)updateloc();
#endif

	/* now update the screen if necessary */
	if (lg.mapped && cmaxx >= cminx && cmaxy >= cminy)
	{
		XCopyArea(display, lg.pixmap, lg.win, linegc, cminx, cminy,
			(unsigned)(cmaxx-cminx), (unsigned)(cmaxy-cminy),
			cminx+lg.winxoff, cminy+lg.winyoff);
	}
	return(SUCCEED);
}
