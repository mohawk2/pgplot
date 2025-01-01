/* This routine is the PGPLOT interface to the Figaro/PGPLOT display server. */

/* Sam Southard, Jr. */
/* Created: 19-Nov-1990 */
/* 12-Dec-1990	SNS/CIT	Locking mechanism implemented.  VMS changes merged in */
/* 15-Mar-1991	SNS/CIT	OPCODES 1, 4, and 5 (device name, capabilities, and */
/*			default file name) now returned without checking for */
/*			the existance of the figaro display server. */
/*  2-Apr-1991	SNS/CIT	Modified to offset values from current window height, */
/*			not the maximum, to deal with the changed version of */
/*			the server.  If the user changes the window size */
/*			while a program is running, he loses.  Too bad. */
/*  6-Sep-1991	SNS/CIT	Changes from SSL::TENNANT implemented */
/* 13-Sep-1991	SNS/CIT	Added routine to get the window, so that routines */
/*			can use both TVPCKG & PGPLOT at the same time */
/* 18-Sep-1991	SNS/CIT	Modified so that PGPLOT and TVPCKG can both be run at */
/*			the same time. */
/* 20-Sep-1991	SNS/CIT	Commands no longer begin with TOK_.  Buffer length */
/*			no longer hard-coded in. */
/* 28-Apr-1993. KS/AAO. As a kluge to make this version of x2driv work with */
/*                      the version of Figdisp we have been using, I have */
/*                      added a zero argument to the call to figdisp_opencomm */
/*                      This is a temporary fix until we sort out the proper */
/*                      versions of pgplot and Figdisp */
/*  2-Jan-1998. KS/AAO. Removed expliciti declaration of sprintf(). */
/* 21-Aug-2007. KS/AAO. Added stdlib.h, removed explicit malloc declaration. */

/* The system include files */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* The X Window include files */
#include <X11/Xlib.h>
#include <X11/Xatom.h>

/* The program include files */
#include "commands.h"

#ifdef VMS
#include <descrip.h>
typedef char * caddr_t;
#endif

#define PGDRIVNAME	"XDISP"	/* the name of the driver */

#define DRIVCAPS	"ICNATRNNNN"	/* the PGPLOT device capabilities */
#define INCHTOMM	25.4		/* convert from inches to mm */
#define MAXINTENSE	65535.0		/* maximum intensity of an RGB value */

#ifdef VMS
void x2driv(opcode,rbuf,nbuf,chrdsc,lchr)
int *opcode;	/* The specific PGPLOT function */
float *rbuf;	/* the floating point values */
int *nbuf;	/* number of floats in rbuf */
struct dsc$descriptor_s *chrdsc;	/* VMS passes strings by descriptor */
int *lchr;	/* number of used characters in chr */
#else
void x2driv_(opcode,rbuf,nbuf,chr,lchr,chrlen)
int *opcode;	/* The specific PGPLOT function */
float *rbuf;	/* the floating point values */
int *nbuf;	/* number of floats in rbuf */
char *chr;	/* character data */
int *lchr;	/* number of used characters in chr */
int chrlen;	/* actual fortran length of chr */
#endif
{
	static short *combuf=NULL; 		/* a buffer for commands */
	static int combuflen;		/* the length of the commands buffer */
	static int nextshort=0;		/* place to put the next short */
	static int first=1;		/* if this is the first time */
	static int error=0;		/* if an error occurred */
	static int clear=1;		/* if we should clear the screen */
	static int maxx,maxy,maxcol;	/* maximum x, y, and color */
	static short *polypts;		/* pointer to polygon fill points */
	static int npolypts=0;		/* the number of polygon points */
	static int ptssofar;		/* number of points so far */
	static float xscale,yscale;	/* x and y scales */
	static char lockname[40];	/* the name of the locking atom */
	static int wymax;		/* the current y max of the window */

	int itmp;	/* a temporary integer */

	int i;		/* silly loop variable */
	int min;	/* the number of chars to write */
	short *answer;	/* the return from the display server */
	int anslen;	/* the length of the answer */
#ifdef VMS
	int chrlen=chrdsc->dsc$w_length;
	char *chr=chrdsc->dsc$a_pointer;
#endif

	void figdisp_sendcommand();	/* send a command buffer */
	short *figdisp_getresponse();	/* get a response from the server */
	void figdisp_closecomm();	/* close connection with the server */
	int figdisp_opencomm();		/* open connection with the server */

	itmp=0;
	switch(*opcode)
	{ /* take care of the trivial cases without consulting X */
	case 1:
		min=strlen(PGDRIVNAME);
		if (min > chrlen) min=chrlen;
		for (i=0 ; i < min ; ++i) chr[i]=PGDRIVNAME[i];
		for ( ; i < chrlen ; ) chr[i++]=' ';
		itmp=1;
		break;
	case 4:
		for (i=0 ; i < 10 ; ++i) chr[i]=DRIVCAPS[i];
		itmp=1;
		break;
	case 5:
		min=strlen(PGDRIVNAME);
		if (min > chrlen) min=chrlen;
		for (i=0 ; i < min ; ++i) chr[i]=PGDRIVNAME[i];
		for ( ; i < chrlen ; ) chr[i++]=' ';
		itmp=1;
		break;
	default:
		break;
	}
	if (itmp) return;

	/* if there's been an error just forget about it */
	if (error) return;

	if (first)
	{ /* initialize link */
		if (combuf==NULL)
		{
			if ((combuflen= figdisp_maxbuflen()) < 0)
			{
				printf("Unable to get buffer size!\n");
				error=1;
				return;
			}
			if ((combuf= (short *)malloc(combuflen*sizeof(short))) == NULL)
			{
				printf("Unable to get command buffer!\n");
				error=1;
				return;
			}
		}
		if (!figdisp_opencomm(combuflen,0))  /* 2nd arg added by KS */
		{
			error=1;
			return;
		}
		
		/* find out the server's stats */
		if (nextshort >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=LG_MAX_DIM;
		/* This command requires immediate response */
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		answer=figdisp_getresponse(&anslen);
		if (answer[0] != LG_MAX_DIM || anslen != 7)
		{
			printf("The PGPLOT server is seriously confused!\n");
			error=1;
			return;
		}
		maxx=answer[2];
		wymax=maxy=answer[4];
		maxcol=answer[6];
		XFree((caddr_t)answer);
		combuf[nextshort++]=LG_SCALE;
		/* This command requires immediate response */
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		answer=figdisp_getresponse(&anslen);
		if (answer[0] != LG_SCALE || anslen != 5)
		{
			printf("The PGPLOT server is seriously confused!\n");
			error=1;
			return;
		}
		xscale=INCHTOMM*((float)answer[3])/answer[1];
		yscale=INCHTOMM*((float)answer[4])/answer[2];
		XFree((caddr_t)answer);
		/* make room for this command */
		if (nextshort >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=LG_DEF_SIZE;
		/* This command requires immediate response */
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		answer=figdisp_getresponse(&anslen);
		if (answer[0] != LG_DEF_SIZE || anslen != 5)
		{
			printf("The PGPLOT server is seriously confused!\n");
			error=1;
			return;
		}
		wymax=answer[4];
		XFree((caddr_t)answer);
		first=0;
	}
	switch(*opcode)
	{ /* do the real work */
	case 2:
		rbuf[0]=rbuf[2]=rbuf[4]=0.0;
		rbuf[1]=maxx;
		rbuf[3]=maxy;
		rbuf[5]=maxcol;
		break;
	case 3:
		rbuf[0]=xscale;
		rbuf[1]=yscale;
		rbuf[2]=1.0;
		break;
	case 6:
		/* make room for this command */
		if (nextshort >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=LG_DEF_SIZE;
		/* This command requires immediate response */
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		answer=figdisp_getresponse(&anslen);
		if (answer[0] != LG_DEF_SIZE || anslen != 5)
		{
			printf("The PGPLOT server is seriously confused!\n");
			error=1;
			return;
		}
		for (i=0 ; i < 4 ; ++i) rbuf[i]=answer[i+1];
		wymax=answer[4];
		XFree((caddr_t)answer);
		break;
	case 7:
		rbuf[0]=1.0;
		break;
	case 8:
	case 19:
	case 23:
		printf("OOPS - unsupported call %d!\n",*opcode);
		break;
	case 18:
		break;
	case 9:
		if (nextshort+2 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=SHOW_LG_WIN;
		combuf[nextshort++]=1;
		combuf[nextshort++]=RESET;
		rbuf[0]=1.0;
		rbuf[1]=1.0;
		if (rbuf[2] != 0.0) clear=0;
		else clear=1;
		break;
	case 10:
		/* terminte communications with the server */
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		figdisp_closecomm();
		first=1;
		break;
	case 11:
		/* make room for this command */
		if (nextshort+2 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=SET_LG_SIZE;
		combuf[nextshort++]=rbuf[0];
		wymax=combuf[nextshort++]=rbuf[1];
		--wymax;
		if (clear)
		{
			if (nextshort >= combuflen)
			{
				figdisp_sendcommand(&combuf[0],nextshort);
				nextshort=0;
			}
			combuf[nextshort++]=CLR_LG_WIN;
		}
		break;
	case 12:
		/* make room for this command */
		if (nextshort+4 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=DRAW_LINE;
		combuf[nextshort++]=rbuf[0];
		combuf[nextshort++]=wymax-rbuf[1];
		combuf[nextshort++]=rbuf[2];
		combuf[nextshort++]=wymax-rbuf[3];
		break;
	case 13:
		/* make room for this command */
		if (nextshort+2 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=DRAW_DOT;
		combuf[nextshort++]=rbuf[0];
		combuf[nextshort++]=wymax-rbuf[1];
		break;
	case 14:
		/* make room for this command */
		if (rbuf[0] != 1.0)
		{
			if (nextshort >= combuflen)
			{
				figdisp_sendcommand(&combuf[0],nextshort);
				nextshort=0;
			}
			combuf[nextshort++]=CLR_LG_WIN;
		}
		break;
	case 15:
		/* make room for this command */
		if (nextshort+1 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=SET_LG_COL;
		combuf[nextshort++]=rbuf[0];
		break;
	case 16:
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		break;
	case 17:
		/* make room for this command */
		if (nextshort+2 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=LG_CURS;
		combuf[nextshort++]=rbuf[0];
		combuf[nextshort++]=wymax-rbuf[1];
		/* This command requires immediate response */
		figdisp_sendcommand(&combuf[0],nextshort);
		nextshort=0;
		answer=figdisp_getresponse(&anslen);
		if (answer[0] != LG_CURS || anslen != 4)
		{
			printf("The PGPLOT server is seriously confused!\n");
			error=1;
			return;
		}
		rbuf[0]=answer[1];
		rbuf[1]=wymax-answer[2];
		if (answer[3] & 0xFF00)
		{ /* it was a button press */
			switch(answer[3] & 0xFF)
			{
			case 0:
				chr[0]='A';
				break;
			case 1:
				chr[0]='D';
				break;
			default:
				chr[0]='X';
				break;
			}
		} else chr[0]=answer[3] & 0xFF;
		XFree((caddr_t)answer);
		break;
	case 20:
		/* is this the first call? */
		if (!npolypts)
		{
			npolypts=rbuf[0];
			if (npolypts*2+2 > combuflen)
			{
				printf("Too many polygon points!\n");
				error=1;
				return;
			}
			if ((polypts=(short *)malloc(
				(unsigned)2*npolypts*sizeof(short)))
			    == (short *)NULL)
			{
				printf("No memory for polygon points!\n");
				error=1;
				return;
			}
			ptssofar=0;
		} else {
			polypts[ptssofar<<1]=rbuf[0];
			polypts[(ptssofar<<1)+1]=wymax-rbuf[1];
			if (++ptssofar == npolypts)
			{ /* finished */
				if (nextshort+1+(npolypts<<1) >= combuflen)
				{
					figdisp_sendcommand(&combuf[0],
						nextshort);
					nextshort=0;
				}
				combuf[nextshort++]=FILL_POLY;
				combuf[nextshort++]=npolypts;
				ptssofar <<= 1;
				for (i=0 ; i < ptssofar ; )
					combuf[nextshort++]=polypts[i++];
				free((char *)polypts);
				npolypts=0;
			}
		}
		break;
	case 21:
		if (nextshort+5 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=SET_LG_LUT;
		combuf[nextshort++]=rbuf[0];
		combuf[nextshort++]=1;
		combuf[nextshort++]=rbuf[1]*MAXINTENSE;
		combuf[nextshort++]=rbuf[2]*MAXINTENSE;
		combuf[nextshort++]=rbuf[3]*MAXINTENSE;
		break;
	case 22:
		if (nextshort+1 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=LG_LINE_WID;
		combuf[nextshort++]=xscale*0.005*rbuf[0];
		break;
	case 24:
		if (nextshort+4 >= combuflen)
		{
			figdisp_sendcommand(&combuf[0],nextshort);
			nextshort=0;
		}
		combuf[nextshort++]=FILL_RECT;
		combuf[nextshort++]=rbuf[0];
		combuf[nextshort++]=wymax-rbuf[1];
		combuf[nextshort++]=rbuf[2];
		combuf[nextshort++]=wymax-rbuf[3];
		break;
	default:
		printf("Unknown PGPLOT OPCODE!\n");
		error=1;
		break;
	}
	return;
}
