/* This routine is the main routine for the Figaro X Window display server. */
/* The initial features will include the initial line graphics capabilites */
/* as descried in the SPECS document. */

/* Sam Southard, Jr. */
/* Created: 31-Jul-1991 (from pgdisp) */
/* Modification History: */
/* 14-Aug-1991	SNS/CIT	No longer contains hooks for xvideo */
/* 20-Aug-1991	SNS/CIT	Now contains definition for patch */
/* 22-Aug-1991	SNS/CIT	Now contains definition for location window */
/* 27-Aug-1991	SNS/CIT	Now contains definition for seeing window */
/*  3-Sep-1991	SNS/CIT	Now contains definition for color map window */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Modified to clean up the wininfo structure.  Global */
/*			variables moved into globals.h */
/* 25-Nov-1991	SNS/CIT	Now prints a short message if showhelp is defined. */
/* 27-Nov-1991	SNS/CIT	Modified to lint cleanly */
/* 24-Feb-1992	SNS/CIT	Now gets visuals for bitmap and line graphics */
/*  6-Mar-1992	SNS/CIT	compile-time DEBUG option added */
/*  8-Apr-1992	SNS/CIT	Now maps the bitmap window with XMapRaised. */

/* Wish list: */
/*	Add a command line flag to ignore possible presence of a lock. */
/*	Get rid of global variables. */

/* system include files */
#include <stdio.h>

/* X Window include files */
#include <X11/Xlib.h>

/* Program include files */
#include "figdisp.h"
#include "messages.h"

#ifdef lint
Display *XOpenDisplay(name)
char *name;
{
	(void)printf(name);
	return((Display *)0);
}
#endif

#define DEFINE_GLOBALS
#include "globals.h"

int main(argc,argv)
int argc;
char **argv;
{
	Display *XOpenDisplay();

	void cleanup();	/* clean up before exiting */
	void printhelp();	/* print a help message */
	void parsedisp();	/* parse command line options */
	void mergeops();	/* merge options from all sources */
	void extractops();	/* extract options into program readable form */

	/* Initialize stuff for the resource manager */
	XrmInitialize();

	/* parse the command line options */
	parsedisp(&argc, argv);

	screen=DefaultScreen(display);

	/* get server defaults, program defaults, and .Xdefaults merged */
	/* in with the command line */
	mergeops();

	/* extract the options into a program readable form */
	extractops();

#ifdef DEBUG
	XSynchronize(display, True);
#endif

	/* extractops might not have been able to get a default font */
	if (res.textfont == NULL)
	{
		(void)fprintf(stderr,MSG_FONTLOAD);
		return(FAIL);
	}

	/* Set up the resource/locking mechanism and the Atoms needed to */
	/* communicate with applications and initialize the window */
	if (initlock() || getvisuals() || initlgwin() || initbmwin())
		return(FAIL);

	/* Map the bitmap graphics window.  The line graphics window will */
	/* be mapped when needed. */
	XMapRaised(display,bm.win);
	bm.mapped=1;

	if (res.showhelp)
	{
		(void)printf(
			"This program has many features.  Press %s to see a\n",
			XKeysymToString(res.keys[HELP]));
		(void)printf(
			"list of used keystrokes.  Consult the user's guide\n");
		(void)printf(
			"for information on how to change the keys used to\n");
		(void)printf(
			"perform each function, as well as other ways to\n");
		(void)printf("customize figdisp.\n");
	}


	/* process events from the applications */
	(void)mainloop();

	/* clean up the windows */
	cleanup();

	/* note that we still need to clean up misc. things */

	return(SUCCEED);
}
