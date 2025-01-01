/* The initlock routine initializes the locking mechanism and all atoms */
/* needed for Figaro/PGPLOT applications to communicate with the display */
/* server. */
/* Return Values: */
/* ALREADY_RUNNING	A copy of the display server is already running on */
/*			the requested screen. */
/* FAIL			The initialization failed for some reason. */
/* SUCCEED		Everything went fine. */

/* Sam Southard, Jr. */
/* Created: 01-Nov-1990 */
/* Modification History: */
/* 10-May-1991	SNS/CIT	Modified for use in the Lick Xvideo program */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for Xvideo */
/*  6-Sep-1991	SNS/CIT	Changes from SSL::TENNANT included */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */

/* The system include files */
#include <stdio.h>

/* The X windows include files */
#include <X11/Xlib.h>

/* The program include files */
#include "commands.h"
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#define HPUX

int initlock()
{
	char lockatomstr[MAXSTRLEN];	/* the name of the locking atom */

#ifndef _AIX
#ifndef HPUX
	char *sprintf();
#endif
#endif

	/* here we check to make sure that no other copy of this */
	/* program is running on the selected display. */

	(void)sprintf(&lockatomstr[0],"%s_%d",NAME_PROG,screen);
	if ((lock=XInternAtom(display,&lockatomstr[0],False)) == None)
	{
		(void)fprintf(stderr,MSG_BADLOCKATOM);
		return(FAIL);
	}

	/* if the atom is owned by someone other than this program */
	/* than another copy of this program is running and we */
	/* should go away */

	if (XGetSelectionOwner(display,lock) != None)
	{ /* if the atom is owned, another display process is running */
		(void)fprintf(stderr,MSG_ALREADYRUNNING);
		return(ALREADY_RUNNING);
	}

	/* now we set up the atom for incremental transfers */

	if ((incrtype=XInternAtom(display,NAME_INCRATOM,False)) == None)
	{
		(void)fprintf(stderr,MSG_BADINCRATOM);
		return(FAIL);
	}

	/* now we set up the atom for data */

	if ((dataatom=XInternAtom(display,NAME_DATAATOM,False)) == None)
	{
		(void)fprintf(stderr,MSG_BADDATAATOM);
		return(FAIL);
	}

	/* now we set up the atom to use for selections */

	if ((selatom=XInternAtom(display,NAME_SELATOM,False)) == None)
	{
		(void)fprintf(stderr,MSG_BADSELATOM);
		return(FAIL);
	}

	return(SUCCEED);
}
