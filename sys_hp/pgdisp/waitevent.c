/* The waitevent routine waits until there are events in the queue. If the */
/* program which was feeding us data goes away then it grabs ownership of the */
/* selection so that the next program will be able to access things. */
/* Return Value: */
/* 0		The program feeding us data died. */
/* selset	An X event happened. */

/* Sam Southard, Jr. */
/* Created: 12-Dec-1990 from mainloop.c */
/* 13-Dec-1990	SNS/CIT	Now calls writeimage to clean up its state if the */
/*			connection goes away */
/* 15-Aug-1991	SNS/CIT	No longer includes hooks for xvideo */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */

/* The system include files */
#include <stdio.h>

/* The X11 include files */
#include <X11/Xlib.h>

/* The program include files */
#include "figdisp.h"
#include "globals.h"
#include "messages.h"

/* Choose one of the following to select either good response time or low */
/* system load, keeping in mind what your system provides.  The default is */
/* BUSY, and will work on all systems, regardless of OS. */
/* #define	BUSY	/* Busy loop while waiting for events.  This loads */
			/* system but gives quickest response time and is */
			/* ompletely portable (since it does nothing). */
#define		SLEEP	/* Sleep for SLEEP_TIME (defined below) seconds */
			/* between successive queue examinations.  This is */
			/* much nicer on the system load but gives poor */
			/* response time. */
/* #ifdef VMS
#define		WAIT	/* use the LIB$WAIT call */
/* #else
#define		USLEEP	/* Sleep for USLEEP_TIME MICROseconds.  This is both */
			/* nice on the CPU and gives decent response time, */
			/* but is not available on some systems (it requires */
			/* the usleep(3) call).  Definately the best choice */
			/* if possible, assuming a wise choice of USLEEP_TIME */
			/* values. */
/* #endif  */

#define SLEEP_TIME	1	/* Number of seconds to sleep between queue */
				/* examinations.  Only valid if SLEEP is */
				/* chosen. */
#define USLEEP_TIME	100000	/* Number of MICROseconds to sleep between */
				/* queue examinations.  Only valid if USLEEP */
				/* is chosen. */

int waitevent(selset)
int selset;	/* if the selection is currently owned */
{
#ifdef VMS
	float waittime;

	waittime=(1e-6)*USLEEP_TIME;
#endif

	while(!QLength(display))
	{
		/* if the selection is not owned we need to grab it again */
		if (XGetSelectionOwner(display,selatom) == None)
		{
			XSetSelectionOwner(display,selatom,lg.win,CurrentTime);
			if (XGetSelectionOwner(display,selatom) != lg.win)
			{
				(void)fprintf(stderr,MSG_BADSELOWN);
				return(FAIL);
			}
			XUngrabKeyboard(display,CurrentTime);
			(void)proccom((short *)NULL,0,(short *)NULL,(int *)0);
			return(0);	/* the selection owner was reset */
		}
		/* pause a while */
#ifdef SLEEP
		sleep(SLEEP_TIME);
#endif
#ifdef USLEEP
		usleep(USLEEP_TIME);
#endif
#ifdef WAIT
		lib$wait(&waittime);
#endif
	}
	return(selset);	/* the selection owner was not reset */
}
