/* The printhelp routine prints a help message describing the functions of */
/* each of the function keys and mouse buttons.  There is currently no */
/* mechanism for actually checking the definition, so be sure that the text */
/* matches the definitions in mainloop.c. */

/* Sam Southard, Jr. */
/* Created: 4-Aug-1991	*/
/*  5-Sep-1991	SNS/CIT	Updated to include all features. */
/* 17-Sep-1991	SNS/CIT	Updated to reflect new way of dismissing seeing and */
/*			patch windows */
/* 18-Sep-1991	SNS/CIT	Now longer says "in the bitmap window only" */
/*  1-Oct-1991	SNS/CIT	Now knows about row/column/general line plots */
/*  7-Oct-1991	SNS/CIT	Now handles the user re-defining the keys */
/*  8-Oct-1991	SNS/CIT	Globals moved into globals.h */
/* 25-Nov-1991	SNS/CIT	Modified for separate X & Y zoom factors */
/* 24-Jun-1992	SNS/CIT	Now handles histogram equalization key */ 

#include "figdisp.h"
#include "globals.h"

#include <stdio.h>

static struct {
	int key;	/* the associated key (-1 for none) */
	char str[90];	/* the help message */
} help[] = {
{-1,"The following keys are defined (F11 = L1 & F21 = R1 on Sun keyboards):\n"},
{ZOOMIN,	"\t= Zoom in\n"},
{ZOOMNORM,	"\t= Go to normal zoom factor\n"},
{ZOOMOUT,	"\t= Zoom out\n"},
{ZOOMXIN,	"\t= Zoom in X coordinates\n"},
{ZOOMXOUT,	"\t= Zoom out X coordinates\n"},
{ZOOMYIN,	"\t= Zoom in Y coordinates\n"},
{ZOOMYOUT,	"\t= Zoom out Y coordinates\n"},
{HELP,		"\t= Help (display this message)\n"},
{CURSOR,	"\t= Toggle cursor display\n"},
{RECENTER,	"\t= Recenter image\n"},
{SHOWLOC,
  "\t= Toggle display of window showing the location of the main window in\n"},
{-1,		"\t  the entire image\n"},
{QUIT,		"\t= Quit\n"},
{SHOWCM,	"\t= Show a window containing the color map\n"},
{SHOWPAT,
  "\t= Toggle display of window showing the pixel values around the cursor\n"},
{ROW,		"\t= Produce a row plot in the line graphics window\n"},
{IMPS,		"\t= Print the image\n"},
{WINPS,		"\t= Print the visible portion of the image\n"},
{INVERT,	"\t= Toggle color map inversion\n"},
{SEEING,
  "\t= Toggle display of window showing the centroid and FWHM of the star\n"},
{-1,	"\t  near the cursor\n"},
{COL,		"\t= Produce a column plot in the line graphics window\n"},
{DECSLIT,	"\t= Decrease number of pixels averaged for a line plot\n"},
{INCSLIT,	"\t= Increase number of pixels averaged for a line plot\n"},
{RESSLIT,	"\t= Reset number of pixels averaged for a line plot\n"},
{INHIBIT,	"\t= Inhibit interpretation of all other keys until this\n"},
{-1,	"\t  key is pressed again.\n"},
{HISTOGRAM,	"\t= Toggle use of histogram equalization.\n\n"},
{-1, "Click on the left mouse button in any window except the pixel values\n"},
{-1, "window to move the point under the cursor to the center of the main\n"},
{-1, "window.\n\n"},
{-1, "Press and hold the right mouse button to manipulate the color LUTs.\n"},
{-1, "The X position of the cursor controls the offset and the Y position\n"},
{-1, "controls the slope.  Release the mouse button within 10 pixels of the\n"},
{-1, "press point to reset the LUT offset and slope.  This has no effect\n"},
{-1, "when histogram equalization is being used.\n\n"},
{-1, "Press the middle mouse button on a point and release it on another\n"},
{-1, "point to produce a line plot of the data values between the two points."},
{-1, "\n\n"},
{-1, "All windows except the location window may be resized arbitrarily.\n"},
{-1, "The location window can be resized within limits, and the program\n"},
{-1, "will often adjust the user defined size.  The line graphics window\n"},
{-1, "should not be resized while it is being modified.\n\n"},
{SHOWPAT, " or "},
{SEEING, " pressed in the main window or in the location window will cause\n"},
{-1, "the pixel values or seeing window do appear.  The window may be\n"},
{-1, "dismissed by the same button inside that window.  All other functions\n"},
{-1, "work from all windows ("},
{ZOOMIN, " always zooms in).\n\n"},
{-1, "All transformations (zoom, resize window) do not move the point at\n"},
{-1, "the center of the window, unless that would cause the entire window\n"},
{-1, "to be empty.\n\n"},
};

void printhelp()
{
	int i=0;

	for (i=0 ; i < sizeof(help)/sizeof(help[0]) ; ++i)
	{
		if (help[i].key > -1)
			(void)fputs(XKeysymToString(res.keys[help[i].key]),
				stdout);
		(void)fputs(help[i].str,stdout);
	}

	return;
}
