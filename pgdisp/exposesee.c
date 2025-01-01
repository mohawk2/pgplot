/* The exposesee routine updates the seeing window.  The seeing window is so */
/* small that we might as well simply update the entire window */

/* Sam Southard, Jr. */
/* Created: 4-Sep-1991 */
/* Modification History: */
/*  8-Oct-1991	SNS/CIT	Globals moved into globals.h */

#include "figdisp.h"
#include "globals.h"

void exposesee()
{
	/* very simply */
	XCopyArea(display, seeing.pixmap, seeing.win, bitgc, 0, 0,
		(unsigned)seeing.width, (unsigned)seeing.height, 0, 0);
	
	return;
}
