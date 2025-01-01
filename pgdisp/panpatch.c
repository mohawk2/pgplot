/* The panpatch routine pans the patch window to set the specified point to */
/* the center of the window.  */

/* Sam Southard, Jr. */
/* Created: 21-Aug-1991 */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */

#include "figdisp.h"
#include "globals.h"

void panpatch(event)
XButtonEvent event;
{
	int dx, dy;	/* the change in x & y */

	void updatepatch();	/* update the patch window */

	dx= event.x - (patch.width>>1);
	dy= event.y - (patch.height>>1);

	/* the center is actually too far (we have labels) */
	dx -= 4*res.textfont->max_bounds.width;
	dy -= (res.textfont->ascent+res.textfont->descent >> 1);

	dx /= 8*res.textfont->max_bounds.width;
	dy /= (res.textfont->ascent+res.textfont->descent);

	updatepatch(patch.xoff+dx,patch.yoff+dy);
	return;
}
