/* The commands.h file can be found in local/figdisp or */
/* src/local/figdisp/commands.h,v (RCS) in the Figaro distribution. */
#include "commands.h"

main()
{
	int x,y;			/* the returned cursor position */
	int val;			/* the returned cursor value */

	figdisp_open();

	/* the next group of commands is really optional - you can place */
	/* whatever you like here.  You may want to always call */
	/* figdisp_init(), but it is not necessary. */
	figdisp_init();
	figdisp_resize(100,100);
	figdisp_cls();
	figdisp_line(0,0,99,0,255);
	figdisp_line(99,0,99,99,255);
	figdisp_line(99,99,0,99,255);
	figdisp_line(0,99,0,0,255);
	figdisp_line(0,0,99,99,255);
	figdisp_line(99,0,0,99,255);
	figdisp_setcursor(50,50);
	figdisp_readcursor(&x,&y,&val);

	if (val & 0xFF00) printf("The user pressed mouse button %d",val & 0xFF);
	else printf("The user pressed key '%c'",val);
	printf(" at (%d,%d).\n",x,y);

	/* you should always call this routine to make sure that everything */
	/* you sent was received. */
	figdisp_close();

	exit(0);
}
