/* This routine is an example of the raw command stream interface to figdisp. */

/* Created: 7-Aug-1991 */
/* Sam Southard, Jr. */
/* Modification History: */
/* 17-Oct-1991	SNS/CIT	Modified for new version of SET_BM_SIZE command and */
/*			to not use a fixed buffer size. */

#include "commands.h"
#include <stdio.h>

main()
{
	short *combuf;		/* the command buffer */
	int combuflen;		/* The length of the command buffer */
	short *response;	/* the response from the display server */
	int rlen;		/* the length of the response */
	int next=0;		/* the next buffer place to use */

	short *figdisp_getresponse();

	/* find out the maximum buffer size */
	if ((combuflen=figdisp_maxbuflen()) < 0)
	{
		printf("Unable to get buffer size!\n");
		exit(1);
	}
	if ((combuf=(short *)malloc(combuflen*sizeof(short))) == NULL)
	{
		printf("Unable to get command buffer\n");
		exit(1);
	}

	/* normally we would want to check periodically (before adding each */
	/* command to the command buffer) to see if we were about to exceed */
	/* the buffer length and if so, send the buffer and reset the */
	/* counter.  However, that would just add a lot of garbage to the */
	/* example, so it is not done. */

	/* corresponds to figdisp_init() */
	combuf[next++]=RESET;
	combuf[next++]=SHOW_BM_WIN;
	combuf[next++]=1;

	/* corresponds to figdisp_resize(100,100) */
	combuf[next++]=SET_BM_SIZE;
	combuf[next++]=100;
	combuf[next++]=100;
	combuf[next++]=8;

	/* corresponds to figdisp_cls() */
	combuf[next++]=CLR_BM_WIN;

	/* corresponds to figdisp_line(0,0,99,0,255) */
	combuf[next++]=BM_LINE;
	combuf[next++]=0;
	combuf[next++]=0;
	combuf[next++]=99;
	combuf[next++]=0;
	combuf[next++]=255;

	/* corresponds to figdisp_line(99,0,99,99,255) */
	combuf[next++]=BM_LINE;
	combuf[next++]=99;
	combuf[next++]=0;
	combuf[next++]=99;
	combuf[next++]=99;
	combuf[next++]=255;

	/* corresponds to figdisp_line(99,99,0,99,255) */
	combuf[next++]=BM_LINE;
	combuf[next++]=99;
	combuf[next++]=99;
	combuf[next++]=0;
	combuf[next++]=99;
	combuf[next++]=255;

	/* corresponds to figdisp_line(0,99,0,0,255) */
	combuf[next++]=BM_LINE;
	combuf[next++]=0;
	combuf[next++]=99;
	combuf[next++]=0;
	combuf[next++]=0;
	combuf[next++]=255;

	/* corresponds to figdisp_line(0,0,99,99,255) */
	combuf[next++]=BM_LINE;
	combuf[next++]=0;
	combuf[next++]=0;
	combuf[next++]=99;
	combuf[next++]=99;
	combuf[next++]=255;

	/* corresponds to figdisp_line(99,0,0,99,255) */
	combuf[next++]=BM_LINE;
	combuf[next++]=99;
	combuf[next++]=0;
	combuf[next++]=0;
	combuf[next++]=99;
	combuf[next++]=255;

	/* corresponds to figdisp_setcursor(50,50) */
	combuf[next++]=BM_SET_CURS;
	combuf[next++]=50;
	combuf[next++]=50;

	/* this and the figdisp_getresponse() correspond to */
	/* figdisp_readcursor() */
	combuf[next++]=BM_GET_CURS;

	/* open the channel to the display server */
	figdisp_opencomm(combuflen*2);

	/* send the buffer we formatted above.  Note that we could have */
	/* formatted the buffer after the call to figdisp_opencomm() */
	figdisp_sendcommand(combuf,next);

	/* now get the response from BM_GET_CURS */
	if ((response=figdisp_getresponse(&rlen)) == NULL)
		printf("was not able to get cursor value!\n");
	else if (response[0] != BM_GET_CURS || rlen != 4)
		printf("We got the wrong response or the wrong length!\n");
	else {
		if (response[3] & 0xFF00)
			printf("The user pressed mouse button %d",
				response[3] & 0xFF);
		else printf("The user pressed key '%c'",response[3]);
		printf(" at (%d,%d).\n",response[1],response[2]);
	}

	/* you may now format more command buffers, send them, and get */
	/* responses for as long as you like */

	/* close off the communication channel */
	figdisp_closecomm();

	exit(0);
}
