/*	Routines for outputing an image file for printing	*/
/*	on the LaserWriter II NTX.				*/
/* Modification History: */
/* 23-Aug-1991	SNS/CIT	Modified to work with fastdisp */
/*  5-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals moved to globals.h */
/* 14-Oct-1991	SNS/CIT	Allcells and usecells no longer in bm structure */
/* 17-Oct-1991	SNS/CIT Modified to deal with 8 and 16 bit images */
/* 22-Nov-1991	SNS/CIT	Now handles specifying a file or a printer to which */
/*			we should send output. */
/* 25-Nov-1991	SNS/CIT	Now adds a sequence number to any output file. */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS. */

#include "figdisp.h"
#include "globals.h"
#include "messages.h"

#include <stdio.h>
#ifndef VMS
#include <malloc.h>
#endif

static char lwfile[100];
static FILE *lwfp = NULL;

/* The lwprintwin routine prints the given portion of the image */
void lwprintwin(sc, sr, nc, nr)
int sc,sr;	/* the starting column and row */
int nc,nr;	/* the number of columns and rows */
{
	unsigned char mapping[256];
	unsigned char *datbuf;
	int i,j;
	int k;

	(void)printf("Please wait, composing postscript file\n");

	if (lwfileinit(nr, nc, sr, sc))
	{
		(void)fprintf(stderr,MSG_LWOPENERR);
		return;
	}
	if ((datbuf = (unsigned char *)malloc((unsigned)nc)) == NULL)
	{
		(void)fprintf(stderr,MSG_MALLOC);
		return;
	}
	for (i=0 ; i < bm.colors ; ++i)
	{
		k=(usecells[i].red+usecells[i].green+usecells[i].blue)/3;
		mapping[i] = (k >> 8);
	}

	for (i=sr ; i < sr+nr ; ++i)
	{
		if (i < 0 || i >= bm.imheight)
			for (j=0 ; j < nc ; ) datbuf[j++]=255;
		else for (j=sc ; j < sc+nc ; ++j)
		{
			if (j < 0 || j >= bm.imwidth) datbuf[j-sc]=255;
			else {
				if (bppix == 16)
					k=rimdat.b16[j+i*bm.imwidth];
				else k=rimdat.b8[j+i*bm.imwidth];
				datbuf[j-sc]=mapping[allcells[k].pixel-
					allcells[0].pixel];
			}
		}
		lwwrite(datbuf, nc);
	}
	lwendimage();
	free((char *)datbuf);
	lwfinish();
	if (res.psfile == NULL)
	{
		if (res.printer == NULL) (void)printf("File sent to printer\n");
		else (void)printf("File sent to printer %s\n",res.printer);
	} else (void)printf("File output to %s\n",lwfile);
}

lwfileinit(nr,nc,sr,sc)
int nr,nc,sr,sc;
{
	static int seqnum = 0;
	FILE *fopen();

	lwabort();
	if (res.psfile == NULL)
		(void)sprintf(lwfile,"/tmp/figlw%d-%d",getpid(),seqnum);
	else (void)sprintf(lwfile,"%s-%d",res.psfile,seqnum);
	seqnum++;
	lwfp = fopen(lwfile,"w");
	if(lwfp == NULL) {
		if (res.psfile == NULL) (void)fprintf(stderr,
			"Can't open temporary file %s\n",lwfile);
		else (void)fprintf(stderr,"Can't open %s\n",lwfile);
		perror("Because");
		return(1);
	}
	lwstart(nr,nc,sr,sc);
	return(0);
}

lwwrite(buf,n)
unsigned char *buf;
int n;
{
	static int cc= 0;

	while(n > 0) {
		(void)fprintf(lwfp,"%02.2x",*buf);
		buf++;
		n--;
		cc++;
		if(cc >= 40) {
			(void)fprintf(lwfp,"\n");
			cc = 0;
		}
	}
}

lwendimage()
{
	(void)fprintf(lwfp,"grestore\n");
	(void)fprintf(lwfp,"newpath\n");
	(void)fprintf(lwfp,"x0 y0 moveto\n");
	(void)fprintf(lwfp,"xscale 0 rmoveto\n");
	(void)fprintf(lwfp,"0 yscale rmoveto xscale neg 0 rmoveto\n");
	(void)fprintf(lwfp,"0 yscale neg rmoveto stroke\n");
}

lwfinish()
{
	char lprcmnd[100];

	if(lwfp != NULL) {
		(void)fprintf(lwfp,"\nshowpage\n");
		(void)fclose(lwfp);
		lwfp = NULL;
/*		(void)sprintf(lprcmnd,"lpr -Plw -r -s %s",lwfile); */
		if (res.psfile == NULL)
		{
			if (res.printer == NULL)
				(void)sprintf(lprcmnd,"lpr -r -s %s",lwfile);
			else (void)sprintf(lprcmnd,"lpr -r -P%s -s %s",
				res.printer, lwfile);
			(void)system(lprcmnd);
		}
	}
}

lwabort()
{
	if(lwfp != NULL) {
		(void)fclose(lwfp);
		if (res.psfile == NULL)
#ifdef VMS
			(void)delete(lwfile);
#else
			(void)unlink(lwfile);
#endif
	}
}

#define	STD_BPI	300

lwstart(nr,nc,sr,sc)
int nr,nc,sr,sc;
{
	int bperpix;
	float port_cbperpix,port_rbperpix,port_bperpix;
	float land_cbperpix,land_rbperpix,land_bperpix;
	int port;
	int xscale,yscale;
	int xoff,yoff;

/*	Print as postscript comment.				*/
	(void)fprintf(lwfp,"%%! xvideo lwwrite.c\n");
	(void)fprintf(lwfp,"%%width=%d height=%d\n",nc,nr);

/*	Start the real postscript program.				*/
/*	Define a string of the correct length for one row of data.	*/
	(void)fprintf(lwfp,"/picstr %d string def\n",nc);

/*	Center the image on the page.					*/

/*	Find bits per pixel in portrait and landscape mode		*/
	port_cbperpix = 7.5*300./(float)nc;
	port_rbperpix = 9.5*300./(float)nr;
	land_cbperpix = 10.*300./(float)nc;
	land_rbperpix = 7.0*300./(float)nr;

/*	We are keeping the axes scaled the same, so we choose the	*/
/*	smallest bits per pixel.					*/
	if(port_cbperpix < port_rbperpix) {
		port_bperpix = port_cbperpix;
	}
	else {
		port_bperpix = port_rbperpix;
	}
	if(land_cbperpix < land_rbperpix) {
		land_bperpix = land_cbperpix;
	}
	else {
		land_bperpix = land_rbperpix;
	}

/*	Now we choose the rotation to give the largest magnification */
	if(port_bperpix >= land_bperpix) {
		port = 1;
		bperpix = (int)port_bperpix;
	}
	else {
		port = 0;
		bperpix = (int)land_bperpix;
	}

/*	If bperpix is 0 then the image actually has to be shrunk to fit	*/
/*	on the page, even with one bit per pixel - thats a big image!	*/

	if(bperpix == 0) {
		bperpix = 1;
		xscale = yscale = (int)(72.*7.5);
		port = 1;
	}
	else {
		xscale = (72*bperpix*nc)/STD_BPI;
		yscale = (72*bperpix*nr)/STD_BPI;
	}

	if(port) {
		xoff = 36+(int)((72.*7.5-(float)xscale)/2.0);
		yoff = 72+(int)((72.*9.5-(float)yscale)/2.0);
	}
	else {
		xoff = 36+(int)((72.*10.-(float)xscale)/2.0);
		yoff = 72+(int)((72.*7.0-(float)yscale)/2.0);
	}


/*	Rotate and reposition if in landscape mode			*/
	if(port == 0)
		(void)fprintf(lwfp,"612 0 translate 90 rotate\n");

/*	Define a few values.						*/
	(void)fprintf(lwfp,"/r0 %d def /c0 %d def\n",sr,sc);
	(void)fprintf(lwfp,"/x0 %d def /y0 %d def\n",xoff,yoff);
	(void)fprintf(lwfp,"/xmag %f def /ymag %f def\n",
		(float)xscale/(float)nc,
		(float)yscale/(float)nr);
	(void)fprintf(lwfp,"/xscale %d def /yscale %d def\n",xscale,yscale);
	(void)fprintf(lwfp,"/xlabel { /col exch def\n");
	(void)fprintf(lwfp,"col c0 sub xmag mul x0 add y0 15 sub 0 0 10\n");
	(void)fprintf(lwfp," col cvs putlabel } def\n");
	(void)fprintf(lwfp,"/ylabel { /row exch def\n");
	(void)fprintf(lwfp,"row r0 sub ymag mul y0 add x0 5 sub -1 0 10\n");
	(void)fprintf(lwfp," row cvs putlabel } def\n");
		

/*	Center image							*/
	(void)fprintf(lwfp,"gsave\n");
	(void)fprintf(lwfp,"%d %d translate\n",xoff,yoff);

/*	Set scaling.							*/
	(void)fprintf(lwfp,"%d %d scale\n",xscale,yscale);

/*	Width, height, and depth parameters for the image command.	*/
	(void)fprintf(lwfp,"%d %d 8\n",nc,nr);

/*	Image matrix for top-to-bottom scan order.			*/
	(void)fprintf(lwfp,"[%d 0 0 -%d 0 %d]\n",nc,nr,nr);

/*	Read the data, convert to string data and do the image command.	*/
	(void)fprintf(lwfp,"{currentfile picstr readhexstring pop} image\n");

}

