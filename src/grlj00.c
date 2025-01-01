/*  Modified:
 *      6th Feb 2007. Re-organised bitmap declarations to get to compile
 *                    on Intel Mac.
 */
 
#include <stdio.h>

#define BIT0 0200
#define BIT1 0100
#define BIT2 040
#define BIT3 020
#define BIT4 010
#define BIT5 04
#define BIT6 02
#define BIT7 01
#define ALLBITS 0377

static char *bitmap;


static int nint(x)
double x;
{
    return x+0.5;
}

void
grlj01_(line,coordd,colour,xdim,ydim)
int *line,*colour,*xdim,*ydim;
float coordd[4];
{
	extern char *bitmap;
	double length, x, y, dx, dy, adx, ady, xinc, yinc;
	int ilen, ix, iy, addr, pixpos, i, adinc, xstart, xfin, 
		temp, nbytes;
	char c;


	if(*line > 0) {				/* Figure out how line will */
		dx = (double)(coordd[2] - coordd[0]);	/* be drawn */
		dy = (double)(coordd[3] - coordd[1]);
		adx = dx;
		ady = dy;
		if(dx < 0.0) adx = -dx;
		if(dy < 0.0) ady = -dy;		/* fabs() has a bug */
		if(adx > ady) length = adx;
		else length = ady;
		ilen = nint(length);
		if(ilen > 1) {
			xinc = dx/length;	/* Do every pixel on axis */
			yinc = dy/length;	/* closest to line orientation */
		}
		else ilen = 1;			/* One pixel line */
	}
	else ilen = 1;				/* go through loop once only (dot) */

	if(*colour != 0) *colour = 8;		/* test for colour */
	x = coordd[0];				/* Set starting point */
	y = coordd[1];

	if(yinc == 0.0 && ilen > 15) {		/* Horizontal line accelerator */
		xstart = nint(x);		/* (at least one complete byte) */
		xfin = nint((double)coordd[2]);
		if(xstart > xfin) {		/* Ensure left to right */
			temp = xstart;
			xstart = xfin;
			xfin = temp;
		}
		iy = *ydim - nint(y) + 1;
		addr = (*xdim+1)*iy + xstart/8;	/* Starting address */
		nbytes = xfin/8 - xstart/8;
		if(xstart%8 != 0) {		/* First partial byte */
			nbytes -= 1;
			c = ALLBITS >> (xstart%8);
			if(*colour == 0) bitmap[addr++] &= ~c;
			else bitmap[addr++] |= c;
		}
		for(i=0;i<nbytes;i++) {		/* Complete bytes */
			if(*colour == 0) bitmap[addr++] &= ~ALLBITS;
			else bitmap[addr++] |= ALLBITS;
		}
		if(xfin%8 != 0) {		/* Last partial byte */
			c = ALLBITS << 8 - (xfin%8);
			if(*colour == 0) bitmap[addr] &= ~c;
			else bitmap[addr] |= c;
		}
		return;				/* All done already */
	}
		
	adinc = 0;
	if(xinc == 0.0) {			/* Vertical line accelerator */
		ix = nint(x);
		iy = *ydim - nint(y) + 1;	/* pgplot likes 0 at bottom */
		addr = (*xdim+1)*iy + ix/8;
		adinc = (*xdim+1) * nint(yinc);
		addr += adinc;
		pixpos = *colour + ix%8;
	}
	for(i=0;i<ilen;i++) {
		if(adinc != 0) addr -= adinc;
		else {
			ix = nint(x);
			iy = *ydim - nint(y) + 1;
			addr = (*xdim+1)*iy + ix/8;
			pixpos = *colour + ix%8;
			x += xinc;		/* Set true coords for next point */
			y += yinc;
		}
		switch (pixpos) {		/* Switches are nice and fast */
		    case 0:			/* Clear bits */
			bitmap[addr] &= ~BIT0;
			break;
		    case 1:
			bitmap[addr] &= ~BIT1;	/* BITn and ~BITn are constant */
			break;			/* expressions evaluated at */
		    case 2:			/* compile time */
			bitmap[addr] &= ~BIT2;
			break;
		    case 3:
			bitmap[addr] &= ~BIT3;
			break;
		    case 4:
			bitmap[addr] &= ~BIT4;
			break;
		    case 5:
			bitmap[addr] &= ~BIT5;
			break;
		    case 6:
			bitmap[addr] &= ~BIT6;
			break;
		    case 7:
			bitmap[addr] &= ~BIT7;
			break;
		    case 8:			/* Set bits */
			bitmap[addr] |= BIT0;
			break;
		    case 9:
			bitmap[addr] |= BIT1;
			break;
		    case 10:
			bitmap[addr] |= BIT2;
			break;
		    case 11:
			bitmap[addr] |= BIT3;
			break;
		    case 12:
			bitmap[addr] |= BIT4;
			break;
		    case 13:
			bitmap[addr] |= BIT5;
			break;
		    case 14:
			bitmap[addr] |= BIT6;
			break;
		    case 15:
			bitmap[addr] |= BIT7;
		    default:
			;
		}
	}
}
		
void
grlj02_(fd, xdim, ydim)
/*****************************************************************/
/*                                                               */
/*  This routine takes the fd of an open file, and converts      */
/*  the bitmap into a series of HP laserjet plus graphics data   */
/*  transfer and cursor positioning commands, writing them to    */
/*  the file.                                                    */
/*                                                               */
/*  The main purpose of this is to greatly reduce the volume     */
/*  of bitmap data which is transferred to the file, so that we  */
/*  can use the 300dpi resolution and still get a full page of   */
/*  graphics.  Providing the plot is not too dense, this         */
/*  should work, improving resolution at the expense of CPU      */
/*  time.  Blank areas of the plot are skipped entirely, via     */
/*  the insertion of horizontal cursor positioning commands.     */
/*  The complexity stems from the fact that cursor positioning   */
/*  must be done in text mode, and the position increments are   */
/*  different in text and graphics modes.			 */
/*  If xdim and ydim are small enough, the bitmap is sent at     */
/*  150 dpi.  Compression is still done for efficiency.          */
/*                                                               */
/*                                                               */
/*  Inputs:   fd             File descriptor of open bitmap      */
/*                           scratch file                        */
/*                                                               */
/*            xdim, ydim     Size of the bitmap to be dumped     */
/*                                                               */
/*  CJL 12/16/88                                                 */
/*                                                               */
/*****************************************************************/
int *fd, *xdim, *ydim;

{
extern char *bitmap;
char *lines;
short int start[100], stop[100];
int j, k, left, right, offset,chunkmax,coplen;
int strt, nbyt, horiz, margin, nl, rowsize, jump, res;
register int i,chunk,n;
char grphres[10], grphmod[10], txtmod[10], movup[20], movdn[20];
char trans[20], moveh[20], init[3];
static char esc = 0x1b;

    res = 300; jump = 12;		       /* Decide on resolution */
    if(*xdim <= 1176 && *ydim <= 1500) {
        res = 150;
        jump = 24;
    }
    sprintf(init,"%cE",esc);                   /* Initialize esc sequences */
    sprintf(grphres,"%c*t%dR",esc,res);
    sprintf(grphmod,"%c*r1A",esc);
    sprintf(txtmod,"%c*rB",esc);
    sprintf(movup,"%s%c&a-%dV%s",txtmod,esc,jump,grphmod);
    sprintf(movdn,"%s%c&a+%dV%s",txtmod,esc,jump,grphmod);

    write (*fd,init,2);                        /* Reset printer */
    write (*fd,grphres,7);                     /* Switch printer to */
    write (*fd,grphmod,5);                     /* graphics mode */

    rowsize = *xdim + 1;		       /* Actual rows in bitmap */
    for(nl=0;nl<(*ydim/5 + 1);nl++) {
        lines = &(bitmap[nl*5*rowsize]);
	    
        k = 0;
        for(i=0;i<5;i++) {
            for(j=0;j<20;j++) {
                start[k] = i*rowsize;          /* Init. position arrays */
                stop[k++] = (i+1) * rowsize;
            }
        }

        chunkmax = 0;
        for( k = 0; k < 5; k++) {              /* Loop through 5 lines */
                                               /* Each line is rowsize chars */
                                               /* long, we use sections */
                                               /* of lines[] */
            left = k * rowsize;
            right = left + rowsize;
            offset = k * 20;                   /* offset in start/stop array */

                                               /* chunk is the number of */
                                               /* pieces of a line which */
                                               /* actually contain graphics */
            n = 0;chunk = 0;                   /* chunkmax is longest line */
                                               /* in this group of 5 */

            for(i = left; i < right; i++) {    /* Search k'th line */

                if(lines[i] == '\0') n++;      /* Start counting blanks */

                else {                         /* We found graphics */

                    if(n >= 20) {              /* If >20 zeros, set chunks */
                                               /* in start and stop arrays */

                        stop[offset + chunk] = i-n;
                        start[offset + (++chunk)] = (i/5) *5;
                    }
                    n = 0;                     /* Reset blanks counter */

                }

            }
            stop[offset + chunk] = i-n;        /* Fix blanks at end of line */
                                               /* How many chunks have we? */
            if(chunk > chunkmax) chunkmax = chunk;

        }

                                               /* Loop over chunks first */
        for (chunk = 0; chunk <= chunkmax; chunk++) {

            for (k = 0; k < 5; k++) {          /* Then loop over lines */

                offset = k * 20;               /* Work out pointers and */
                strt = start[offset + chunk];  /* chunk sizes */
                nbyt = stop[offset + chunk] - strt;
                margin = strt - (rowsize*k);

                if (nbyt != 0) {               /* If we have graphics, move */
                                               /* cursor to start of */
                                               /* current chunk of data */
                    horiz = (margin * jump * 8) / 5;
                    sprintf (moveh,"%s%c&a%dH%s",
                                   txtmod,esc,horiz,grphmod);
                    write (*fd,moveh,strlen(moveh));
                }

                                               /* Issue transfer data */
                                               /* escape sequence */
                sprintf (trans,"%c*b%dW",esc,nbyt);
                write (*fd,trans,strlen(trans));
                                               /* And write data if there */
                if (nbyt != 0)                 /* is any left on this line */
                    write (*fd,lines+strt,nbyt);
            }
                                               /* Move up 12/720 inches to */
            write (*fd,movup,16);              /* compensate for 5 graphics */
                                               /* linefeeds of 1/300 inch */
        }
        write (*fd,movdn,16);                  /* move down for next set */

        for (i = 0; i < 5*rowsize; i++)        /* Clear buffer in case there */
            lines[i] = '\0';                   /* are less than 5 lines next */
                                               /* time we read from bitmap */

                                               /* Next 5 lines unless EOF */
    }

    write (*fd,txtmod,4);                      /* Switch back to text mode */
 
    return;
}


void
grlj03_(fd, xdim, ydim)
/*****************************************************************/
/*                                                               */
/*  This routine takes the fd of an open file, and converts      */
/*  the bitmap into a series of HP laserjet plus graphics data   */
/*  transfer commands, writing them to the file.                 */
/*                                                               */
/*  This is the dumb 150dpi version ... no compression           */
/*                                                               */
/*                                                               */
/*  Inputs:   fd             File descriptor of open bitmap      */
/*                           scratch file                        */
/*                                                               */
/*            xdim, ydim     Size of the bitmap to be dumped     */
/*                                                               */
/*  CJL 12/16/88                                                 */
/*                                                               */
/*****************************************************************/
int *fd, *xdim, *ydim;

{
extern char *bitmap;
char *line;
char grphres[10], grphmod[10], txtmod[10], init[3];
char trans[20];
int nl, rowsize;
static char esc = 0x1b;

    sprintf(init,"%cE",esc);                   /* Initialize esc sequences */
    sprintf(grphres,"%c*t150R",esc);
    sprintf(grphmod,"%c*r1A",esc);
    sprintf(txtmod,"%c*rB",esc);

    write (*fd,init,2);                        /* Reset printer */
    write (*fd,grphres,7);                     /* Switch printer to */
    write (*fd,grphmod,5);                     /* graphics mode */

    rowsize = *xdim + 1;		       /* Actual rows in bitmap */
    sprintf(trans,"%c*b%dW",esc,rowsize);
    for(nl=0;nl<(*ydim+2);nl++) {	       /* Still haven't figured out */
        line = &(bitmap[nl*rowsize]);	       /* why need +2 here! */
        write (*fd,trans,strlen(trans));
        write (*fd,line,rowsize);
    }

    write (*fd,txtmod,4);                      /* Switch back to text mode */
 
    return;
}


#include <fcntl.h>

void
grcopn_(fnam,fd,ier,lf)
char *fnam;
int *fd, *ier;
int lf;
{
	char filename[100];
	int i;

	*ier = 0;
	for(i=0;i<lf;i++) {
		filename[i] = fnam[i];
	}
	filename[lf] = '\0';
	if((*fd = open(filename,O_CREAT | O_WRONLY | O_TRUNC,0666)) < 0) {
		perror("output file open:");
		*ier = 1;
	}
}

void 
grccls_(fd)
int *fd;
{
	close(*fd);
}

void
grputc_(fd,c)
int *fd;
char *c;
{
	write(*fd,c,1);
}

void
grclrm_(size)
int *size;
{
	int i;

	for(i=0;i<*size;i++) {
		bitmap[i] = '\0';
	}
}

void
grgetm_(size,ierr)
int *size, *ierr;
{
	char *malloc();

	*ierr = 0;
	if((bitmap = malloc(*size)) == NULL) *ierr = 1;
	grclrm_(size,bitmap);
	printf("Bitmap initialized\n");
}


void
grfrem_()
{
	cfree(bitmap);
}
