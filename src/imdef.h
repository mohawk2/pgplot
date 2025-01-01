/* Definitions for the Imagraph driver */
/* Sam Southard, Jr. */
/* Created: January 5, 1989 */
/* 1-Feb-1989	Board-specific defines moved into imboard.h */
/* 8-Mar-1989	Structure defintions for ioctl calls added */
/*		IM_IOCCLRSCR and IM_IOCLSEEK added */
/*		ME_START added */
/* 19-Mar-1989	DMABASEOFF added */
/* 27-Mar-1989	IM_IOCSETORG added */
/* 12-Apr-1989	Explicit alignment added to im_int_two structure */
/* 13-Apr-1989	IM_IOCCURSOFF, IM_IOCCURSON, IM_IOCCURSBLINK and */
/*		IM_IOCCURSMOVE added */
/* 20-Apr-1989	IM_IOCCURSREAD, IM_IOCCURSPTN, and IM_IOCCURSSET added */
/*		NUMCURSORS added */
/* 1-May-1989	IM_IOCSETRWP added */
/* 4-May-1989	IM_IOCWRITE added */

#define TIMEVAL	(60)	/* number of seconds before timeout */
#define FIFOLEN (30)	/* Number of Read FIFO values saved */
#define DEVDBSIZE (2048)	/* size of memory given to each driver */
#define IMMAXPHYS (65536)	/* maximum single transfer */
#define DMABASEOFF (0x100000)	/* the offset from the base IO address of the */
				/* Imagraph board with a csr of 0 to the base */
				/* of it's frame buffer.  Frame buffers are */
				/* placed contiguously. */
#define NUMCURSORS 1	/* number of pre-defined cursors */

/* The following are definitions for the ioctl structures */
/* The are primarily for user convinience and to get the ioctl(2) call */
/* to copy the correct amount of data into the kernel space */
/* the imioctl & imrioctl routines simply access them as a u_short stream */

/* for IM_IOCxMOVE, IM_IOCxLINE, IM_IOCxRCT, IM_IOCxFRCT, x= A or R */
struct im_two {
	u_short x;
	u_short y;
};

/* for IM_IOCCLR, IM_IOCSCLR, and IM_IOCELPS */
struct im_three {
	u_short p1;	/* color for CLR and SCLR, a for ELPS */
	u_short p2;	/* X for CLR and SCLR, b for ELPS */
	u_short p3;	/* Y for CLR and SCLR, DX for ELPS */
};

/* for IM_IOCCPY, IM_IOCSCPY, IM_IOCxGCPY, IM_IOCxARC */
struct im_four {
	u_short p1;	/* SAH for CPY, SCPY, & GCPY, Xc for ARC */
	u_short p2;	/* SAL for CPY, SCPY, & GCPY, Yc for ARC */
	u_short p3;	/* X for CPY, SCPY, & GCPY,  Xe for ARC */
	u_short p4;	/* Y for CPY, SCPY, & GCPY, Ye for ARC */
};

/* for IM_IOCxEARC */
struct im_six {
	u_short a;
	u_short b;
	u_short Xc;	/* dXc for REARC */
	u_short Yc;	/* dYc for REARC */
	u_short Xe;	/* dXe for REARC */
	u_short Ye;	/* dYe for REARC */
};

/* for IM_IOCCURSPTN */
struct im_thirty {
	u_short pattern[30];
};

/* for IM_IOCxPLL, IM_IOCxPLG, and IM_IOCNIOC */
struct im_int_two {
	u_short count;	/* number of points */
	u_short pad;	/* padding so datap is aligned */
	caddr_t datap;	/* pointer to data */
};

/* for IM_IOCxLUT, IM_IOCxPTN, and IM_IOCDxxx */
struct im_int_three {
	u_short p1;	/* start for LUT and PTN, X size for Dxxx */
	u_short p2;	/* size for LUT and PTN, Y size for Dxxx */
	caddr_t datap;	/* pointer to data */
};

/* Ioctl definitions */

#define IM_IOCTIMERVAL	_IOW(i,3,int)	/* Set the timeout value */
#define IM_IOCSIG	_IOW(i,4,struct sigdata)	/* signal on intr. */
#define IM_IOCNOSIG	_IO(i,5)	/* turn off IOCSIG */
#define IM_IOCDOWARM	_IO(i,6)	/* Warm start */
#define IM_IOCDOCOLD	_IO(i,7)	/* Cold start */
#define IM_IOCLATCHWARM _IOW(i,8,int)	/* do cold if warm fails */
#define IM_IOCWCLCK	_IOW(i,9,u_short)	/* write direction value */
#define IM_IOCRCLCK	_IOR(i,10,u_short)	/* read direction value */
#define IM_IOCWAREA	_IOW(i,11,u_short)	/* set area detection mode */
#define IM_IOCRAREA	_IOR(i,12,u_short)	/* read area detection mode */
#define IM_IOCWCOL	_IOW(i,13,u_short)	/* write color mode */
#define IM_IOCRCOL	_IOR(i,14,u_short)	/* read color mode */
#define IM_IOCWOPM	_IOW(i,15,u_short)	/* write operation mode */
#define IM_IOCROPM	_IOR(i,16,u_short)	/* read operation mode */
#define IM_IOCWMOD	_IOW(i,17,u_short)	/* write modify mode */
#define IM_IOCRMOD	_IOR(i,18,u_short)	/* read operation mode */
#define IM_IOCWSL	_IOW(i,19,u_short)	/* write slant mode */
#define IM_IOCRSL	_IOR(i,20,u_short)	/* read slant mode */
#define IM_IOCWSD	_IOW(i,21,u_short)	/* write rotation mode */
#define IM_IOCRSD	_IOR(i,22,u_short)	/* read rotation mode */
#define IM_IOCWSDIR	_IOW(i,23,u_short)	/* write scan direction */
#define IM_IOCRSDIR	_IOR(i,24,u_short)	/* read scan direction */
#define IM_IOCWDDIR	_IOW(i,25,u_short)	/* write dest. direction */
#define IM_IOCRDDIR	_IOR(i,26,u_short)	/* read dest. direction */
#define IM_IOCRFIFO	_IOR(i,27,u_short)	/* read HD FIFO */
#define IM_IOCWFIFO	_IOW(i,28,u_short)	/* write HD FIFO */
#define IM_IOCRHDSTAT	_IOR(i,29,u_short)	/* read HD status register */
#define IM_IOCRHDREG	_IOWR(i,30,struct im_two)	/* read HD register */
#define IM_IOCWHDREG	_IOW(i,31,struct im_two)	/* write HD register */
#define IM_IOCHDRD	_IOR(i,32,u_short)	/* read HD frame buffer */
#define IM_IOCHDWRT	_IOW(i,33,u_short)	/* write HD frame buffer */
#define IM_IOCRHDDRREG	_IOWR(i,34,struct im_two)	/* read HD draw param */
#define IM_IOCWHDDRREG	_IOW(i,35,struct im_two)	/* write HD draw par */
#define IM_IOCMOD	_IOW(i,36,u_short)	/* modify frame buffer */
#define IM_IOCPAINT	_IO(i,37)		/* HD Paint command */
#define IM_IOCDOT	_IO(i,38)		/* HD Dot command */
#define IM_IOCCRCL	_IOW(i,39,u_short)	/* HD Circle command */
#define IM_IOCPTN	_IOW(i,40,u_short)	/* HD Ptn command */
#define IM_IOCAMOVE	_IOW(i,41,struct im_two)	/* HD Absolute Move */
#define IM_IOCALINE	_IOW(i,42,struct im_two)	/* HD Absolute Line */
#define IM_IOCARCT	_IOW(i,43,struct im_two)	/* HD Abs. Rectangle */
#define IM_IOCAFRCT	_IOW(i,44,struct im_two)	/* HD Abs Filled Rect */
#define IM_IOCRMOVE	_IOW(i,45,struct im_two)	/* HD Relative Move */
#define IM_IOCRLINE	_IOW(i,46,struct im_two)	/* HD Relative Line */
#define IM_IOCRRCT	_IOW(i,47,struct im_two)	/* HD Rel. Rectangle */
#define IM_IOCRFRCT	_IOW(i,48,struct im_two)	/* HD Rel Filled Rect */
#define IM_IOCCLR	_IOW(i,49,struct im_three)	/* HD Clear Area */
#define IM_IOCSCLR	_IOW(i,50,struct im_three)	/* HD Sel. Clear Area */
#define IM_IOCCPY	_IOW(i,51,struct im_four)	/* HD Copy command */
#define IM_IOCSCPY	_IOW(i,52,struct im_four)	/* HD Selective Copy */
#define IM_IOCAGCPY	_IOW(i,53,struct im_four)	/* HD Abs Graph Copy */
#define IM_IOCRGCPY	_IOW(i,54,struct im_four)	/* HD Rel Graph Copy */
#define IM_IOCELPS	_IOW(i,55,struct im_three)	/* HD Ellipse command */
#define IM_IOCAARC	_IOW(i,56,struct im_four)	/* HD Absolute Arc */
#define IM_IOCRARC	_IOW(i,57,struct im_four)	/* HD Relative Arc */
#define IM_IOCAEARC	_IOW(i,58,struct im_six)	/* HD Ellipse Abs Arc */
#define IM_IOCREARC	_IOW(i,59,struct im_six)	/* HD Ellipse Rel Arc */
#define IM_IOCWPTN	_IOW(i,60,struct im_int_three)	/* HD Write Ptn RAM */
#define IM_IOCRPTN	_IOW(i,61,struct im_int_three)	/* HD Read Ptn RAM */
#define IM_IOCDRD	_IOW(i,62,struct im_int_three)	/* HD DMA Read */
#define IM_IOCDWT	_IOW(i,63,struct im_int_three)	/* HD DMA Write */
#define IM_IOCDMOD	_IOW(i,64,struct im_int_three)	/* HD DMA Modify */
#define IM_IOCAPLL	_IOW(i,65,struct im_int_two)	/* HD Abs Polyline */
#define IM_IOCRPLL	_IOW(i,66,struct im_int_two)	/* HD Rel Polyline */
#define IM_IOCAPLG	_IOW(i,67,struct im_int_two)	/* HD Abs Polygon */
#define IM_IOCRPLG	_IOW(i,68,struct im_int_two)	/* HD Rel Polygon */
#define IM_IOCWEDGE	_IOW(i,69,u_short)	/* write edge parameter */
#define IM_IOCREDGE	_IOR(i,70,u_short)	/* read edge parameter */
#define IM_IOCSETCOL	_IOW(i,71,u_short)	/* change the drawing color */
#define IM_IOCWLUT	_IOW(i,72,struct im_int_two)	/* write color LUTs */
#define IM_IOCRLUT	_IOW(i,73,struct im_int_two)	/* read color LUTs */
#define IM_IOCNIOC	_IOW(i,74,struct im_int_two)	/* N ioctls at once */
#define IM_IOCCLRSCR	_IOW(i,75,u_short)	/* clear screen to data */
#define IM_IOCLSEEK	_IOW(i,76,struct im_two)	/* position rw ptr */
#define IM_IOCSETSWAP	_IOW(i,77,u_short)	/* to swap or not to swap */
#define IM_IOCNWFIFO	_IOW(i,78,struct im_int_two)	/* N write FIFOs */
#define IM_IOCSETORG	_IOW(i,79,struct im_two)	/* set origin */
#define IM_IOCCURSOFF	_IO(i,80)		/* cursor off */
#define IM_IOCCURSON	_IO(i,81)		/* cursor on */
#define IM_IOCCURSBLINK	_IOW(i,82,u_short)	/* set cursor blink pattern */
#define IM_IOCCURSMOVE	_IOW(i,83,struct im_two)	/* move cursor */
#define IM_IOCCURSCOL	_IOW(i,84,struct im_four)	/* set cursor color */
#define IM_IOCCURSREAD	_IOR(i,85,struct im_two)	/* read cur pos. */
#define IM_IOCCURSPTN	_IOW(i,86,struct im_thirty)	/* user-def. cursor */
#define IM_IOCCURSSET	_IOW(i,87,u_short)	/* select cursor to use */
#define IM_IOCSETRWP	_IOW(i,88,struct im_two)	/* set RWP */
#define IM_IOCWRITE	_IOW(i,89,struct im_int_three)	/* an odd width DWT */

/* The structure necessary for IM_IOCSIG */

struct sigdata {
	int pid;	/* the process to signal */
	int sig;	/* the signal to send */
};

/* flags for structure member flags */
#define IMFLG_CONFIGURED	(0x01)	/* if device is configured */
#define IMFLG_OPEN		(0x02)	/* if the device is open */
#define IMFLG_READ		(0x04)	/* if reading */
#define IMFLG_WRITE		(0x08)	/* if writing */
#define IMFLG_BUSY		(IMFLG_READ|IMFLG_WRITE)	/* if busy */
#define IMFLG_RESET		(0x10)	/* if device has been reset */
#define IMFLG_CCUDONE		(0x20)	/* waiting on the CCU */
#define IMFLG_TIMEOUT		(0x40)	/* if the device timed out */
#define IMFLG_ATTACHED		(0x80)	/* if device attached */

/* Message control codes for viop & cpu drivers.  Commands after IM_ASYNCMSG */
/* are send asyncronously */

#define IMMSG_PROBE	(1)	/* probe */
#define IMMSG_OPEN	(2)	/* open */
#define IMMSG_READ	(3)	/* read */
#define IMMSG_WRITE	(4)	/* write */
#define IMMSG_CLOSE	(5)	/* close */

#define IM_ASYNCMSG	(5)	/* the last sync. message */

#define IMMSG_SIGMSG	(6)	/* a message buffer to save for a signal */

#define IM_LASTMSG	(6)	/* the last message */

/* Definitions for me_udata table entries */

/* For all commands: */
#define ME_CMD		(0)	/* which command */
#define ME_RTN		(1)	/* the return value */

/* for IOCTLs with a single piece of data */
#define ME_DATA		(2)

/* for IOctls that require multiple parameters */
#define ME_P1		ME_DATA		/* P1 */
#define ME_P2		(ME_DATA+1)	/* P2 */
#define ME_P3		(ME_DATA+2)	/* P3 */
#define ME_P4		(ME_DATA+3)	/* P4 */
#define ME_P5		(ME_DATA+4)	/* P5 */
#define ME_P6		(ME_DATA+5)	/* P6 */

/* for IM_IOCRHDREG & IM_IOCWHDREG - P2 is regno, P2 is data */
#define ME_REGNO	ME_P1	/* register number to read or write */

/* for IMMSG_PROBE */
#define ME_BUS 		ME_P1	/* bus number */
#define ME_CSR		ME_P2	/* CSR address */
#define ME_ILEV		ME_P3	/* interrupt level */

/* for IMMSG_READ & IMMSG_WRITE and IOCTLs the require memory mapping */
#define ME_SDR0		ME_P1	/* SDR0 from process table */
#define ME_SDR7		ME_P2	/* SDR7 from process table */
#define ME_ADDR		ME_P3	/* address of buffer */
#define ME_CNT		ME_P4	/* number of things to read/write */

/* for the DMA requests to the Hitachi */
#define ME_AX		ME_P5	/* for the AX parameter */
#define ME_AY		ME_P6	/* for the AY parameter */

/* for the IM_IOCxLUT and IM_IOCxPTN commands */
#define ME_START	ME_P5	/* for the start of the transfer */
