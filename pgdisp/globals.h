/* This file contains all the global variable definitions. */

/* Sam Southard, Jr. */
/* Created: 8-Oct-1991 (from figdisp) */
/* Modification History: */
/*  9-Oct-1991	SNS/CIT	textfont added. */
/* 10-Oct-1991	SNS/CIT	allcells and usecells added. */
/* 14-Oct-1991	SNS/CIT	textfont moved into resource structure */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 24-Jun-1992	SNS/CIT	histpix, usehist, and goodhist added. */

/* Wish list: */
/*	Get rid of global variables. */

#ifdef DEFINE_GLOBALS
Display *display;	/* the main display */
int screen;		/* the main screen */
struct wininfo lg;	/* The line graphics window */
struct wininfo bm;	/* The bitmap graphics window */
struct wininfo patch;	/* The patch window */
struct wininfo loc;	/* The location window */
struct wininfo seeing;	/* The seeing window */
struct wininfo cwin;	/* The color map window */
Atom selatom;		/* The atom for the selection */
Atom dataatom;		/* The atom for data */
Atom incrtype;		/* The incremental type atom */
Atom lock;		/* Used to ensure only one application at a time */
Colormap bitcmap;	/* Was bm.cmap */
Colormap linecmap;	/* Was lg.cmap */
Visual *bitvisual;	/* Was bm.visual */
Visual *linevisual;	/* Was lg.visual */
unsigned int bitdepth;	/* Was bm.depth */
unsigned int linedepth;	/* Was lg.depth */
XColor allcells[BM_COLORS];	/* The colors we'd use if we were the only */
			/* application around */
XColor *usecells;	/* The colors we can play with */
XColor locline;		/* Color to use when drawing lines in the location */
			/* window.  Was loc.allcells[0]. */
GC bitgc;		/* The gc to use with bitmap graphics.  Was bm.gc */
GC bitgcclear;		/* gc to to clear bitmap graphics.  Was bm.gcclear */
GC linegc;		/* Was lg.gc */
GC linegcclear;		/* Was lg.gclear */
GC xorgc;		/* The gc to use to draw XOR lines */
GC textgc;		/* The gc to use to print text */
GC textgcclear;		/* The gc to use to clear text */
struct resource res;	/* The resources to use */
Window srcwin;		/* The source of the data */
int selset=0;		/* If the selection window has been set */
union rawdata rimdat;	/* The image data */
int bppix;		/* The numebr of bits per pixel */
int *histpix;		/* The index into allcells to use for histogram */
			/* equalization.  There are bm.colors of these */
int usehist;		/* True if we should use histogram equalization */
int goodhist;		/* True if histpix is valid. */
#else
extern Display *display;
extern int screen;
extern struct wininfo lg;
extern struct wininfo bm;
extern struct wininfo patch;
extern struct wininfo loc;
extern struct wininfo seeing;
extern struct wininfo cwin;
extern Atom selatom;
extern Atom dataatom;
extern Atom incrtype;
extern Atom lock;
extern Colormap bitcmap;
extern Colormap linecmap;
extern Visual *bitvisual;
extern Visual *linevisual;
extern unsigned int bitdepth;
extern unsigned int linedepth;
extern XColor allcells[BM_COLORS];
extern XColor *usecells;
extern XColor locline;
extern GC bitgc;
extern GC bitgcclear;
extern GC linegc;
extern GC linegcclear;
extern GC xorgc;
extern GC textgc;
extern GC textgcclear;
extern struct resource res;
extern Window srcwin;
extern int selset;
extern union rawdata rimdat;
extern int bppix;
extern int *histpix;
extern int usehist;
extern int goodhist;
#endif
