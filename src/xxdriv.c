/* From:	"graph@ucbast" 26-APR-1990 11:10:01.29 */
/************************************************************************
 *
 *  xxdriver -- xwindow PGPLOT Driver for SUN workstations (XNEWS)
 *
 *  wilson hoffman building on a driver by
 *  Brian M Sutin, May 17, 1989.
 *
 *  this driver uses the X emulation capability of XNEWS
 *   it supports 7 colors or black/white
 *   the window size is static at 535 x 700
 *   it uses the SUN convention to call C programs from FORTRAN
 *   change the include directory pointers before compiling
 *   the window is white(and colors) on black
 *   bug: first cursor return must be a mouse button
 */

/*
 ** dev_name -- the PGPLOT device name **********************************
 */
static char dev_name[] = "XX" ;
/*
 ** scale -- the number of pixels per inch on a typical screen ********
 */
#define SCALE		82
#define TRUE 		1
#define FALSE		0
/*
 ** DEF_*_WID -- default size of the screen *****************************
 */
#define windowX 161
#define windowY 300
#define windowW	700
#define windowH	535

#define INT(x)	( (int) ( (x) + 0.5 ) )

#include "/home/openwin/include/X11/Xlib.h"
#include "/home/openwin/include/X11/Xutil.h"
#include "/home/openwin/include/X11/cursorfont.h"
#include <stdio.h>

#define FontName "-ADOBE-NEW CENTURY SCHOOLBOOK-MEDIUM-R-NORMAL--*-140-*-*-P-*"
#define WindowName "PGPLOT"
#define WindowMess "PGPLOT   (Press right mouse button to continue)"

char *display_name = NULL;
Display *dpy;
Window window;
GC gc;
int screen, ic;
XImage *xi;
unsigned char image[1][1280] ;
Visual *sdefault;
XSetWindowAttributes xswa;
XSizeHints xsh;
XGCValues xgcvl;
int pixel;

XColor exact_color,screen_color;
Colormap cmap;
Font font;

XPoint VERTEX[500] ;
static char *colors[14] = {"white", "red", "green", "blue", "orange", 
"turquoise", "brown", "magenta", "yellow","firebrick", "limegreen","salmon",
"pink", "goldenrod"};
static int colorindex[14];
static int COLOR = 1 ;			/* current pen color */
static int APPEND = 0 ;			/* to erase or not */
#define NCOLOR	14
static int numcolor = 14;
static int NPOLY = 0, POLY = 0 ;
static int xpos, ypos ;
static char button ;

void xxdriv_( FUNC, BUFFER, NBUF, STRING, NSTR, lenstring )
int *FUNC ;				/* function */
float *BUFFER ;				/* floating data */
int *NBUF ;				/* length of BUFFER */
char *STRING ;
int *NSTR ;				/* length of STRING */
int lenstring ;
{
    int i ;
    int width ;
    int newwidth;
    int istyle ;
    int len ;

    *NBUF = 0 ;
    *NSTR = 0 ;

    switch( *FUNC ) {
/*
 *************** return device name *************************************
 */
    case 1:
	strcpy( STRING, dev_name ) ;
	*NSTR = strlen( dev_name ) ;
	for( i = *NSTR ; i < lenstring ; i++ )
	    *(STRING+i) = ' ' ;
	break ;
/*
 *************** return minimum range of view surface and color index ***
 */
    case 2:
	BUFFER[0] = 0 ;			/* minimum X value */
	BUFFER[1] = windowW - 1 ;	/* maximum X value */
	BUFFER[2] = 0 ;			/* minimum Y value */
	BUFFER[3] = windowH - 1 ;	/* maximum Y value */
	BUFFER[4] = 0 ;			/* minimum color value */
	BUFFER[5] = NCOLOR - 1 ;	/* maximum color value */
	*NBUF = 6 ;
	break ;
/*
 *************** return device scale ************************************
 */
    case 3:
	BUFFER[0] = SCALE ;		/* X units per inch */
	BUFFER[1] = SCALE ;		/* Y units per inch */
	BUFFER[2] = 0.2 ;/* really 1 */	/* pen width */
	*NBUF = 3 ;
	break ;
/*
 *************** return device capabilities *****************************
 */
    case 4:
	strcpy( STRING, "ICNATRPNNN" ) ;
	*NSTR = 10 ;
/*	STRING[0] = 'I' ;		 interactive device 
	STRING[1] = 'C' ;		 cursor is available 
	STRING[2] = 'N' ;		 no dashed lines 
	STRING[3] = 'A' ;		 polygon fill available 
	STRING[4] = 'T' ;	  	 fat lines 
	STRING[5] = 'R' ;                rectangle fill
	STRING[6] = 'P' ;		 line o pixels
	STRING[7] = 'N' ;		 not used
	STRING[8] = 'N' ;		 not used
	STRING[9] = 'N' ;		 not used	 */
	break ;
/*
 *************** return default device/file name ************************
 */
    case 5:
	strcpy( STRING, " " ) ;
	*NSTR = 0 ;
	break ;
/*
 *************** return default size of view ****************************
 */
    case 6:
	BUFFER[0] = 0 ;			/* default X min */
	BUFFER[1] = windowW - 1 ;	/* default X max */
	BUFFER[2] = 0 ;			/* default Y min */
	BUFFER[3] = windowH - 1 ;	/* default Y max */
	*NBUF = 4 ;
	break ;
/*
 *************** return miscellaneous defaults **************************
 */
    case 7:
	BUFFER[0] = 2.0 ;		/* return a random number */
	*NBUF = 1 ;
	break ;
/*
 *************** select device ******************************************
 */
    case 8:
	/* do nothing */
	break ;
/*
 *************** open workstation ***************************************
 */
    case 9:
    if ((dpy = XOpenDisplay(display_name)) == NULL) {
	fprintf(stderr, "PGPLOT cannot connext to server %s\n",
		XDisplayName(display_name));	
	BUFFER[2] = 0.0 ;
        return;
    }
    screen = DefaultScreen(dpy);

    xswa.event_mask = ExposureMask ;
    xswa.background_pixel = BlackPixel(dpy,screen);
    xswa.backing_store = Always ;
    xswa.backing_planes = 0xff ;
    sdefault = DefaultVisual(dpy,screen); 	


    window = XCreateWindow(dpy, RootWindow(dpy,screen),
	windowX, windowY, windowW, windowH, 0,
	CopyFromParent, InputOutput,CopyFromParent,
	CWEventMask | CWBackPixel | CWBackingStore | CWBackingPlanes , &xswa);

    xsh.x = windowX;
    xsh.y = windowY;
    xsh.width = windowW;
    xsh.height = windowH;
    xsh.flags = PPosition | PSize;
    XSetNormalHints(dpy, window, &xsh);

    XStoreName(dpy, window, WindowName);
    cmap = DefaultColormap(dpy,screen);

    if ((sdefault->class == PseudoColor) ||  (sdefault->class == StaticColor)) {
	for(i=0; i<14; i++)  {
          XAllocNamedColor(dpy, cmap,colors[i], &screen_color, &exact_color);
          colorindex[i] = screen_color.pixel; 
        }
        numcolor = 14;
    }
    else  {    
	colorindex[0] = WhitePixel(dpy,screen);
	numcolor = 1;
    }  

    /* Create graphics context. */

    xgcvl.foreground = WhitePixel(dpy,screen);
    xgcvl.background = BlackPixel(dpy,screen);

    gc = XCreateGC(dpy, window, GCForeground | GCBackground, &xgcvl);   

    XSetForeground(dpy,gc,WhitePixel(dpy,screen));

/*    Load the font for text writing */
    font = XLoadFont(dpy, FontName);
    XSetFont(dpy, gc, font);

/*  Set initial values for line width, style, cap-style, join-style */

    XSetLineAttributes(dpy,gc,1,LineSolid,CapButt,JoinRound);

/*	Map the window  */
    XMapWindow(dpy, window);

	BUFFER[0] = 0.0 ;		/* return channel 0 */
	BUFFER[1] = 1.0 ;		/* always successful open */
	if( BUFFER[2] )			/* no-erase mode */
	    APPEND = 1 ;
	else
	    APPEND = 0 ;
	*NBUF = 2 ;
	XFlush(dpy) ;
	break ;
/*
 *************** close workstation **************************************
 */
    case 10:
	XFlush(dpy);
	XStoreName(dpy, window,WindowMess) ;

	 button = 'a' ;
	 while (button != '3')  {
	     WaitForCurse(dpy,window,&button,&xpos,&ypos) ;
	     }

	XDestroyWindow( dpy, window);
	break ;
/*
 *************** begin picture ******************************************
 */
    case 11:
	if( !APPEND )	{		/* erase screen */
	  XClearWindow(dpy, window);
	}
	XFlush(dpy) ;
	break ;

/*
 *************** draw line **********************************************
 */
    case 12:
      XDrawLine(dpy,window,gc, INT(BUFFER[0]),windowH-1- INT(BUFFER[1]),
		   INT(BUFFER[2]),windowH-1- INT(BUFFER[3]) ) ;
	break ;
/*
 *************** draw dot ***********************************************
 */
    case 13:
      width = 	xgcvl.line_width ;
      if (width == 1) {
	XDrawPoint(dpy,window,gc, INT(BUFFER[0]),windowH-1- INT(BUFFER[1]));
	}
      else  {
	XFillArc(dpy,window,gc,  INT(BUFFER[0])-width/2,
            windowH-1-INT(BUFFER[1])-width/2,width,width,0,359*64);
	}
      break ;
/*
 *************** end picture ********************************************
 */
    case 14:
	XFlush(dpy) ;
	XSync(dpy,TRUE);
	break ;
/*
 *************** set color index ****************************************
 */
    case 15:
	COLOR = BUFFER[0]-1 ;
	if (COLOR > numcolor-1) COLOR = numcolor-1;
	if (COLOR < 0) XSetForeground(dpy,gc,BlackPixel(dpy,screen)) ;
	else  XSetForeground(dpy,gc,colorindex[COLOR]) ;
	break ;
/*
 *************** flush buffer *******************************************
 */
    case 16:
	XFlush(dpy) ;
	XSync(dpy,TRUE);
	break ;
/*
 *************** read curser ********************************************
 */
    case 17:
	XWarpPointer(dpy,RootWindow(dpy,screen),window,INT(BUFFER[0]),
		windowH-1-INT(BUFFER[1]),windowW,windowH,INT(BUFFER[0]),
			windowH-1-INT(BUFFER[1])) ;
	WaitForCurse(dpy,window,&button,&xpos,&ypos) ;

	BUFFER[0] = xpos ;
	BUFFER[1] = windowH-1- ypos ;
	*STRING = button ;
	*NBUF = 2 ;
	*NSTR = 1 ;
	break ;
/*
 *************** erase alpha screen *************************************
 */
    case 18:
	/* no alpha screen to erase */
	break ;
/*
 *************** set line style *****************************************
 */
    case 19:
	  xgcvl.line_style =  LineSolid ;
	  XChangeGC(dpy,gc,GCLineStyle, &xgcvl) ;
	break ;
/*
 *************** polygon fill *******************************************
 */
    case 20:
	if( POLY == 0 ) {
	    NPOLY = BUFFER[0] ;
	    POLY = NPOLY ;
	    }
	else {
	    VERTEX[POLY].y = windowH-1- BUFFER[1] ;
	    VERTEX[POLY].x = BUFFER[0] ;
	    --POLY;
	    if( POLY == 0 )
	    XFillPolygon(dpy,window,gc,&(VERTEX[1]),NPOLY,Complex,
							CoordModeOrigin)  ;
	    }
	break ;
/*
 *************** set color represention *********************************
 */
    case 21:
	break ;
/*
 *************** set line width *****************************************
 */
    case 22:
	newwidth = BUFFER[0] ;
	if (newwidth<1 || newwidth>40) newwidth = 1 ;
	if (newwidth > 1) newwidth = newwidth/2;
	xgcvl.line_width =  newwidth ;
	XChangeGC(dpy,gc,GCLineWidth, &xgcvl);
	break ;
/*
 *************** escape function ****************************************
 */
    case 23:
	/* no escape functions */
	break ;
/*
 *************** rectangle fill  ****************************************
 */
    case 24:
	XFillRectangle (dpy,window,gc,
	 INT(BUFFER[0]),windowH-1-INT(BUFFER[3]),INT(BUFFER[2]-BUFFER[0]),
	  INT(BUFFER[3]-BUFFER[1])) ;
	break ;
/*
 *************** not implemented ****************************************
 */
    case 25:
	break ;
/*
 *************** Line of Pixels  ****************************************
 */
    case 26:
	for (ic = 0; ic <= *NBUF-3; ic++)
		image[0][ic] = INT(BUFFER[ic+2]) ;
	xi = XCreateImage(dpy,sdefault,DisplayPlanes(dpy,screen),ZPixmap,
		0,image,*NBUF-2,1,8,0);
	XPutImage (dpy,window,gc,xi,0,0,INT(BUFFER[0]),
		windowH-1-INT(BUFFER[1]),*NBUF-2,1) ;
/*
 *************** future unknown functions *******************************
 */
    default:
	*NBUF = -1 ;
	break ;
	}
}
/* 
 ************** WAIT FOR THE CURSOR 
*/
int WaitForCurse(dpy,window,button,x,y)
Display *dpy;
Window window;
char *button ;
int *x ;
int *y ;
{
    int thecursor;
    XEvent event ;
    XComposeStatus compose ;
    KeySym key ;
    char code[4] ;

/*     -- turn on cursor */
      thecursor = XCreateFontCursor(dpy,XC_crosshair);
      XDefineCursor(dpy,window,thecursor);

      XSelectInput(dpy,window,
          ExposureMask|StructureNotifyMask|KeyPressMask|ButtonPressMask) ;
      XGrabPointer(dpy,window,True,KeyPressMask|ButtonPressMask,GrabModeAsync,
	GrabModeAsync,window,None,CurrentTime);
/*      XGrabButton(dpy,AnyButton,AnyModifier,window,True,GrabModeAsync,
	GrabModeAsync,window,None);  */
      XGrabKeyboard(dpy,window,False,GrabModeAsync,GrabModeAsync,CurrentTime);
      XMaskEvent(dpy,KeyPressMask|ButtonPressMask,&event) ;

      if (event.xkey.window == window) {
       switch (event.type)  {
	case ButtonPress:
	    *x = event.xbutton.x ;
	    *y = event.xbutton.y ;
	    *button = (char) (event.xbutton.button + 060) ; 
	    break ;
	case KeyPress:
	    *x = event.xkey.x  ;
	    *y = event.xkey.y  ;
	    while (XLookupString(&event,code,4,&key,&compose) == 0) ; 
	    *button = code[0] ;
	    break ;
       }
      }
/*     --turn off button and key trapping  */
	XSelectInput(dpy,window,ExposureMask|StructureNotifyMask) ;
	XUngrabKeyboard(dpy,CurrentTime);
	XUngrabPointer(dpy,CurrentTime);
/*	XUngrabButton(dpy,AnyButton,AnyModifier,window); */
        XUndefineCursor(dpy,window);
}
