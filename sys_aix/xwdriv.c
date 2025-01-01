/* XWDRIV -- Fortran callable PGPLOT driver for X Windows software */
/* Modification History: */
/* 30-Apr-1991	SNS/CIT	Trivial modifications for sharable libraries */

#define SIGNAL 0                  /* Choose implementation of backing */
#define FORK   1                  /* store. Choose one by setting it
                                     to 1. The SIGNAL method seems to
                                     work well under Berkeley based Unix
                                     while the FORK method works under
                                     both Berkeley and System V Unix.
                                     If no backing store is required,
                                     set both to 0 */

                                  /* Get includes */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/bitmaps/xlogo64>
#include <stdio.h>
#include <sys/time.h>
#include <signal.h>

                                  /* Define some constants */

#define TRUE  1
#define FALSE 0
#define BELL  7
#define COLORMULT 65534
#define MAXCOLOR 145
#define NCOLORS 16
#define MAXPOINTS 1000 /* this is sloppy! */

                                  /* Useful macros */

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

                                  /* Global variables */

static Display *display;
static Window window;
static GC gc;
static Pixmap pixmap;

                                  /* Begin xwdriv */

#ifdef _AIX
void xwdriv (ifunc, rbuf, nbuf, chr, lchr, len)
#else
void xwdriv_ (ifunc, rbuf, nbuf, chr, lchr, len)
#endif
int   *ifunc, *nbuf, *lchr;
int   len;
float rbuf[];
char  *chr;

/*PGPLOT driver for workstations running X Windows.

  Version 1.0 - 1989 Nov 06 - A. L. Fey
                              Initial try.
  Version 1.1 - 1990 Feb 15 - A. L. Fey
                              Add 'line of pixels'
                              and 'area fill'.
  Version 2.0 - 1990 Jun 07 - A. L. Fey
                              Merge functionality with S.C.
                              Allendorf's Fortran XEDRIVER.FOR.
                              Add code to implement a backing store.
  Version 2.1 - 1990 Jun 28 - A. L. Fey
                              Fix-up display visual classification.
  Version 2.2 - 1990 Jul 06 - A. L. Fey
                              Add additional code for alternate
                              implementation of a backing store.
                              This method 'forks' a process to run
                              in the background and thus requires
                              more overhead than the 'signal' method.
  Version 2.3 - 1990 Oct  8 - Jim Trice (trice@asta.pa.uky.edu)
                              Fix opcode 1.
  Version 2.4 - 1991 Mar 19 - Jim Morgan (morgan@astro.umd.edu)
                              Fix cursor problems; signals.

  Supported device: This driver should work with all workstations
  running the X Windows (Version 11) software under Unix.

  Device type code: /XWINDOW.

  Default device name: The PGPLOT device specification is of the
  form host:server.screen, in which host refers to the machine
  name; server, the server number on that machine; and screen, the
  screen number on that server. For example, rira:0.0 instructs the
  server you are running on to connect to server 0 on the host
  called rira, and that the default screen on that server will be
  screen 0. The default device name is the NULL string, which
  implies that the driver will connect to the server listed in
  the Unix environment DISPLAY variable.

  Default view surface dimensions: Depends on monitor.

  Resolution: Depends on monitor.

  Color capability: X describes color capabilities of a display with a
  visual. This driver will work on systems with visual type of either
  PseudoColor or StaticColor. For a PseudoColor visual the color map is
  read/write and the colors will be those defined by PGPLOT. The number
  of colors available to PGPLOT depends on the monitor and the number not
  allocated by other clients. This driver will attempt to allocate as
  many of these non-allocated colors as the X Windows server will
  allow, up to a maximum of 145 colors. This maximum comes from the
  maximum number of colors that PGPLOT will use internally and a desire
  to avoid hogging the resources of the server. On a display with a
  StaticColor visual the color map is read only. For this case, this
  driver will use the closest hardware equivalents of the PGPLOT colors.
  Again, the maximum number of colors available depends on the monitor
  but is limited to 145, as before. Also, on a StaticColor display, the
  'line of pixels' option may produce unsatisfactory results since there
  will only be a limited number of gray scale levels available in the
  read only color map. This driver will also work on monochrome systems.

  Input capability: The cursor is controlled by the mouse. The user
  positions the cursor, and then types any key on the controlling
  keyboard or the mouse. The mouse buttons are defined to return the
  following characters:

    Button   Character
    ------   ---------
      1          A
      2          D
     >2          X

  File format: It is not possible (at present) to send workstation
  plots to a disk file.

  Obtaining hardcopy: Not possible (at present).

  NOTE: On the close workstation call the user is requested to type
  a <RETURN> to close the PGPLOT window. This driver requires that
  the cursor be in the PGPLOT window when the <RETURN> is typed.
  Optionally, the window may be closed by pressing any mouse button
  while the cursor is in the PGPLOT window. */

{
      extern char *xw_dev_name ;

                                  /* X structures */

      static Colormap cmap;
      static Visual *visual;
      static Window parent;
      static GC gcb;
      static Pixmap icon_pixmap;
      static Cursor cursor;
      static XColor colorcell_defs[MAXCOLOR];
      static XColor c_black, c_red, c_green;
      static XSizeHints size_hints;
      static XEvent report;
      static XGCValues values;
      static XSetWindowAttributes setwinattr;
      static XWMHints wmhints;
      static XImage *xi;
      static XPoint points[MAXPOINTS];
      static KeySym keysym;
      static XComposeStatus compose;
      static XVisualInfo VisualInfo;

                                  /* Window variables */

      static int cells;
      static int screen;
      static unsigned int width;
      static unsigned int height;
      static unsigned long value_mask;
      static int depth;
      static int x, y;
      static unsigned int border_width = 4;
      static unsigned int display_width, display_height;
      static unsigned int display_widthMM, display_heightMM;
      extern char *xw_window_name ;
      extern char *xw_icon_name ;
      static int cursor_cross = 34;  /* XC_crosshair */
      static int cursor_bogos = 10;  /* XC_bogosity */

                                  /* PGPLOT color table (RGB) */

      static float ctable[NCOLORS][3] =
         { {0.0,0.0,0.0}, {1.0,1.0,1.0}, {1.0,0.0,0.0}, {0.0,1.0,0.0},
           {0.0,0.0,1.0}, {0.0,1.0,1.0}, {1.0,0.0,1.0}, {1.0,1.0,0.0},
           {1.0,0.5,0.0}, {0.5,1.0,0.0}, {0.0,1.0,0.5}, {0.0,0.5,1.0},
           {0.5,0.0,1.0}, {1.0,0.0,0.5},
           {0.333,0.333,0.333},
           {0.667,0.667,0.667} };

                                  /* Various variables */

      static unsigned long plane_masks[MAXCOLOR];
      static unsigned long pixels[MAXCOLOR];
      static unsigned long black, white, color;
      static char buffer[10];
      static unsigned char image[1][1280];
      static float resol[2];
      static int maxcol;
      static int icount = 0, npoints;
      static int imin, imax, jmin, jmax;
      static int xmin, xmax, ymin, ymax;
      static int i0, i1, j0, j1, ic;
      static float factor;
      static int mono, Static;
      static int i;
#if FORK
      static int running = FALSE, pid;
#endif

#if SIGNAL
                                  /* Timer for Expose event handler */

      static int running = FALSE;
      static struct itimerval ovalue, tvalue = {
         {0, 100000},                     /* 0.1 second interval */
         {0, 100000}                      /* 0.1 second value */
         };
      static grxw03();                    /* Expose event handler */
#endif

/* Turn OFF Expose event handler before we do anything */

#if SIGNAL
         signal (SIGALRM, SIG_IGN);
#endif

#if FORK
         if (running)
            kill (pid, SIGSTOP);
#endif

/* Branch on opcode. */

         switch (*ifunc) {

/*--- IFUNC=1, Return device name ---------------------------------------*/

         case 1 :
              strcpy( chr, xw_dev_name ) ;
              *lchr = strlen( xw_dev_name ) ;
              for ( i = *lchr ; i < len ; i++ )
                  chr[i] = ' ';
              break;

/*--- IFUNC=2, Return physical min and max for plot device, and range
               of color indices -----------------------------------------*/

         case 2 :

              rbuf[0] = 0.0;
              rbuf[1] = (float) (imax - imin);
              rbuf[2] = 0.0;
              rbuf[3] = (float) (jmax - jmin);
              rbuf[4] = 0.0;
              rbuf[5] = (float) maxcol;
              *nbuf = 6;

              break;

/*--- IFUNC=3, Return device resolution ---------------------------------*/

         case 3 :

              rbuf[0] = resol[0];
              rbuf[1] = resol[1];
              rbuf[2] = 1.0;             /* Device coordinates per pixel */
              *nbuf = 3;

              break;

/*--- IFUNC=4, Return misc device info ----------------------------------*/

         case 4 :

              chr[0] = 'I'; /* interactive device */
              chr[1] = 'C'; /* cursor is available */
              chr[2] = 'N'; /* no dashed lines */
              chr[3] = 'A'; /* area fill available */
              chr[4] = 'N'; /* no fat lines */
              chr[5] = 'R'; /* rectangle fill available */
              chr[6] = 'P'; /* line of pixels available */
              chr[7] = 'N'; /* not used */
              chr[8] = 'N'; /* not used */
              chr[9] = 'N'; /* not used */
              *lchr = 10;

              break;

/*--- IFUNC=5, Return default file name ---------------------------------*/

         case 5 :

              strcpy (chr, "");  /* Default name is NULL */
              *lchr = 0;

              break;

/*--- IFUNC=6, Return default physical size of plot ---------------------*/

         case 6 :

              rbuf[0] = 0.0;
              rbuf[1] = (float) (imax - imin);
              rbuf[2] = 0.0;
              rbuf[3] = (float) (jmax - jmin);
              *nbuf = 4;

              break;

/*--- IFUNC=7, Return misc defaults -------------------------------------*/

         case 7 :

              rbuf[0] = 1.0;
              *nbuf = 1;

              break;

/*--- IFUNC=8, Select plot ----------------------------------------------*/

         case 8 :

              break;

/*--- IFUNC=9, Open workstation -----------------------------------------*/

         case 9 :

         /* Connect to X server */

              for (i = *lchr; i < strlen (chr); i++)
                 chr[i+1] = '\0'; /* pad chr with nulls */

              if ((display = XOpenDisplay (chr)) == NULL) {
                 (void) fprintf (stderr,
                    "XWDRIV: cannot connect to X server %s\n",
                    XDisplayName (chr));
                 rbuf[1] = 0.0;
                 break;
                 }

         /* Get screen size from display structure macro */

              screen = DefaultScreen (display);

         /* Size and position window */

              display_width  = DisplayWidth (display, screen);
              display_height = DisplayHeight (display, screen);
              display_widthMM  = DisplayWidthMM (display, screen);
              display_heightMM = DisplayHeightMM (display, screen);

              resol[0] = 25.4 * ((float) display_width /
                                 (float) display_widthMM);
              resol[1] = 25.4 * ((float) display_height /
                                 (float) display_heightMM);

              factor = 8.5 / 11.0;
              width = 3 * display_width / 4;
              height = factor * width;

              x = (display_width - width) / 2;
              y = (display_height - height) / 2;

              imin = (int) (0.25 * resol[0] + 0.5);
              jmin = (int) (0.25 * resol[1] + 0.5);
              imax = width - imin - 1;
              jmax = height - jmin - 1;

         /* Classify the display and create a color map */

              black = BlackPixel (display, screen);
              white = WhitePixel (display, screen);

              depth  = DisplayPlanes (display, screen);
              visual = DefaultVisual (display, screen);
              parent = RootWindow    (display, screen);

              if (depth == 1) {
                 /* Revert to monochrome */
                 mono = TRUE;
                 }
              else {
                 switch (visual->class) {

                 case PseudoColor :
                      /* Get the default color map */
                      cmap = DefaultColormap (display, screen);
                      value_mask = 0;
                      mono = FALSE;
                      Static = FALSE;
                      break;
                 case StaticColor :
                      /* Get the default color map */
                      cmap = DefaultColormap (display, screen);
                      value_mask = 0;
                      mono = FALSE;
                      Static = TRUE;
                      break;
                 default :
                      /* Default visual is not one we can use;
                         try to find one of type PseudoColor */
                      if (!XMatchVisualInfo (display, screen, depth,
                             PseudoColor, &VisualInfo)) {
                         /* Revert to monochrome */
                         mono = TRUE;
                         }
                      else {
                         /* Found visual of type PseudoColor */
                         visual = VisualInfo.visual;
                         /* Create a color map */
                         cmap = XCreateColormap (display, parent, visual,
                            AllocNone);
                         setwinattr.colormap = cmap;
                         value_mask = CWColormap;
                         mono = FALSE;
                         Static = FALSE;
                         }
                      break;
                      }
                 }

         /* Create a window */

              if (!mono)
                 window = XCreateWindow (display, parent,
                    x, y, width, height, border_width, depth,
                    InputOutput, visual, value_mask, &setwinattr);
              else
                 window = XCreateSimpleWindow (display, parent,
                    x, y, width, height, border_width,
                    white, black);

         /* Set window manager hints to assure keyboard input */

              wmhints.flags = InputHint;
              wmhints.input = TRUE;
              XSetWMHints (display, window, &wmhints);

         /* Set the maximum number of colors and allocate color cells
            if the color map is read/write */

              maxcol = 1;  /* Default for monochrome device */

              if (!mono) {

                 /* Determine the maximum number of colors available */
                 cells  = DisplayCells (display, screen);
                 maxcol = min (cells, MAXCOLOR);

                 /* Grab as many color cells as we need (or can) */
                 if (!Static) {
                    while (!XAllocColorCells (display, cmap, True,
                            plane_masks, 0, pixels, (unsigned) maxcol) &&
                            maxcol > 2) {
                       maxcol--;
                       }
                    }

                 maxcol -= 1;

                 /* Revert to monochrome if two or fewer colors were found */
                 if (maxcol <= 1) {
                    mono = TRUE;
                    maxcol = 1;
                    }

                 }

         /* Load the color table */

              if (!mono) {

                 /* Store PGPLOT color definitions */
                 for (i = 0; i <= min (NCOLORS - 1, maxcol); i++) {
                    colorcell_defs[i].pixel = pixels[i];
                    colorcell_defs[i].red   = (int)(ctable[i][0]*COLORMULT+0.5);
                    colorcell_defs[i].green = (int)(ctable[i][1]*COLORMULT+0.5);
                    colorcell_defs[i].blue  = (int)(ctable[i][2]*COLORMULT+0.5);
                    colorcell_defs[i].flags = DoRed | DoGreen | DoBlue;
                    colorcell_defs[i].pad   = 0;

                    if (!Static) {
                       XStoreColor (display, cmap, &colorcell_defs[i]);
                       }
                    else {
                       XAllocColor (display, cmap, &colorcell_defs[i]);
                       pixels[i] = colorcell_defs[i].pixel;
                       }
                    }

                 /* Redefine black and white */
                 black = pixels[0];
                 white = pixels[1];

                 /* Get color structures for cursor colors */
                 c_black.pixel = pixels[0];
                 c_red.pixel   = pixels[2];
                 c_green.pixel = pixels[3];
                 XQueryColor (display, cmap, &c_black);
                 XQueryColor (display, cmap, &c_red);
                 XQueryColor (display, cmap, &c_green);

                 }

         /* Set the window colors */

              XSetWindowBackground (display, window, black);
              XSetWindowBorder (display, window, white);

         /* Create pixmap of depth 1 (bitmap) for icon */

              icon_pixmap = XCreateBitmapFromData (display, window,
                 xlogo64_bits, xlogo64_width, xlogo64_height);

         /* Initialize size hint property for window manager */

              size_hints.flags = PPosition | PSize | PMinSize | PMaxSize;
              size_hints.x = x;
              size_hints.y = y;
              size_hints.width = width;
              size_hints.height = height;
              size_hints.min_width = width;
              size_hints.min_height = height;
              size_hints.max_width = width;
              size_hints.max_height = height;

         /* Set properties for window manager (always before mapping) */

              XSetStandardProperties (display, window, xw_window_name,
                 xw_icon_name, icon_pixmap, NULL, 0, &size_hints);

         /* Create a pixmap for double buffering */

              pixmap = XCreatePixmap (display, window, width, height, depth);

         /* Create default graphics contexts and make a few adjustments */

              gc  = XCreateGC (display, pixmap, 0, &values);
              gcb = XCreateGC (display, pixmap, 0, &values);
              XSetFillRule  (display, gc, WindingRule);
              XSetFillStyle (display, gc, FillSolid);
              XSetFillRule  (display, gcb, WindingRule);
              XSetFillStyle (display, gcb, FillSolid);

         /* Specify foreground colors in graphics contexts */

              XSetForeground (display, gc, white);
              XSetForeground (display, gcb, black);

         /* Clear the pixmap - we do this for servers that do not
            clear the pixmap when it is created */

              XFillRectangle (display, pixmap, gcb, 0, 0, width, height);

         /* Select the event types wanted */

              XSelectInput (display, window, StructureNotifyMask
                 | ExposureMask | KeyPressMask | ButtonPressMask
                 | EnterWindowMask | LeaveWindowMask);

         /* Define a cursor */

              cursor = XCreateFontCursor (display, cursor_bogos);
              XDefineCursor (display, window, cursor);
              if (!mono)
                 XRecolorCursor (display, cursor, &c_green, &c_black);

         /* Display window */

              XMapRaised (display, window);

         /* Wait for mapping notification */

              XNextEvent (display, &report);

#if SIGNAL
         /* Setup timer for Expose event handler */

              setitimer (0, &tvalue, &ovalue);
              running = TRUE;
#endif

#if FORK
         /* Start Expose event handler */

              if (!running) {
                 pid = fork ();
                 if (pid == 0) {
                    grxw04 ();
                    }
                 running = TRUE;
                 }
#endif

         /* Initialize the damaged region */

              grxw02 (width, height, &xmin, &xmax, &ymin, &ymax);

         /* Successful-- return display */

              rbuf[0] = 1.0;  /* display; */
              rbuf[1] = 1.0;
              *nbuf = 2;

              break;

/*--- IFUNC=10, Close workstation ---------------------------------------*/

         case 10 :

#if FORK
         /* Kill Expose event handler before we do anything */

              if (running) {
                 kill (pid, SIGKILL);
                 running = FALSE;
                 }
#endif
#if SIGNAL
              running = FALSE;
#endif

         /* Ask for user response */

              (void) fprintf (stderr,
                 "%cType <RETURN> to continue: ", BELL);

         /* Discard any ButtonPress or KeyPress events encountered
            up till now */

              while (XCheckTypedEvent (display, ButtonPress, &report))
                 ;
              while (XCheckTypedEvent (display, KeyPress, &report))
                 ;

         /* Event loop */

              while (1) {

                 XNextEvent (display, &report);
                 switch (report.type) {

                 case Expose :
                      XCopyArea (display, pixmap, window, gc,
                         report.xexpose.x, report.xexpose.y,
                         report.xexpose.width, report.xexpose.height,
                         report.xexpose.x, report.xexpose.y);
                      break;
                 case ButtonPress :
                      goto endclose;
                 case KeyPress :
                      XLookupString (&report, buffer, 10, &keysym, &compose);
                      if (keysym == XK_Return)
                         goto endclose;
                      break;
                 case EnterNotify :
                      XSetInputFocus (display, window,
                         RevertToPointerRoot, CurrentTime);
                      break;
                 case LeaveNotify :
                      XSetInputFocus (display, PointerRoot,
                         RevertToPointerRoot, CurrentTime);
                      break;
                 default :
                      break;
                      }
                 }

              endclose :

         /* Free resources */

              XUndefineCursor (display, window);
              XFreeCursor (display, cursor);
              XUnmapWindow (display, window);
              XFreeGC (display, gc);
              XFreeGC (display, gcb);
              XDestroyWindow (display, window);
              XFreePixmap (display, icon_pixmap);
              XFreePixmap (display, pixmap);
              XCloseDisplay (display);

              break;

/*--- IFUNC=11, Begin picture -------------------------------------------*/

         case 11 :

#if SIGNAL
         /* Non-standard window */

              /* Translate input */
              i0 = (int) (rbuf[0] + 0.5) + 2 * imin + 1;
              j0 = (int) (rbuf[1] + 0.5) + 2 * jmin + 1;

              /* See if it is different than what we already have */
              if (i0 != width || j0 != height) {
                 width = i0;
                 height = j0;
                 imax = width - imin - 1;
                 jmax = height - jmin - 1;
                 x = (display_width - width) / 2;
                 y = (display_height - height) / 2;

                 /* Destroy old pixmap and create a new one */
                 XFreePixmap (display, pixmap);
                 pixmap = XCreatePixmap (display, window, width,
                    height, depth);

                 /* Reset size hint property for window manager */
                 size_hints.flags = PPosition | PSize | PMinSize | PMaxSize;
                 size_hints.x = x;
                 size_hints.y = y;
                 size_hints.width = width;
                 size_hints.height = height;
                 size_hints.min_width = width;
                 size_hints.min_height = height;
                 size_hints.max_width = width;
                 size_hints.max_height = height;

                 /* Set properties for window manager */
                 XSetStandardProperties (display, window, xw_window_name,
                    xw_icon_name, icon_pixmap, NULL, 0, &size_hints);

                 /* Resize the window */
                 XResizeWindow (display, window, width, height);

                 }
#endif

         /* Clear the pixmap */

              XFillRectangle (display, pixmap, gcb, 0, 0, width, height);

         /* Clear the window */

              XClearWindow (display, window);

         /* Reset the damaged region */

              grxw02 (width, height, &xmin, &xmax, &ymin, &ymax);

              break;

/*--- IFUNC=12, Draw line -----------------------------------------------*/

         case 12 :

         /* Translate input */

              i0 = (int) (rbuf[0] + 0.5) + imin;
              j0 = (jmax - jmin) - (int) (rbuf[1] + 0.5) + jmin;
              i1 = (int) (rbuf[2] + 0.5) + imin;
              j1 = (jmax - jmin) - (int) (rbuf[3] + 0.5) + jmin;

         /* Draw the line */

              XDrawLine (display, pixmap, gc, i0, j0, i1, j1);

         /* Update the damaged region */

              grxw01 (1, i0, j0, i1, j1, &xmin, &xmax, &ymin, &ymax);

              break;

/*--- IFUNC=13, Draw dot ------------------------------------------------*/

         case 13 :

         /* Translate input */

              i0 = (int) (rbuf[0] + 0.5) + imin;
              j0 = (jmax - jmin) - (int) (rbuf[1] + 0.5) + jmin;

         /* Draw the point */

              XDrawPoint (display, pixmap, gc, i0, j0);

         /* Update the damaged region */

              grxw01 (0, i0, j0, i0, j0, &xmin, &xmax, &ymin, &ymax);

              break;

/*--- IFUNC=14, End picture ---------------------------------------------*/

         case 14 :

              break;

/*--- IFUNC=15, Select color index --------------------------------------*/

         case 15 :

         /* Translate input */

              ic = (int) (rbuf[0] + 0.5);

         /* Check input for proper range */

              if (ic < 0 || ic > maxcol) {
                 ic = 1;
                 rbuf[0] = (float) ic;
                 }

         /* Change the color index - handle monochrome properly */

              if (!mono) {
                 color = pixels[ic];
                 XSetForeground (display, gc, color);
                 }
              else if (ic == 1)
                 XSetForeground (display, gc, white);
              else
                 XSetForeground (display, gc, black);

              break;

/*--- IFUNC=16, Flush buffer. -------------------------------------------*/

         case 16 :

         /* Copy pixmap to window and flush display */

              if (xmax != -1)
                 XCopyArea (display, pixmap, window, gc, xmin, ymin,
                       xmax - xmin + 1, ymax - ymin + 1, xmin, ymin);
              XFlush (display);

         /* Reset the damaged region */

              grxw02 (width, height, &xmin, &xmax, &ymin, &ymax);

              break;

/*--- IFUNC=17, Read cursor. --------------------------------------------*/

         case 17 :

         /* Create graphics cursor */

              cursor = XCreateFontCursor (display, cursor_cross);
              XDefineCursor (display, window, cursor);
              if (!mono)
                 XRecolorCursor (display, cursor, &c_red, &c_black);

         /* Translate input */

              i0 = (int) (rbuf[0] + 0.5) + imin;
              j0 = (jmax - jmin) - (int) (rbuf[1] + 0.5) + jmin;

         /* Move cursor */

              XWarpPointer (display, None, window, 0, 0,
                 0, 0, i0, j0);

         /* Discard any ButtonPress or KeyPress events encountered
            up till now */

              while (XCheckTypedEvent (display, ButtonPress, &report))
                 ;
              while (XCheckTypedEvent (display, KeyPress, &report))
                 ;

         /* Event loop */

              while (1) {

                 XNextEvent (display, &report);
                 switch (report.type) {

                 case Expose :
                      XCopyArea (display, pixmap, window, gc,
                         report.xexpose.x, report.xexpose.y,
                         report.xexpose.width, report.xexpose.height,
                         report.xexpose.x, report.xexpose.y);
                      break;
                 case ButtonPress :
                      rbuf[0] = (float) (report.xbutton.x - imin);
                      rbuf[1] = (float) ((jmax - jmin) -
                                         report.xbutton.y + jmin);
                      if (report.xbutton.button == Button1)
                         strcpy (chr, "A");
                      else if (report.xbutton.button == Button2)
                         strcpy (chr, "D");
                      else
                         strcpy (chr, "X");
                      *nbuf = 2;
                      *lchr = 1;
                      goto endcursor;
                 case KeyPress :
                      rbuf[0] = (float) (report.xbutton.x - imin);
                      rbuf[1] = (float) ((jmax - jmin) -
                                         report.xbutton.y + jmin);
                      XLookupString (&report, buffer, 10, &keysym, &compose);
                      if ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R))
                        break; /* do nothing because its a modifier key. */
                      strcpy (chr, buffer);
                      *nbuf = 2;
                      *lchr = 1;
                      goto endcursor;
                 case EnterNotify :
                      XSetInputFocus (display, window,
                         RevertToPointerRoot, CurrentTime);
                      break;
                 case LeaveNotify :
                      XSetInputFocus (display, PointerRoot,
                         RevertToPointerRoot, CurrentTime);
                      break;
                 default :
                      break;
                      }
                 }

              endcursor :

         /* Return cursor to original state */

              cursor = XCreateFontCursor (display, cursor_bogos);
              XDefineCursor (display, window, cursor);
              if (!mono)
                 XRecolorCursor (display, cursor, &c_green, &c_black);

              break;

/*--- IFUNC=18, Erase alpha screen. -------------------------------------*/
         /* (Not implemented: no alpha screen) */

         case 18 :

              break;

/*--- IFUNC=19, Set line style. -----------------------------------------*/
         /* (Not implemented: should not be called) */

         case 19 :

              break;

/*--- IFUNC=20, Polygon fill. -------------------------------------------*/

         case 20 :

         /* Use icount as indication of first time or not */

              if (icount == 0) {
                 /* Translate input */
                 npoints = (int) (rbuf[0] + 0.5);
                 icount = npoints;
                 if (npoints > MAXPOINTS) { /* this is sloppy! */
                    *nbuf = -1;
                    break;
                    }
                 }
              else {

         /* Second or other time; draw to next vertex; decrement icount */

                 /* Translate input */
                 i0 = (int) (rbuf[0] + 0.5) + imin;
                 j0 = (jmax - jmin) - (int) (rbuf[1] + 0.5) + jmin;

                 /* Load vertex into array */
                 points[npoints - icount].x = i0;
                 points[npoints - icount].y = j0;

                 /* Decrement counter */
                 icount -= 1;

         /* Update the damaged region */

                 grxw01 (0, i0, j0, i0, j0, &xmin, &xmax, &ymin, &ymax);

         /* Last call; give the polygon fill command */

                 if (icount == 0)
                    XFillPolygon (display, pixmap, gc, points, npoints,
                       Complex, CoordModeOrigin);
                 }

              break;

/*--- IFUNC=21, Set color representation. -------------------------------*/

         case 21 :

         /* This is ignored for a monochrome device */

              if (!mono) {

                 /* Translate input */
                 ic = (int) (rbuf[0] + 0.5);

                 /* Load the color structure */
                 if (ic >= 0 && ic <= maxcol) {
                    colorcell_defs[ic].pixel = pixels[ic];
                    colorcell_defs[ic].red   = (int)(rbuf[1]*COLORMULT+0.5);
                    colorcell_defs[ic].green = (int)(rbuf[2]*COLORMULT+0.5);
                    colorcell_defs[ic].blue  = (int)(rbuf[3]*COLORMULT+0.5);
                    colorcell_defs[ic].flags = DoRed | DoGreen | DoBlue;
                    colorcell_defs[ic].pad   = 0;

                    if (!Static) {
                       XStoreColor (display, cmap, &colorcell_defs[ic]);
                       }
                    else {
                       XAllocColor (display, cmap, &colorcell_defs[ic]);
                       pixels[ic] = colorcell_defs[ic].pixel;
                       }
                    }
                 }

              break;

/*--- IFUNC=22, Set line width. -----------------------------------------*/
         /* (Not implemented: should not be called) */

         case 22 :

              break;

/*--- IFUNC=23, Escape --------------------------------------------------*/
         /* (Not implemented: ignored) */

         case 23 :

              break;

/*--- IFUNC=24, Rectangle Fill. -----------------------------------------*/

         case 24 :

         /* Translate input */

              i0 = (int) (rbuf[0] + 0.5) + imin;
              j0 = (jmax - jmin) - (int) (rbuf[3] + 0.5) + jmin;
              i1 = (int) (rbuf[2] - rbuf[0] + 1.5);
              j1 = (int) (rbuf[3] - rbuf[1] + 1.5);

         /* Draw the rectangle */

              XFillRectangle (display, pixmap, gc, i0, j0,
                 (unsigned) i1, (unsigned) j1);

         /* Update the damaged region */

              grxw01 (1, i0, j0, i0 + i1 - 1, j0 + j1 - 1,
                      &xmin, &xmax, &ymin, &ymax);

              break;

/*--- IFUNC=25, ---------------------------------------------------------*/
         /* (Not implemented: ignored) */

         case 25 :

              break;

/*--- IFUNC=26, Line of pixels ------------------------------------------*/

         case 26 :

         /* Translate input */

              i0 = (int) (rbuf[0] + 0.5) + imin;
              j0 = (jmax - jmin) - (int) (rbuf[1] + 0.5) + jmin;

         /* Load the image array */

              for (i = 0; i <= *nbuf - 3; i++)
                  image[0][i] = pixels[(int) (rbuf[i + 2] + 0.5)];

         /* Create the image */

              xi = XCreateImage (display, visual, depth, ZPixmap, 0,
                  image, *nbuf - 2, 1, 8, 0);

         /* Draw the image */

              XPutImage (display, pixmap, gc, xi, 0, 0, i0, j0,
                  *nbuf - 2, 1);

         /* Update the damaged region */

              grxw01 (1, i0, j0, i0 + *nbuf - 3, j0,
                      &xmin, &xmax, &ymin, &ymax);

              break;

/*--- IFUNC=?, ----------------------------------------------------------*/

         default :

         /* Notify the user of an input error */

              (void) fprintf (stderr,
                 "Unimplemented function in X Windows device driver: %d\n",
                 *ifunc);
              *nbuf = -1;

              break;

              } /* End of switch */

/* Turn ON Expose event handler, then return to calling program */

#if SIGNAL
         if (running)
             signal (SIGALRM, grxw03);
#endif

#if FORK
         if (running)
            kill (pid, SIGCONT);
#endif

         return;

} /* End of xwdriv */

/*GRXW01 -- PGPLOT XWindow driver, calculate 'damaged' region.
            From S.C. Allendorf's XEDRIVER.FOR */

grxw01 (line, i0, j0, i1, j1, xmin, xmax, ymin, ymax)
int line, i0, j0, i1, j1;
int *xmin, *xmax, *ymin, *ymax;
{
                                        /* Update the damaged region. */
      if (i0 > *xmax)
         *xmax = i0;
      if (i0 < *xmin)
         *xmin = i0;
      if (j0 > *ymax)
         *ymax = j0;
      if (j0 < *ymin)
         *ymin = j0;
                                        /* See if we were passed a
                                           rectangle and update the
                                           damaged region accordingly. */
      if (line == 1) {
         if (i1 > *xmax)
            *xmax = i1;
         if (i1 < *xmin)
            *xmin = i1;
         if (j1 > *ymax)
            *ymax = j1;
         if (j1 < *ymin)
            *ymin = j1;
         }
}

/*GRXW02 -- PGPLOT XWindow driver, reset 'damaged' region.
            From S.C. Allendorf's XEDRIVER.FOR */

grxw02 (width, height, xmin, xmax, ymin, ymax)
unsigned int width, height;
int *xmin, *xmax, *ymin, *ymax;
{
                                        /* Reset the boundaries of the
                                           damaged region. */
      *xmax = -1;
      *ymax = -1;
      *xmin = width + 1;
      *ymin = height + 1;
}

#if SIGNAL
/*GRXW03 -- PGPLOT XWindow driver, Expose event handler (redrawing routine).
            From S.C. Allendorf's XEDRIVER.FOR */

static grxw03 (sig, code, scp)
int sig, code;
struct sigcontext *scp;
{
      XEvent event;
      int event_mask;

                                       /* Select events. */

      event_mask = ExposureMask | EnterWindowMask | LeaveWindowMask;

      while (XCheckWindowEvent (display, window, event_mask, &event)) {

                                       /* If part of the window has been
                                          exposed, redraw that part. We
                                          ignore NoExpose events and
                                          GraphicsExpose events. */

            switch (event.type) {

            case Expose :
                 XCopyArea (display, pixmap, window, gc,
                    event.xexpose.x, event.xexpose.y,
                    event.xexpose.width, event.xexpose.height,
                    event.xexpose.x, event.xexpose.y);
                 break;
            case EnterNotify :
                 XSetInputFocus (display, window,
                    RevertToPointerRoot, CurrentTime);
                 break;
            case LeaveNotify :
                 XSetInputFocus (display, PointerRoot,
                    RevertToPointerRoot, CurrentTime);
                 break;
            default :
                 break;
                 }
            }
}
#endif

#if FORK
/*GRXW04 -- PGPLOT XWindow driver, Expose event handler (redrawing routine).
            From S.C. Allendorf's XEDRIVER.FOR */

grxw04 ()
{
      XEvent event;

      while (1) {

                                       /* If part of the window has been
                                          exposed, redraw that part. We
                                          ignore NoExpose events and
                                          GraphicsExpose events. */


         XNextEvent (display, &event);
         switch (event.type) {

         case Expose :
              XCopyArea (display, pixmap, window, gc,
                 event.xexpose.x, event.xexpose.y,
                 event.xexpose.width, event.xexpose.height,
                 event.xexpose.x, event.xexpose.y);
              break;
         case EnterNotify :
              XSetInputFocus (display, window,
                 RevertToPointerRoot, CurrentTime);
              break;
         case LeaveNotify :
              XSetInputFocus (display, PointerRoot,
                 RevertToPointerRoot, CurrentTime);
              break;
         default :
              break;
              }
         }
}
#endif
