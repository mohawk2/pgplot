/* Generated by Interface Builder */

#import "PGView.h"

@implementation PGView

#import <dpsclient/wraps.h>      // Needed for PSxxx routines
#import <dpsclient/dpsclient.h>

- initFrame:(const NXRect *) frameRect
{
   NXZone *zone;

   self = [super initFrame:frameRect];

   zone=[self zone];
   cops =NXZoneMalloc(zone, 1000*sizeof(char));
   pecops=cops;
   numPoints=0;
   iopt=dps_ustroke;
   width=1.0;
   gray=0.0;

   return self;
}

- drawSelf:(const NXRect *) rects :(int)rectCount
{
   if (iclear != 0) {
      iclear=0;
      PSsetgray(1.0);
      NXRectFill(&bounds);
   }

   if (numPoints>0 ) {

      bbox[0]=bounds.origin.x;   // Xmin
      bbox[1]=bounds.origin.y;   // Ymin
      bbox[2]=bounds.size.width;  // Xmax
      bbox[3]=bounds.size.height; // Ymax

      PSnewpath();
      PSsetlinejoin(1);
      PSsetlinecap(1);
      PSsetlinewidth(width);
      PSsetgray(gray);

      DPSDoUserPath(i2pos, numPoints*2, dps_short,
                     cops, numPoints, bbox, iopt);
      numPoints=0;
      pecops =cops;
   }

   return self;
}

// The following methods are used by the remote Speaker to draw onto
// the sceen.

- clear
{
   iclear=1;
   numPoints=0;
   pecops =cops;
   [self update];
   width=1.0;
   gray=0.0;
   return self;
}

- dopsop: (int) iop at: (int) n1 and:(int) n2
{
   int itmp;

   if ( numPoints>990 ) {
      itmp=numPoints;
      [self update];
      i2pos[numPoints][0] = i2pos[itmp-1][0];
      i2pos[numPoints][1] = i2pos[itmp-1][1];
      numPoints++;
      *pecops++ = dps_moveto;
   }

//   cops[numPoints]    =iop;
   *pecops++    =iop;
   i2pos[numPoints][0] =(short) n1;
   i2pos[numPoints][1] =(short) n2;
   numPoints++;
   return self;
}

- eofill
{
   iopt=dps_ueofill;
   [self update];
   iopt=dps_ustroke;
   return self;
}

- flush
{
   [self update];
   return self;
}

- linewidth: (double) dwidth
{
   if( width != (float) dwidth ) [self update];
   width= (float) dwidth;
   return self;
}

- setgray: (double) dgray
{
   [self update];
   if( gray != (float) dgray ) [self update];
   gray= (float) dgray;
   return self;
}

@end
