#import "pgvSpeaker.h"
#include <dpsclient/dpsclient.h>
#include <stdlib.h>
#import <stdio.h>

void myspeak( id idum);
id   mySpeak;

void nexsup_( int *ifunc, float *x1, float *x2)
/* Support routine for NEDRIVER.FOR */
{
   double dtmp;
   int    n1,n2;

   switch (*ifunc) {
   case 0:
      mkspeak(&mySpeak);
//      mySpeak=n1;
      break;
   case 1:
      [mySpeak clear];
      break;
   case 2:
      n1= (long) *x1;
      n2= (long) *x2;
      [mySpeak dopsop: dps_moveto at:n1 and:n2];
      break;
   case 3:
      n1= (long) *x1;
      n2= (long) *x2;
      [mySpeak dopsop: dps_lineto at:n1 and:n2];
      break;
   case 4:
      n1= (long) *x1;
      n2= (long) *x2;
      [mySpeak dopsop: dps_rlineto at:n1 and:n2];
      break;
   case 5:
      [mySpeak flush];
      break;
   case 6:
      dtmp= (double) *x1;
      [mySpeak setgray: dtmp];
      break;
   case 7:
      [mySpeak eofill];
      break;
   case 8:
      dtmp= (double) *x1;
      [mySpeak linewidth: dtmp];
      break;
   default :
printf("Oh dear, something has gone a bit wrong in nexsup.\n");
      break;
   }
}
