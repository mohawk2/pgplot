#import "pgvListener.h"
#import "pgvSpeaker.h"
#import <stdio.h>
// POSIX defines sleep in <unistd.h> which does not exist on the NeXT, sigh.
unsigned int sleep(unsigned int seconds);

void mkspeak( id *retSpeaker)
//
// This routine creates a Speaker object for communicating with the
// PGPLOT viewer.  If there is no Listener then the pgview program
// is launched, and 5 attempts made to connect to the port.  Of course,
// this means the pgview must lie in your current path.
//
{
#include "sername.h"
#include <stdlib.h>

    port_t thePort;
    id     mySpeaker;
    int    icnt;

    mySpeaker = [[pgvSpeaker alloc] init];
    thePort = NXPortFromName(PGV_SERVER_NAME, NULL);
    if (thePort==PORT_NULL) {
        printf("Launching pgview...\n");
        system("pgview &");
        icnt=0;
        while (thePort==PORT_NULL && icnt<5) {
            sleep(3);
            printf("waiting...\n");
            thePort = NXPortFromName(PGV_SERVER_NAME, NULL);
            icnt=icnt+1;
        } /* end while */
        if (thePort==PORT_NULL) {
            printf("Could not find port connected to pgview.\n");
            exit(1);
        } /* end if */
    } /* end if */
    [mySpeaker setSendPort:thePort];

    *retSpeaker=mySpeaker;
}
