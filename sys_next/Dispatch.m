
/* Template created by Interface Builder */

#import "Dispatch.h"
#import "sername.h"

@implementation Dispatch

- init
{
   [super init];
   [self newWindow:self];

   myListener = [[pgvListener alloc] init];
   [myListener checkInAs: PGV_SERVER_NAME];
   [myListener addPort];
   [myListener setDelegate:self];

//   mySpeaker = [[pgvSpeaker alloc] init];
//   thePort = NXPortFromName(PGV_SERVER_NAME, NULL);
//   [mySpeaker setSendPort:thePort];

   return self;
}

- newWindow:sender
{
   newPGObject = [PGObject new];
   [newPGObject show:self];
   curView=[newPGObject getView];
   return self;
}

- clear
{
   [curView clear];
   return self;
}

- dopsop: (int) iop at: (int) n1 and:(int) n2
{
   [curView dopsop:iop at:n1 and:n2];
   return self;
}

- eofill
{
   [curView eofill];
   return self;
}

- flush
{
   [curView flush];
   return self;
}

- linewidth: (double) dwidth
{
   [curView linewidth:dwidth];
   return self;
}

- setgray: (double) dgray
{
   [curView setgray:dgray];
   return self;
}

@end
