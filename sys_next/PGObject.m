
/* Generated by Interface Builder */

#import "PGObject.h"
#import <appkit/Application.h>
#import <appkit/Window.h>

@implementation PGObject

+ new
{
   self = [super new];
   [NXApp loadNibSection:"pgobject.nib" owner:self];
   return self;
}

- show:sender
{
   [myWindow makeKeyAndOrderFront:self];
   return self;
}

- (id) getView
{
   return myView;
}

@end
