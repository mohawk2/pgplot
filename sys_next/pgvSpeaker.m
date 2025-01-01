#import <appkit/appkit.h>
#import "pgvSpeaker.h"
#import <mach.h>
#import <sys/message.h>
#import <servers/netname.h>
extern port_t name_server_port;
extern id NXResponsibleDelegate();
@implementation  pgvSpeaker :Speaker
{}
-(int)clear
/* */
{
return [self selectorRPC:"clear"
	paramTypes:""];
}
-(int)dopsop : (int) iop
	at : (int) n1
	and : (int) n2
/* */
{
return [self selectorRPC:"dopsop:at:and:"
	paramTypes:"iii",
		iop,
		n1,
		n2];
}
-(int)eofill
/* */
{
return [self selectorRPC:"eofill"
	paramTypes:""];
}
-(int)flush
/* */
{
return [self selectorRPC:"flush"
	paramTypes:""];
}
-(int)linewidth : (double) dwidth
/* */
{
return [self selectorRPC:"linewidth:"
	paramTypes:"d",
		dwidth];
}
-(int)setgray : (double) dgray
/* */
{
return [self selectorRPC:"setgray:"
	paramTypes:"d",
		dgray];
}
@end
