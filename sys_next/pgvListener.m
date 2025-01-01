#import <appkit/appkit.h>
#import "pgvListener.h"
#import <mach.h>
#import <sys/message.h>
#import <servers/netname.h>
extern port_t name_server_port;
extern id NXResponsibleDelegate();
@implementation  pgvListener :Listener
{}
static NXRemoteMethod *remoteMethods = NULL;
#define REMOTEMETHODS 6
+ (void)initialize 
/* */
{
    if (!remoteMethods) {
	remoteMethods =
	(NXRemoteMethod *) malloc((REMOTEMETHODS+1)*sizeof(NXRemoteMethod));
	remoteMethods[0].key = 
	@selector(clear);
	remoteMethods[0].types = "";
	remoteMethods[1].key = 
	@selector(dopsop:at:and:);
	remoteMethods[1].types = "iii";
	remoteMethods[2].key = 
	@selector(eofill);
	remoteMethods[2].types = "";
	remoteMethods[3].key = 
	@selector(flush);
	remoteMethods[3].types = "";
	remoteMethods[4].key = 
	@selector(linewidth:);
	remoteMethods[4].types = "d";
	remoteMethods[5].key = 
	@selector(setgray:);
	remoteMethods[5].types = "d";
	remoteMethods[REMOTEMETHODS].key = NULL;
    }
}
-(int)clear
/* */
{
    id _NXd;
    if (_NXd = NXResponsibleDelegate(self,
	@selector(clear)))
	return [_NXd clear];
    return -1;
}

-(int)dopsop : (int) iop
	at : (int) n1
	and : (int) n2
/* */
{
    id _NXd;
    if (_NXd = NXResponsibleDelegate(self,
	@selector(dopsop:at:and:)))
	return [_NXd dopsop : iop
		at : n1
		and : n2];
    return -1;
}

-(int)eofill
/* */
{
    id _NXd;
    if (_NXd = NXResponsibleDelegate(self,
	@selector(eofill)))
	return [_NXd eofill];
    return -1;
}

-(int)flush
/* */
{
    id _NXd;
    if (_NXd = NXResponsibleDelegate(self,
	@selector(flush)))
	return [_NXd flush];
    return -1;
}

-(int)linewidth : (double) dwidth
/* */
{
    id _NXd;
    if (_NXd = NXResponsibleDelegate(self,
	@selector(linewidth:)))
	return [_NXd linewidth : dwidth];
    return -1;
}

-(int)setgray : (double) dgray
/* */
{
    id _NXd;
    if (_NXd = NXResponsibleDelegate(self,
	@selector(setgray:)))
	return [_NXd setgray : dgray];
    return -1;
}

- (int) performRemoteMethod : (NXRemoteMethod *) method
                  paramList : (NXParamValue *) paramList {
/* */
    switch (method - remoteMethods) {
    case 0:
	return [self clear];
    case 1:
	return [self dopsop : paramList[0].ival
		at : paramList[1].ival
		and : paramList[2].ival];
    case 2:
	return [self eofill];
    case 3:
	return [self flush];
    case 4:
	return [self linewidth : paramList[0].dval];
    case 5:
	return [self setgray : paramList[0].dval];
    default:
	return [super performRemoteMethod : method paramList : paramList];
    }
}
- (NXRemoteMethod *) remoteMethodFor: (SEL) aSel {
/* */
    NXRemoteMethod *rm;
    if (rm = NXRemoteMethodFromSel(aSel,remoteMethods))
        return rm;
    return [super remoteMethodFor : aSel];
}
@end
