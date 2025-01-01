/*
 *	IOCTL function codes and parameter definitions
 */
#define UNIXmasterClear 	_IO(i,1)
#define UNIXinterruptWait	_IOWR(i,2,int)
#define UNIXwaitDMA		_IO(i,3)
#define UNIXinterruptRead	_IOR(i,4,int)
#define UNIXsetRegister 	_IO(i,5)
#define UNIXconfig		_IOW(i,12,int)
#define UNIXpowerReset		_IO(i,15)
#define DEBUGreadStat		_IOW(i,7,int)
#define DEBUGwrtStat		_IOW(i,16,int)
#define DEBUGreadTrace		_IOW(i,8,int)
#define DEBUGreadDev		_IOW(i,9,int)
#define DEBUGtoggle		_IO(i,10)
#define DEBUGreadUnit		_IOW(i,11,int)
#define DEBUGreadCSR		_IO(i,13)
#define DEBUGreset		_IO(i,14)
#define UNSTICK			_IO(i,17)
