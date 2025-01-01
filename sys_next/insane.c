/* Since different versions of Fortran use different conventions when
** constructing subroutine names, this file contain 'name conversion'
** routines.  
*/
/* The Absoft Fortran compiler has an unsual calling sequence, where the
** length of every argument (not just character arrays) is passed.  If
** you are using the Absoft compiler, then use #define ABSOFT, otherwise
** comment out the line.
*/
#define ABSOFT

CGTENV( chr_ptr, cbuf_ptr, chr_len, cbuf_len)
char *chr_ptr, *cbuf_ptr;
int   chr_len,  cbuf_len;
{
   cgtenv_(chr_ptr, cbuf_ptr, chr_len, cbuf_len);
   return;
}

contim(itime, itzone)
int   *itime;
struct tm *itzone;
{
   contim_(itime, itzone);
   return;
}

cgttod(itime)
int   *itime;
{
   cgttod_(itime);
   return;
}

long int GROTER(cdev, ldev, cdev_len)
char *cdev;
long int *ldev;
int cdev_len;
{
   groter_(cdev, ldev, cdev_len);
   return;
}

GRCTER(fd)
int *fd;
{
   grcter_(fd);
   return;
}

GRWTER(fd, cbuf, lbuf, cbuf_len)
int *fd;
char *cbuf;
long int *lbuf;
int cbuf_len;
{
   grwter_(fd, cbuf, lbuf, cbuf_len);
   return;
}

GRPTER(fd, cprom, lprom, cbuf, lbuf, cprom_len, cbuf_len)
int  *fd;
char *cprom, *cbuf;
long int *lprom, *lbuf;
int  cprom_len, cbuf_len;
{
   grpter_(fd, cprom, lprom, cbuf, lbuf, cprom_len, cbuf_len);
   return;
}

void nexsup( int *ifunc, float *x1, float *x2)
{
   nexsup_(ifunc, x1, x2);
   return;
}

#ifdef ABSOFT
void XWDRIV(opcode,rbuf,nbuf,chr,lchr, lop,lr,ln,chrlen,ll)
int lop,lr,ln,ll;
#else
void XWDRIV(opcode,rbuf,nbuf,chr,lchr, chrlen)
#endif
int *opcode;	/* The specific PGPLOT function */
float *rbuf;	/* the floating point values */
int *nbuf;	/* number of floats in rbuf */
char *chr;	/* character data */
int *lchr;	/* number of used characters in chr */
int chrlen;	/* actual fortran length of chr */
{
   xwdriv_(opcode,rbuf,nbuf,chr,lchr,chrlen);
   return;
}

#ifdef ABSOFT
void X2DRIV(opcode,rbuf,nbuf,chr,lchr, lop,lr,ln,chrlen,ll)
int lop,lr,ln,ll;
#else
void X2DRIV(opcode,rbuf,nbuf,chr,lchr, chrlen)
#endif
int *opcode;	/* The specific PGPLOT function */
float *rbuf;	/* the floating point values */
int *nbuf;	/* number of floats in rbuf */
char *chr;	/* character data */
int *lchr;	/* number of used characters in chr */
int chrlen;	/* actual fortran length of chr */
{
   x2driv_(opcode,rbuf,nbuf,chr,lchr,chrlen);
   return;
}
