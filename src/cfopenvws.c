/* Date: Fri, 15 Dec 89 15:02:47 -0500 */
/* From: dj@astro.lsa.umich.edu */

#include <cgidefs.h>
#include <ctype.h>

Cerror
cfopenvws_(name,screenname, windowname, windowfd, retained, dd,
	   cmapsize,cmapname,flags,ptr,scr_len, wndw_len, cmap_len,
	   ptr_len)
int *name, *windowfd, * retained, *dd, *cmapsize, *flags;
long scr_len, wndw_len, cmap_len, ptr_len;
char *screenname, *windowname, *cmapname, *ptr;
{
 Cvwsurf devdd;
 char *iptr, *malloc(), *trunc();
 Cerror error, open_vws();

 /* initialize viewsurface structure */
 NORMAL_VWSURF(devdd, CGPIXWINDD);

 /* fill in passed parameters */
 strncpy(devdd.screenname,screenname,DEVNAMESIZE-1);
 devdd.screenname[DEVNAMESIZE-1] = '\0';
 strncpy(devdd.windowname,windowname,DEVNAMESIZE-1);
 devdd.windowname[DEVNAMESIZE-1] = '\0';
 devdd.windowfd = *windowfd;
 devdd.retained = *retained;
 devdd.dd = *dd;
 devdd.cmapsize = *cmapsize;
 strncpy(devdd.cmapname,cmapname,DEVNAMESIZE-1);
 devdd.cmapname[DEVNAMESIZE-1] = '\0';
 trunc(devdd.cmapname);
 devdd.flags = *flags;
 iptr = malloc(ptr_len+1);
 strncpy(iptr,ptr,ptr_len);
 iptr[ptr_len] = '\0';
 trunc(iptr);
 devdd.ptr = &iptr;

 /* open up the viewsurface */
 error = open_vws(name,&devdd);

 /* pass the params back to the calling program */
 *windowfd = devdd.windowfd;
 free(iptr);

 return(error);
}


static
char *trunc(string)
char *string;
{
 char *s;

 for (s=string; *s ;s++);
 for (s--; s >= string && isspace(*s) ;*s-- = '\0');
 return(string);
}
