/* This file contains all the routines necessary to implement Jeff Hester's */
/* seeing algorithm, modified as necessary for incorporation in the display */
/* server.  Note that at the current point only the minimum possible work has */
/* been done to get this algorithm working.  A lot of cleanup could be done. */

/* Sam Southard, Jr. */
/* Created: 4-Sep-1991 (from Jeff Hester's code) */
/*  6-Sep-1991	SNS/CIT	Modified to lint cleanly */
/*  8-Oct-1991	SNS/CIT	Globals now in globals.h */
/* 17-Oct-1991	SNS/CIT	Modified to deal with 8 and 16 bit images */
/* 14-Apr-1992	SNS/CIT	Now compiles under VMS */

#include "figdisp.h"
#include "globals.h"

#ifdef lint
double infinity()
{
	return(2.0);
}
#endif

#define MAXPTS	30	/* Maximum nummr of points to fit	*/

int _ngauss;		/* Number of gaussians to be fit */
double pixscale;	/* Used by igauss()		*/

starfit(xcen, ycen, fwhm0, nx, ny, avgx, avgy, baseline, xc, yc, xfw, yfw)
int xcen, ycen;		/* Approximate center of the star	*/
double fwhm0;		/* Initial guess at FWHM		*/
int nx, ny;		/* Number of points to fit along x, y	*/
   			/* Actually uses 2*nx+1 and 2*ny+1 points */
int avgx, avgy;		/* Number of columns, rows to average	*/
int baseline;		/* 0 for const sky, 1 for linear, 2 for quadratic */
double *xc, *yc, *xfw, *yfw;	/* return values */
{
int i, ii, j;
double x[MAXPTS], y[MAXPTS], wt[MAXPTS], xa[6], ya[6];

_ngauss = 1;
pixscale = 1.;

/* Do x fit */

for(i= -nx, ii=0; i<= nx; i++,ii++)  {
   y[ii] = 0.;
   for(j= -avgy; j<= avgy; j++)
   {
	if (bppix ==16) y[ii] += (double)rimdat.b16[(ycen+j)*bm.imwidth+xcen+i];
	else y[ii] += (double)rimdat.b8[(ycen+j)*bm.imwidth+xcen+i];
   }
   y[ii] = y[ii]/(2*avgy+1);
   x[ii] = xcen+i;
   wt[ii] = 1.;
   }

gstarfit(x, y, wt, 2*nx+1, xa, baseline, fwhm0);

/* Do y fit */

for(i= -ny, ii=0; i<= ny; i++, ii++)  {
   y[ii] = 0.;
   for(j= -avgx; j<= avgx; j++)
   {
	if (bppix ==16) y[ii] += (double)rimdat.b16[(ycen+i)*bm.imwidth+xcen+j];
	else y[ii] += (double)rimdat.b8[(ycen+i)*bm.imwidth+xcen+j];
   }
   y[ii] = y[ii]/(2*avgx+1);
   x[ii] = ycen+i;
   wt[ii] = 1.;
   }

gstarfit(x, y, wt, 2*ny+1, ya, baseline, fwhm0);

*xc=xa[4];
*yc=ya[4];
*xfw=xa[5];
*yfw=ya[5];

return;
}


gstarfit(x, y, wt, npts, a, baseline, fwhm0)
double *x, *y, *wt;
int npts;
double *a;
int baseline;
double fwhm0;
{
static int i;
int digauss();
static int aused[6];
static double deltaa[6], sigmaa[6], yfit[MAXPTS], scale, lambda;
static double cvrg, dchisq,  chisq, chisq1;
double fabs(), curfit(), igauss();
static double leftx, lefty, rightx, righty, s; 
double sqrt();

for (i=0; i<3*_ngauss+3; i++) aused[i]=1;

/* Scale flux  data so it is of order 1 for fitting */

scale=0.;
for(i=0; i<npts; i++) scale += y[i];
scale = scale==0. ? 1. : scale/npts;
for(i=0; i<npts; i++) y[i] /= scale;

/* Make a first guess at the background */

leftx = x[1];
rightx = x[npts-2];
lefty = (y[0]+y[1]+y[2])/3.;
righty = (y[npts-1]+y[npts-2]+y[npts-3])/3.;
a[1] = (righty-lefty)/(rightx-leftx);
a[0] = lefty - a[1]*leftx;
a[2]=0.;

switch(baseline) {
      case 0 : aused[1]=aused[2]=0;
               aused[3]=1;
               a[1]=0.;
               a[2]=0.;
               a[0]=(lefty+righty)/2.;
               break;
      case 1 : aused[2]=0;
               aused[0]=aused[1]=1;
               break;
      case 2 : aused[0]=aused[1]=aused[2]=1;
               break;
      }

/* Initial guess at parameters */

for(i=0; i<_ngauss; i++) {
   a[3*i+3] = y[npts/2] - (a[0] + a[1]*x[npts/2]);
   a[3*i+4] = x[npts/2];
   a[3*i+5] = fwhm0;
   }

s = 1.;

/* Loop over fit attempts */

cvrg = .001;
dchisq=2.*cvrg;
chisq1 = 0.;  lambda = .001;

/* Iterate curve fitting until convergence criterion met */

for(i=1; dchisq>=cvrg; i++) {
   chisq=curfit(x,y,wt,npts,3+_ngauss*3,1,a,aused,deltaa,sigmaa,
         			&lambda,yfit,igauss,digauss)/s;
   dchisq=fabs((chisq-chisq1)/chisq);
   chisq1=chisq;
   }
}

/*
CURFIT -- C translation of subroutine curfit from Bevington.
Curfit does non-linear least squares fitting with a combination of
a gradient search of parameter space and a linearization of the
fitting function.
The code has been modified somewhat from the FORTRAN.  In particular
note that curfit is passed pointers to the routines which evaluate
the function and its derivatives.  Also note that (*functn)() and
(*fderiv)() are passed (x[i], a) instead of (x, i, a).
The argument 'aused' is a pointer to an int array of dimension
nterms.  Aused[i] contains a flag specifying whether or not the 
parameter a[i] is to be varied in the fit or treated as a constant.

JJH, April, 1984

*lambdap should be set = .001 at the beginning of a new fit

Modify fchisq() to normalize output according to weights

Version 28 Feb 1989
27 April 1989 -- Fix problem with size of arrays (4 -> 8 for doubles)
*/

#include <math.h>

double curfit(x,y,weight,npts,nterms,mode,a,aused,deltaa,sigmaa,lambdap,yfit,
functn,fderiv)
double *x, *y, *weight, *a, *deltaa, *sigmaa, *lambdap, *yfit, (*functn)();
int npts, nterms, mode, (*fderiv)(), *aused;
{
double fabs(), fchisq();
static double **alpha, *beta, *deriv, **array, *b;
char *tstalloc();
double sqrt();
double chisq1, chisqr, tiny;
int nfree, i, j, k, l, ntused;
static int lastnterms=0, *m;
double _matinv();
char *makearray();
void freearray();

#ifndef HUGE
#include <float.h>
#define HUGE DBL_MAX
#endif
tiny=1./sqrt(HUGE);

if(lastnterms!=nterms) {
	if(lastnterms) {
		free((char *)beta);  free((char *)deriv);  free((char *)b);
		free((char *)m);
   		freearray((char **)alpha);
   		freearray((char **)array);
		}
#ifdef lint
	(void)tstalloc((unsigned)sizeof(int)*nterms);
	(void)tstalloc((unsigned)8*nterms);
	(void)tstalloc((unsigned)8*nterms);
	(void)tstalloc((unsigned)8*nterms);
#else
	m=(int *)tstalloc((unsigned)sizeof(int)*nterms);
	beta=(double *)tstalloc((unsigned)8*nterms);
	deriv=(double *)tstalloc((unsigned)8*nterms);
	b=(double *)tstalloc((unsigned)8*nterms);
#endif
   	(void)makearray(nterms,nterms,8,(char ***)&alpha);
 	(void)makearray(nterms,nterms,8,(char ***)&array);
 	lastnterms=nterms;
	}

ntused=0;
for(i=0; i<nterms; i++) {
	if(aused[i]) m[ntused++]=i;
	}

nfree = npts - ntused;
if (nfree < 1) return(-1.);

for(i=0; i<npts; i++) {
	switch (mode) {
		case -1 : weight[i]= y[i]==0. ? 1. : 1./fabs(y[i]);
			  break;
		case  0 : weight[i] = 1.;
			  break;
		case  1 : break;
		default : weight[i] = 1.;
	}	}

for(j=0; j<ntused; j++) {
	beta[j]=0.;
	for(k=0; k<=j; k++) alpha[k][j]=0.;
	}

for(i=0; i<npts; i++) {
	(*fderiv)(x[i], a, deltaa, nterms, deriv);
	for(j=0; j<ntused; j++) {
		beta[j] += weight[i]*(y[i]-(*functn)(x[i],a))*deriv[m[j]];
		for(k=0; k<=j; k++) {
			alpha[k][j] += weight[i]*deriv[m[j]]*deriv[m[k]];
	}	}	}

for(j=0; j<ntused; j++) {
	for(k=0; k<=j; k++) alpha[j][k] = alpha[k][j];
	}

for(i=0; i<npts; i++) yfit[i]=(*functn)(x[i],a);
chisq1 = fchisq(y,weight,npts,nfree,yfit);

while(1) {
	for(l=0; l<ntused; l++) if(alpha[l][l]<tiny) return(chisq1); 
	for(j=0; j<ntused; j++)  {
		for(k=0; k<ntused; k++) {
			array[k][j] = alpha[k][j]/sqrt(alpha[j][j]*alpha[k][k]);

			}
		array[j][j] = 1.+(*lambdap);
		}

	(void)_matinv(array,ntused);

	for(j=0; j<nterms; j++) b[j]=a[j];
	for(j=0; j<ntused; j++) {
		for(k=0; k<ntused; k++) {
			b[m[j]] += beta[k]*array[k][j]/sqrt(alpha[j][j]*alpha[k]
[k]);
		}	}

	for(i=0; i<npts; i++) yfit[i] = (*functn)(x[i],b);
	chisqr = fchisq(y,weight,npts,nfree,yfit);

	if(chisq1 >= chisqr) break;

	(*lambdap) = 10.*(*lambdap);
	}

for(j=0; j<nterms; j++) a[j] = b[j];
for(j=0; j<ntused; j++) sigmaa[m[j]] = sqrt(array[j][j]/alpha[j][j]);
(*lambdap) /= 10.;
return(chisqr);
}



/*
FCHISQ() -- Evaluate reduced chi**2 normalize according to weights
*/

double fchisq(y,weight,npts,nfree,yfit)
double *y, *weight, *yfit;
int npts, nfree;
{
int i;
double chisq=0., wtsum = 0.;

for(i=0; i<npts; i++) {
   chisq += weight[i]*(y[i]-yfit[i])*(y[i]-yfit[i]);
   wtsum += weight[i];
   }
wtsum = wtsum/npts;

return(chisq/(wtsum*nfree));
}


/****************************************************************/
/* IGAUSS -- Subroutines to compute a function and the 		*/
/* derivatives of that function.  The function is the integral	*/
/* of a gaussian over a pixel width.  This is used by specred	*/
/* when doing gaussian fits of spectral lines.  Presumably the	*/
/* true distribution of the data is gaussian in wavelength, and	*/
/* each pixel represents the integral of that parent gaussian	*/
/* over a pixel width.  These routines approximate that 	*/
/* by integrating the gaussian and its derivatives using a 3	*/
/* point gaussian quadrature.	The routines are in the proper	*/
/* form to be called by curfit().				*/
/*    The actual function being integrated is a quadratic base-	*/
/* line with _ngauss gaussians on top of it.  The form is:	*/
/* Y = a0 + a1*X + a2*X*X + a3*exp(-W * [(X-a4)**2]/[a5**2])	*/
/*			  + a6*exp(-W * [(X-a7)**2]/[a8**2])	*/
/*			  + .........				*/
/* JJH, April 1984						*/
/****************************************************************/

#define W 2.77258872 	/* 4*ln(2) -- Makes the a5 a FWHM 	*/

/* double pixscale;*//* Pixel width in units of 'x' -- 	*/
/* int _ngauss;*/	/* The number of gaussians to be  	*/
			/* fit.  Pixscale and _ngauss must	*/
			/* be set by the calling routine  	*/

double iwt[3] = {.27777777778, .44444444444, .27777777778};
			/* Weights for 3 point gaussian quadrature */

double igauss(xc, a)
double xc, *a;
{
double out, pow, exp(), x, quadstep, sqrt();
double sum=0.;
int n, i;

quadstep = sqrt(15.)/10. * pixscale;

for(i=0; i<3; i++) {		/* Loop over steps in quadrature*/
   x = xc + (i-1)*quadstep;
   out = a[0] + a[1]*x + a[2]*x*x;
   for(n=0; n<_ngauss; n++) {		/* Loop over gaussians	*/
      pow = (x-a[4+3*n])/a[5+3*n];
      out += a[3+3*n] * exp( -1.*W * pow*pow );
      }
   sum += out*iwt[i];
   }
return(sum);
}


/*
DIGAUSS() -- Calculate derivatives of igauss by integrating the
derivatives of the 'parent' function.
*/

digauss(xc, a, da, nt, drv)
double xc, *a, *da, *drv;
int nt;
{
double pow, a3, a4, a5, exp(), quadstep, sqrt();
double deriv[63];		/* Dimensioned for up to 20 gaussians */
double x;
int n, i;

quadstep = sqrt(15.)/10. * pixscale;

for(i=0; i<3*(1+_ngauss); i++) drv[i]=0.;
for(i=0; i<3; i++) {
   x = xc + (i-1)*quadstep;
   deriv[0] = 1.;
   deriv[1] = x;
   deriv[2] = x*x;

   for(n=0; n<_ngauss; n++) {
      a3=a[3+3*n];  a4=a[4+3*n]; a5=a[5+3*n];
      pow = exp( -1.*W * (x-a4)*(x-a4)/(a5*a5));
      deriv[3+3*n] = pow;
      deriv[4+3*n] = 2.*W*a3*pow*(x-a4)/(a5*a5);
      deriv[5+3*n] = 2.*W*a3*pow*(x-a4)*(x-a4)/(a5*a5*a5);
      }

   for(n=0; n<3*(_ngauss+1); n++) drv[n] += iwt[i]*deriv[n];
   }
return;
}


/*
MATINV -- C translation of subroutine matinv from Bevington

JJH, 4/84

Change name to work around Mirella native MATINV     5 July 1988
*/

double _matinv(array,norder)
double **array; int norder;
{
int i, j, k, l;
static int *jk, *ik;
double fabs(), amax, save, det;
char *tstalloc();
static int lastn=0;

if(lastn != norder) {
   if(lastn) {
      free((char *)ik);   free((char *)jk);
      }
#ifdef lint
   (void)tstalloc((unsigned)sizeof(int)*norder);
   (void)tstalloc((unsigned)sizeof(int)*norder);
#else
   jk=(int *)tstalloc((unsigned)sizeof(int)*norder);
   ik=(int *)tstalloc((unsigned)sizeof(int)*norder);
#endif
   lastn = norder;
   }

det=1.;

for(k=0; k<norder; k++) {
   amax=0.;
   while(1) {
      for(i=k; i<norder; i++) {
         for(j=k; j<norder; j++) {
            if(fabs(amax) <= fabs(array[j][i])) {
               amax = array[j][i];
               ik[k] = i;
               jk[k] = j;
               }
            }
         }

      if(amax==0.)  return(0.);

      i = ik[k];
      
      if(i<k) continue;
      if(i>k) {
         for(j=0; j<norder; j++) {
            save = array[j][k];
            array[j][k] = array[j][i];
            array[j][i] = -save;
            }
         }

      j = jk[k];
     
      if(j<k) continue;
      if(j>k) {
         for(i=0; i<norder; i++) {
            save = array[k][i];
            array[k][i] = array[j][i];
            array[j][i] = -save;
            }
         }
      break;
      }

   for(i=0; i<norder; i++) {
      if(i != k) array[k][i] = -array[k][i]/amax;
      }
   
   for(i=0; i<norder; i++) {
      for(j=0; j<norder; j++) {
         if(i != k && j != k) {
            array[j][i] = array[j][i] + array[k][i]*array[j][k];
            }
         }
      }
   for(j=0; j<norder; j++) {
      if(j != k) array[j][k] = array[j][k]/amax;
      }
   array[k][k] = 1./amax;
   det = det * amax;
   }

for(l=0; l<norder; l++) {
   k = norder - l - 1;
   j=ik[k];
   if(j>k) {
      for(i=0; i<norder; i++) {
         save = array[k][i];
         array[k][i] = -array[j][i];
         array[j][i] = save;
         }
      }
   i = jk[k];
   if(i>k) {
      for(j=0; j<norder; j++) {
         save = array[j][k];
         array[j][k] = -array[j][i];
         array[j][i] = save;
         }
      }
   }
return(det);
}

/*
MAKEARRAY() -- Generate space for a contiguous array (which FORTRAN can
understand) and map into it an array of pointers (which C can more easily
deal with).

Jeff Hester, CIT
*/

#include <stdio.h>

char *makearray(xsize,ysize,elsize,array)
int xsize,ysize,elsize;
char ***array;
{
char *malloc();
char *ptr;
int j, *iptr, isize;

#ifdef lint
(void)malloc((unsigned)4*ysize);
#else
(*array) = (char **)malloc((unsigned)4*ysize);
#endif
if(*array<=0) {
   (void)fprintf(stderr,
	"MAKEARRAY: Memory allocation failed for pointers. %d bytes\n",
	4*ysize);
   perror("malloc");
   return(0);
   }
ptr=malloc((unsigned)elsize*xsize*ysize);
if(ptr<=0) {
   (void)fprintf(stderr,
	"MAKEARRAY: Memory allocation failed for data. %d bytes\n",
	elsize*xsize*ysize);
   perror("malloc");
   return(0);
   }

isize=(elsize*xsize*ysize+3)/4;
#ifdef lint
iptr=NULL;
#else
iptr=(int *)ptr;
#endif
for(j=0; j<isize; j++) *iptr++ =0;

for(j=0; j<ysize; j++) {
   (*array)[j]=ptr+xsize*elsize*j;
   }
return(ptr);
}


/*
FREEARRAY() -- FREE THE SPACE ALLOCATED WITH A CALL TO MAKEARRAY
*/

void freearray(array)
char **array;
{
free(array[0]);
free((char *)array);
}


/*Subroutine to do malloc with test for valid return.*/

char *tstalloc(length)
unsigned length;
{
char *tptr;
char *malloc();

void exit();

tptr=malloc(length);
if(tptr==0) {
   (void)printf("tstalloc:  Unsuccessful allocation of %d bytes. Must exit.\n",
         length);
   perror("tstalloc: ");
   exit(-1);
   }
return(tptr);
}
