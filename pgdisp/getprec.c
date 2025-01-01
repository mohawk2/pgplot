/* The getprec routine returns the number of decimal digits of precision */
/* necessary to distinguish between the given values.  A maximum of MAX_PREC */
/* is used to prevent labels from getting too long. */

/* Sam Southard, Jr. */
/* Created: 26-Jun-1992 */
/* Modification History: */
/*  1-Jul-1992	SNS/CIT	Now only uses half the difference, since rounding off */
/*			can make using the full difference confusing. */

#include <math.h>

#define MAX_PREC	6

int getprec(val1,val2)
double val1, val2;	/* The values which need to be distinguished. */
{
	int prec=0;	/* The precision necessary */
	double diff;	/* the difference between the two values. */

	double fabs();

	diff=0.5*fabs(val2-val1);

	while (diff < 2.0 && prec < MAX_PREC)
	{
		++prec;
		diff *= 10.0;
	}

	return(prec);
}
