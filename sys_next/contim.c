#include <time.h>

cgttod_(itime)
int   *itime;
/* Allow Fortran to call the C gettimeofday function. */
{
      gettimeofday( itime, 0);
      return;
}

contim_(itime, itzone)
time_t *itime;
struct tm *itzone;

/* Converts the localtime function into a Fortran readable array.
 *
 * itime   I    The system time
 * itzone    O  An array 11 elements long containing the localtime data
 */
{
      struct tm *localtime();

      *itzone= *localtime(itime);
      return;
}
