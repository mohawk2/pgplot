#include <time.h>

contim( itime, itzone)
int   *itime;
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
