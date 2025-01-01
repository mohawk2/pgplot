cgtenv_(chr_ptr, cbuf_ptr, chr_len, cbuf_len)
char *chr_ptr, *cbuf_ptr;
int   chr_len,  cbuf_len;

/* Return the value of an environment variable.
 *
 * chr_ptr   Input   The environment variable
 * cbuf_ptr  Return  The translation (blanked filled)
 * chr_len   Input   The Fortran size of chr
 * cbuf_len  Input   The Fortran size of cbuf
 */
{
      int i;
      char *getenv (), *itmp, *iloc;

      itmp= cbuf_ptr;
      iloc= getenv(chr_ptr);
      for (i=1; i<=cbuf_len; i++) {
         if ( iloc==0 )
            *itmp=' ';
         else
            if ( *iloc==0 ) {
               *itmp=' ';
               iloc=0;
            } else {
               *itmp=*iloc;
               iloc++;
            }
         itmp++;
      }
      return;
}
