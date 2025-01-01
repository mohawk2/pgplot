/*
 * Emulates the Berkeley UNIX Fortran routines.
 */

#define NULL 0
#define MAXLEN 256

typedef int STRING;
#define addr(string) ((char *)(string & 0xE0000007FFFFFFFF))
#define len(string) ((string & 0x1FFFFFF800000000) >> 35 )

#include <time.h>
/************************************************************************/
FDATE(date)
STRING date;
{
  long time(),clock;
  char *s;
  clock = time((long *)0);
  strncpy(addr(date), ctime(&clock), 24);
}
/************************************************************************/
GETENV(ename,evalue)
STRING ename,evalue;
{
  char buffer[MAXLEN],*s,*getenv();
  int i,length;

/* Get a zero terminated version of the input string. */

  strncpy(buffer,addr(ename),len(ename));
  buffer[len(ename)] = 0;

/* Get the environment variable, and its length. */

  s = getenv(buffer);
  if(s == NULL){
    length = 0;
  }else{
    strcpy(addr(evalue),s);
    length = strlen(s);
  }

/* Blank pad the fortran string. */

  s = addr(evalue) + length;
  for(i=length; i < len(evalue); i++) *s++ = ' ';
}
/************************************************************************/
GETLOG(string)
STRING string;
{
  char *s,*getlogin();
  int i,length;

  s = getlogin();
  if(s == NULL) s = "unknown";

  strcpy(addr(string),s);
  length = strlen(s);
  s = addr(string) + length;
  for(i=length; i < len(string); i++) *s++ = ' ';
}
/************************************************************************/
int ISATTY(lunit)
int *lunit;
{
  int filedes;
  if(*lunit==0) filedes = 2;
  else if(*lunit==6) filedes = 1;
  else filedes=0;
  return( (isatty(filedes) ? -1 : 0) );
}
/************************************************************************/
SLEEP(secs)
int *secs;
{
  sleep(*secs);
}
