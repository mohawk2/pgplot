#!/bin/csh -f
#+
#  Name:
#     mk

#  Purpose:
#     Invoke "make" to build the PGPLOT library.

#  Type of Module:
#     Unix C shell script.

#  Description:
#     This script should be used to invoke the UNIX "make" command to build
#     the PGPLOT library. It wraps up the normal "make" command, which it
#     invokes after first defining appropriate environment variables for
#     the computer system in use. Note that this is not the normal way PGPLOT
#     is built. This file and the accompanying makefile.figaro have been added
#     for the purposes of Portable Standalone Figaro, to give a consistent 
#     way of making libraries throughout the system.

#  Invocation:
#     Invoke mk in the same way as you would normally use make, e.g:
#        mk build
#           Will build the PGPLOT library from its source files.
#
#     See the associated makefile for details of other make targets.

#  Prior Requirements:
#     -  This script will not execute unless the SYSTEM environment variable is
#     defined to identify the computer system in use.
#     -  A warning will be issued if $SYSTEM is not one of the recognised
#     computer systems on which the PGPLOT library is supported. In this case,
#     no further environment variables will be defined by this script and any
#     that are pre-defined will be passed on to "make" unaltered.

#  Authors:
#     RFWS: R.F. Warren-Smith (STARLINK, RAL)
#     KS: K. Shortridge (AAO).
#     SJM: S. Meatheringham (MSSSO)
#     {enter_new_authors_here}

#  History:
#     14-JUL-1992 (RFWS):
#        Original version.
#      3-APR-1993 (KS):
#        Modified for PGPLOT.
#      2-JUL-1993 (KS):
#        Introduced use of COMPILE_DEBUG to control -g and -O flags.
#        Note that the -w flag remains - otherwise there are warnings on
#        SUNS that would require code changes to bypass and I'd rather
#        avoid these.
#     12th Oct 1994 (SJM)
#	 Added in Solaris2 support (sun4_Solaris)
#     29th Nov 1994 (KS)
#        Added control of RANLIB for Solaris.
#     26th Feb 1997 (KS) 
#        Added use of X_INCLUDE for systems with X include files in
#        unusual places.
#     24th Aug 1998 (KS)
#        Added HP-UX case.
#     14th Aug 2002 (KS)
#        Added MacOSX case.
#     26th Feb 2004.
#        MacOS X and Linux now use g77.
#     13th Oct 2010.
#        MacOS X now includes architecture flags (-arch and -m32).
#      1st Dec 2010.
#        Solaris now uses g77.
#     {enter_changes_here}

#  Bugs:
#     There is something of a conflict bwtween this file and the setup_system
#     file used by the standard Figaro build process. A number of the
#     definitions from setup_system have to be refrocuded here, but not all 
#     are, which is a potential source of confusion.
#     {note_any_bugs_here}

#-

#  See if COMPILE_DEBUG has been defined.

      if ($?COMPILE_DEBUG) then
         set OPTDBG = -g
         echo "All compilations will use the debug (-g) flag"
      else
         set OPTDBG = -O
      endif

#  Some of Pgplot makes use of X11 include files. On some systems, these are not
#  present in the expected /usr/include/X11 directory. So we allow the
#  environment variable X_INCLUDE to specify an alternate directory (the
#  one it specifies should include the X11 directory)

      if ($?X_INCLUDE) then
         set XINC = -I$X_INCLUDE 
      else
         set XINC = ""
      endif

#  Check that SYSTEM is defined.
      if ( ! $?SYSTEM ) then
         echo "mk: Please define the environment variable SYSTEM to identify"
         echo "    your computer system"

#  If OK, test for each recognised system.
      else
         switch ( $SYSTEM )

#  DECstations:
            case mips:
               setenv FFLAGS "$OPTDBG -w"
               setenv CX_ROUTINES ""
               setenv FX_ROUTINES ""
               setenv SYS_DIR "sys_mips"
               setenv CFLAGS "$OPTDBG $XINC"
               echo "mk: Environment variables defined for $SYSTEM system"
            breaksw

#  SUN4 systems:
            case sun4:
               setenv SYS_DIR "sys_sun"
               setenv CX_ROUTINES "svdriv.c"
               setenv FX_ROUTINES ""
               setenv FFLAGS "$OPTDBG -PIC -w"
               setenv CFLAGS "$OPTDBG $XINC"
               echo "mk: Environment variables defined for $SYSTEM system"
            breaksw

#  SUN4 (Solaris2) systems:
            case sun4_Solaris:
               setenv SYS_DIR "sys_sun"
               setenv CX_ROUTINES ""
               setenv FX_ROUTINES ""
               setenv FFLAGS "$OPTDBG -w"
               setenv CFLAGS "$OPTDBG -DBSD_COMP -DSOLARIS2 $XINC"
               setenv RANLIB "echo >/dev/null"
               echo "mk: Environment variables defined for $SYSTEM system"
            breaksw

#  Linux (for PC) systems:
            case linux:
               setenv SYS_DIR "sys_linux"
               setenv CX_ROUTINES ""
               setenv FX_ROUTINES ""
               setenv FC "g77 -I/usr/local/include -fno-globals -Wno-globals -fno-automatic -finit-local-zero"
               setenv FFLAGS "$OPTDBG -I$SYS_DIR -L/usr/local/lib -Isrc"
               setenv CFLAGS "$OPTDBG -I/usr/local/include -D_f2c_ $XINC"
               echo "mk: Environment variables defined for $SYSTEM system"
            breaksw
            
#  MacOSX systems:
            case MacOSX:
               setenv SYS_DIR "sys_MacOSX"
               setenv CX_ROUTINES ""
               setenv FX_ROUTINES ""
#               setenv FC "g77 -m32 -fno-globals -Wno-globals -fno-automatic -finit-local-zero"
               setenv FC "gfortran -m32 -fno-automatic -finit-local-zero"
               setenv FFLAGS "$OPTDBG -I$SYS_DIR -L/usr/X11R6/lib -Isrc"
               setenv CFLAGS "-arch i386 $OPTDBG -D_f2c_ $XINC"
               echo "mk: Environment variables defined for $SYSTEM system"
            breaksw
            
#  HP-UX systems:
            case hp_ux:
               setenv FOPTDBG "$OPTDBG"
               if (!($?COMPILE_DEBUG)) then
                  set FOPTDBG = -O2
               endif
               setenv SYS_DIR "sys_hp"
               setenv CX_ROUTINES ""
               setenv FX_ROUTINES ""
               setenv FC "fort77"
               setenv FFLAGS "$FOPTDBG +ppu +es +E6 +U77"
               setenv CFLAGS "-Ae $OPTDBG -Dhp_ux"
               echo "mk: Environment variables defined for $SYSTEM system"
            breaksw

#  Issue a warning if SYSTEM is not recognised.
            default:
               echo "mk: WARNING: value of SYSTEM = $SYSTEM not recognised..."
               echo "             no environment variable definitions made"
            breaksw
         endsw

#  Invoke make with the appropriate environment variables set to over-ride
#  default macros defined in the makefile.
         echo make -f makefile.figaro -e $argv[1-]
         make -f makefile.figaro -e $argv[1-]
      endif

#  End of script.
