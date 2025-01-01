$! DCL command procedure to compile PGDISP for VAX/VMS
$!
$! PGDISP may be started wth a RUN command:
$!	$ run PGDISP
$! To specify command-line options, define PGDISP as a "foreign
$! command", e.g.
$!	$ PGDISP == "$SCR:[TJP]PGDISP"
$!	$ PGDISP -line 64
$!
$! The object and executable files are placed in the current default
$! directory.
$!
$! Ignore the following messages from the linker:
$! %LINK-W-NUDFSYMS, 7 undefined symbols:
$! %LINK-I-UDFSYM,         COMPOSITEOBJECTCLASS
$! %LINK-I-UDFSYM,         COMPOSITEWIDGETCLASS
$! %LINK-I-UDFSYM,         CONSTRAINTWIDGETCLASS
$! %LINK-I-UDFSYM,         OBJECTCLASS
$! %LINK-I-UDFSYM,         RECTOBJCLASS
$! %LINK-I-UDFSYM,         WIDGETCLASS
$! %LINK-I-UDFSYM,         WINDOWOBJCLASS
$!----------------------------------------------------------------------
$! The source code is found in directory with logical name SRC, defined
$! as follows (change this line for your installation):
$!
$ DEFINE SRC [-.PGDISP]
$!
$! Xwindow include files are in the following directory:
$!
$ DEFINE X11 DECW$INCLUDE
$!----------------------------------------------------------------------
$! Compile:
$!
$ CC /OBJECT=pg_cleanup.obj /define=PGDISP SRC:cleanup.c
$ CC SRC:pgdisp.c
$ CC /OBJECT=pg_figcurs.obj /define=PGDISP SRC:figcurs.c
$ CC /OBJECT=pg_getdata.obj /define=PGDISP SRC:getdata.c
$ CC /OBJECT=pg_getvisuals.obj /define=PGDISP SRC:getvisuals.c
$ CC /OBJECT=pg_handlexevent.obj /define=PGDISP SRC:handlexevent.c
$ CC /OBJECT=pg_proccom.obj /define=PGDISP SRC:proccom.c
$ CC /OBJECT=pg_resdb.obj /define=DPGDISP SRC:resdb.c
$ CC SRC:exposelgwin.c
$ CC SRC:getcolors.c
$ CC SRC:initlgluts.c
$ CC SRC:initlgwin.c
$ CC SRC:initlock.c
$ CC SRC:initwmattr.c
$ CC SRC:mainloop.c
$ CC SRC:resizelgwin.c
$ CC SRC:returnbuf.c
$ CC SRC:waitevent.c
$ CC SRC:updatelgtitle.c
$!
$! Link:
$!
$ LINK/EXEC=pgdisp.exe pgdisp, pg_cleanup, pg_figcurs, pg_getdata, -
pg_getvisuals, pg_handlexevent, pg_proccom, pg_resdb, exposelgwin, -
getcolors, initlgluts, initlgwin, initlock, initwmattr, mainloop, -
resizelgwin, returnbuf, waitevent, updatelgtitle, SYS$INPUT:/opt
SYS$SHARE:VAXCRTL.EXE/SHARE
SYS$SHARE:DECW$XLIBSHR.EXE/SHARE
$!
$! Remove intermediate files:
$!
$ DELETE/LOG/NOCONFIRM pgdisp.obj;*, pg_cleanup.obj;*, pg_figcurs.obj;*,-
pg_getdata.obj;*, pg_getvisuals.obj;*, pg_handlexevent.obj;*,-
pg_proccom.obj;*, pg_resdb.obj;*, exposelgwin.obj;*, getcolors.obj;*,-
initlgluts.obj;*, initlgwin.obj;*, initlock.obj;*, initwmattr.obj;*,-
mainloop.obj;*, resizelgwin.obj;*, returnbuf.obj;*, waitevent.obj;*,-
updatelgtitle.obj;*
$ PURGE/LOG/NOCONFIRM pgdisp.exe
$!
$ EXIT
