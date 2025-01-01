# Makefile for PGPLOT on MIPS machine, using ULTRIX and XWINDOWS.
#
# The original makefile for HPUX 8.07 as produced by
# M. J. Clement,  31-Jul-1992. That Makefile was adapted
# for use on a Decstation 5000 by Brad Carter and Jamie Lloyd
# in March 1993.
#
# This generates the PGPLOT binary files in the source directory
# (SRCDIR) and installs the archive and shared libraries in LIBDIR.
# If the latter is not /lib or /usr/lib, then links are useful for
# compiling programs.  The example programs are also compiled in
# the SRCDIR.
#
# In the SYSDIR, execute "setup".  Then change to the SRCDIR
# and run in turn
#          make lib		!main library (libpgplot.a)
#          make grfont.dat	!binary font file
#          make prog		!example programs
#          make oldlib		!obsolete program library (optional)
#          make clean		!remove binaries and restore SRCDIR
#
# SRC directory depends on where you install pgplot
SRC=.
#	Directory containing source code
SRCDIR=$(SRC)/pgplot/src
#	Directory containing demo programs
DEMODIR=$(SRC)/pgplot/examples
#	Directory containing font stuff
FONTDIR=$(SRC)/pgplot/fonts
#	Directory containing system-dependent code
SYSDIR=$(SRC)/pgplot/sys_mips
#	Directory to put the libraries
LIBDIR=$(SRC)/pgplot/lib
#	Fortran compiler 
FCOMPL=f77
FFLAGC=-O 
#	C compiler
CCOMPL=cc
CFLAGC=-O
#	Command used to compile demo programs.
DCOMPL=f77 -O
#	libraries required for linking demo programs
PGPLOT=-L$(LIBDIR) -lpgplot
LIBS=-lX11

.f.o:
	$(FCOMPL) -c $(FFLAGC) $<
.c.o:
	$(CCOMPL) -c $(CFLAGC) $<

#-----------------------------------------------------------------------
# Routine lists:
#   PG_ROUTINES: basic PGPLOT routines (Fortran-77)
#   PG_NON_STANDARD: non-Fortran-77 aliases for basic routines
#   GR_ROUTINES: support routines, not called directly by applications
#	(Fortran-77)
#   SYSTEM_ROUTINES: potentially non-portable routines, usually
#	operating-system dependent
#-----------------------------------------------------------------------
PG_ROUTINES=\
    pgask.o  pgbbuf.o pgbeg.o  pgbin.o  pgbox.o  pgbox1.o\
    pgcn01.o pgcnsc.o pgconb.o pgcons.o pgcont.o pgconx.o\
    pgcp.o   pgcurs.o pgdraw.o pgebuf.o pgend.o  pgenv.o \
    pgerrb.o pgerrx.o pgerry.o pgetxt.o pgfunt.o pgfunx.o\
    pgfuny.o pggray.o pghi2d.o pghis1.o pghist.o pgiden.o\
    pglab.o  pglcur.o pgldev.o pglen.o  pgline.o pgmove.o\
    pgmtxt.o pgncur.o pgnpl.o  pgnumb.o pgolin.o pgpage.o\
    pgpap.o  pgpixl.o pgpnts.o pgpoly.o pgpt.o   pgptxt.o\
    pgqcf.o  pgqch.o  pgqci.o  pgqcol.o pgqcr.o  pgqfs.o \
    pgqinf.o pgqls.o  pgqlw.o  pgqpos.o pgqvp.o  pgqwin.o\
    pgrect.o pgrnd.o  pgrnge.o pgscf.o  pgsch.o  pgsci.o \
    pgscr.o  pgsfs.o  pgshls.o pgsls.o  pgslw.o  pgsvp.o \
    pgswin.o pgtbox.o pgtext.o pgtime.o pgtlab.o pgtlb1.o\
    pgupdt.o pgvsiz.o pgvstd.o pgvw.o   pgwnad.o
PG_NON_STANDARD=\
    pgadvance.o pgbegin.o  pgcurse.o  pglabel.o\
    pgmtext.o   pgncurse.o pgpaper.o  pgpoint.o\
    pgptext.o   pgvport.o  pgvsize.o  pgvstand.o\
    pgwindow.o
GR_ROUTINES=\
    grarea.o grbpic.o grchsz.o grclip.o\
    grclos.o grclpl.o grcurs.o grdot0.o grdtyp.o\
    gresc.o gretxt.o grfa.o\
    grgray.o grgrgr.o grgrps.o grqci.o grqdev.o grqdt.o\
    grqfnt.o grqcol.o grqls.o  grqlw.o grqpos.o grqtyp.o\
    grldev.o grlen.o grlin0.o grlin1.o grlin2.o grlin3.o\
    grlina.o grmcur.o grmker.o grmova.o gropen.o\
    grpage.o grpars.o grrect.o grscr.o grslct.o\
    grsetc.o grsci.o grsfnt.o grsls.o grslw.o\
    grsets.o grsize.o grsyds.o grsymk.o grsyxd.o\
    grterm.o grtext.o grtoup.o grtrn0.o\
    grtxy0.o grvct0.o grxhls.o grxrgb.o grsfs.o\
    grpixl.o grpxpo.o grpxpx.o grpxre.o grrec0.o grfao.o gritoc.o
SYSTEM_ROUTINES=\
    grdate.o grflun.o grgcom.o grgenv.o grgetc.o\
    grglun.o grgmsg.o grlgtr.o grmsg.o groptx.o grprom.o\
    grsy00.o grtrml.o grtter.o gruser.o grwarn.o gr_termio.o grtermio.o

#-----------------------------------------------------------------------
# Device drivers
#-----------------------------------------------------------------------
CGDRIVER=cgdriv.o cfopenvws.o
GFDRIVER=gfdriv.o grre01.o grte02.o grre04.o grgf04.o lib_get_vm.o
GTDRIVER=gtdriv.o grre01.o grte02.o grte04.o grgf04.o grtk01.o
IMDRIVER=imdriv.o grim00.o grim02.o grim03.o lib_get_vm.o
IVDRIVER=ivdriv.o griv00.o grivas.o
LJDRIVER=ljdriv.o grlj00.o
LLDRIVER=lldriv.o grlj00.o
NUDRIVER=nudriv.o
PSDRIVER=psdriv.o grps01.o grps02.o
VPDRIVER=vpdriv.o
CPDRIVER=cpdriv.o
VCDRIVER=vcdriv.o
PXDRIVER=pxdriv.o grpx01.o grpx02.o lib_get_vm.o gribf1.o
QMDRIVER=qmdriv.o grqm00.o
QPDRIVER=qpdriv.o grqm00.o
REDRIVER=redriv.o grre01.o grre02.o grte02.o grre04.o lib_get_vm.o
SVDRIVER=svdriv.o svblock.o
TEDRIVER=tedriv.o grte01.o grte02.o grte04.o lib_get_vm.o
TFDRIVER=tfdriv.o grtf01.o grtf02.o grtf03.o lib_get_vm.o
TKDRIVER=tkdriv.o grtk00.o grtk01.o
TVDRIVER=tvdriv.o grtv00.o
VEDRIVER=vedriv.o grve01.o grve02.o lib_get_vm.o gribf1.o
VTDRIVER=vtdriv.o grvt02.o grvt03.o
VVDRIVER=vvdriv.o grve01.o grve02.o lib_get_vm.o gribf1.o
ZSDRIVER=zsdriv.o grzs01.o grzs02.o grzs03.o grzs04.o grzs05.o
XWDRIVER=xwdriv.o xwblock.o
X2DRIVER=x2driv.o figdisp_comm.o

# Drivers for MIPS version
DRIVERS=$(NUDRIVER) $(PSDRIVER) $(VPDRIVER) $(CPDRIVER) \
        $(VCDRIVER) $(TEDRIVER) $(XWDRIVER) $(X2DRIVER) $(VTDRIVER)

DISPATCH_ROUTINE=grexec.o

OBSOLETE_ROUTINES=\
    grchar.o grchr0.o grdat2.o grgtc0.o grmark.o grinqli.o grinqpen.o\
    grsetli.o grsetpen.o grlinr.o grmovr.o grtran.o grvect.o\
    pgsetc.o pgsize.o grinqfont.o grsetfont.o

#-----------------------------------------------------------------------
# Target "lib" is used to built the PGPLOT subroutiune library.
# libpgplot.a is the primary PGPLOT library and libpgplot.sl is
# the shared library.
#-----------------------------------------------------------------------

lib: arlib

arlib:	$(PG_ROUTINES) $(PG_NON_STANDARD) $(GR_ROUTINES) \
	   $(DISPATCH_ROUTINE) $(DRIVERS) $(SYSTEM_ROUTINES)
	ar ru $(LIBDIR)/libpgplot.a $(PG_ROUTINES) \
		$(PG_NON_STANDARD) $(GR_ROUTINES) $(DISPATCH_ROUTINE) \
		$(DRIVERS) $(SYSTEM_ROUTINES)

	ranlib $(LIBDIR)/libpgplot.a

#-----------------------------------------------------------------------
# libpgobs.a contains obsolete routines used by some programs
#-----------------------------------------------------------------------

oldlib: $(OBSOLETE_ROUTINES)
	@ar ru libpgobs.a $(OBSOLETE_ROUTINES)

	ranlib $(LIBDIR)/libpgobs.a


#-----------------------------------------------------------------------
# Target "prog" is used to make the demo programs. They can also be made
# individually.
#-----------------------------------------------------------------------
# List of demo programs
DEMOS = pgdemo1 pgdemo2 pgdemo3 pgex5 pgex13 pgex14 \
      pgex15 pgex17 pgex18 pgex22 pgex23 pgex24

prog: $(DEMOS)

pgdemo1: $(DEMODIR)/pgdemo1.f
	$(DCOMPL) -o pgdemo1 $(DEMODIR)/pgdemo1.f $(PGPLOT) $(LIBS)
pgdemo2: $(DEMODIR)/pgdemo2.f
	$(DCOMPL) -o pgdemo2 $(DEMODIR)/pgdemo2.f $(PGPLOT) $(LIBS)
pgdemo3: $(DEMODIR)/pgdemo3.f
	$(DCOMPL) -o pgdemo3 $(DEMODIR)/pgdemo3.f $(PGPLOT) $(LIBS)
pgex5:  $(DEMODIR)/pgex5.f
	$(DCOMPL) -o pgex5 $(DEMODIR)/pgex5.f $(PGPLOT) $(LIBS)
pgex13: $(DEMODIR)/pgex13.f
	$(DCOMPL) -o pgex13 $(DEMODIR)/pgex13.f $(PGPLOT) $(LIBS)
pgex14: $(DEMODIR)/pgex14.f
	$(DCOMPL) -o pgex14 $(DEMODIR)/pgex14.f $(PGPLOT) $(LIBS)
pgex15: $(DEMODIR)/pgex15.f
	$(DCOMPL) -o pgex15 $(DEMODIR)/pgex15.f $(PGPLOT) $(LIBS)
pgex17: $(DEMODIR)/pgex17.f
	$(DCOMPL) -o pgex17 $(DEMODIR)/pgex17.f $(PGPLOT) $(LIBS)
pgex18: $(DEMODIR)/pgex18.f
	$(DCOMPL) -o pgex18 $(DEMODIR)/pgex18.f $(PGPLOT) $(LIBS)
pgex22: $(DEMODIR)/pgex22.f
	$(DCOMPL) -o pgex22 $(DEMODIR)/pgex22.f $(PGPLOT) $(LIBS)
pgex23: $(DEMODIR)/pgex23.f
	$(DCOMPL) -o pgex23 $(DEMODIR)/pgex23.f $(PGPLOT) $(LIBS)
pgex24: $(DEMODIR)/pgex24.f
	$(DCOMPL) -o pgex24 $(DEMODIR)/pgex24.f $(PGPLOT) $(LIBS)

#-----------------------------------------------------------------------
# Target "grfont.dat" is the binary font file.
# This is created from grfont.txt with the "pgpack" program.
#    (a) compile the `pgpack' program; then
#    (b) run `pgpack' to convert the ASCII version of the font file
#    (grfont.txt) into the binary version (grfont.dat). When executed,
#    `pgpack' should report:
#	Characters defined:   996
#	Array cells used:   26732
#-----------------------------------------------------------------------
pgpack: $(FONTDIR)/pgpack.f
	$(FCOMPL) $(FFLAGC) -o pgpack $(FONTDIR)/pgpack.f

grfont.dat: $(FONTDIR)/grfont.txt pgpack
	@rm -f $(SRC)/pgplot/grfont.dat
	./pgpack <$(FONTDIR)/grfont.txt
	mv grfont.dat $(SRC)/pgplot

#-----------------------------------------------------------------------
# Target "install" is required for Figaro.
#-----------------------------------------------------------------------
install:

#-----------------------------------------------------------------------
# Target "clean" is used to remove all the intermediate files and
# the system_dependent source that was copied from the SYSDIR.
#-----------------------------------------------------------------------
clean:
	@makeclean

#-----------------------------------------------------------------------

xwblock.o: xwblock.c
	$(CCOMPL) -c $(CFLAGC) $(DEFINES) $(INCLUDES) xwblock.c
grdate.o: grdate.f
	$(FCOMPL) -c $(FFLAGC) grdate.f

# Include file dependencies:
# The following PG routines reference `pgplot.inc'

grgray.o grgrgr.o grgrps.o \
pgask.o  pgbbuf.o pgbeg.o  pgbin.o  pgbox.o  pgcont.o pgcp.o \
pgcurs.o pgebuf.o pgend.o  pgerrb.o pgerrx.o pgerry.o pggray.o pghi2d.o \
pghist.o pgiden.o pglcur.o pglen.o  pgmtxt.o pgncur.o pgolin.o \
pgpage.o pgpap.o  pgpixl.o pgpoly.o pgpt.o   pgptxt.o pgqch.o pgqfs.o \
pgqinf.o pgqvp.o  pgqwin.o pgrect.o pgsch.o  pgsfs.o  pgsvp.o \
pgswin.o pgupdt.o pgvsiz.o pgvstd.o pgvw.o   pgwnad.o: pgplot.inc

# The following GR routines reference `grpckg.inc'

grarea.o grbpic.o grchr0.o grchsz.o grclos.o grclpl.o grcurs.o \
grdot0.o grdtyp.o gresc.o  gretxt.o grfa.o   grgray.o grgrgr.o \
grldev.o grlen.o  grlin0.o grlin1.o grlin2.o grlin3.o \
grlina.o grlinr.o grmker.o grmova.o grmovr.o gropen.o grpage.o \
grpixl.o grpxpo.o grpxpx.o \
grqci.o  grqcol.o grqdev.o grqdt.o  grqfnt.o grqls.o  \
grqlw.o  grqpos.o grqtyp.o grrec0.o grrect.o \
grsci.o  grscr.o  grsetc.o grsetli.o grsets.o grsfnt.o grsfs.o grsize.o \
grslct.o grsls.o  grslw.o  grterm.o grtext.o grtrn0.o grtxy0.o \
grvct0.o: grpckg1.inc

griv00.o : gadef.h gmdef.h gphdef.h
grtv00.o : imdef.h

x2driv.o figdisp_comm.o : commands.h
