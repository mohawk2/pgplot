cc -c -o pg.cleanup.o -DPGDISP -O -I/usr/local/include cleanup.c
cc -c -O -I/usr/local/include pgdisp.c
cc -c -o pg.figcurs.o -DPGDISP -O -I/usr/local/include figcurs.c
cc -c -o pg.getdata.o -DPGDISP -O -I/usr/local/include getdata.c
cc -c -o pg.getvisuals.o -DPGDISP -O -I/usr/local/include getvisuals.c
cc -c -o pg.handlexevent.o -DPGDISP -O -I/usr/local/include handlexevent.c
cc -c -o pg.proccom.o -DPGDISP -O -I/usr/local/include proccom.c
cc -c -o pg.resdb.o -DPGDISP -O -I/usr/local/include resdb.c
cc -c -O -I/usr/local/include exposelgwin.c
cc -c -O -I/usr/local/include getcolors.c
cc -c -O -I/usr/local/include initlgluts.c
cc -c -O -I/usr/local/include initlgwin.c
cc -c -O -I/usr/local/include initlock.c
cc -c -O -I/usr/local/include initwmattr.c
cc -c -O -I/usr/local/include mainloop.c
cc -c -O -I/usr/local/include resizelgwin.c
cc -c -O -I/usr/local/include returnbuf.c
cc -c -O -I/usr/local/include waitevent.c
cc -c -O -I/usr/local/include updatelgtitle.c
cc -o pgdisp -O -I/usr/local/include -Bstatic pg.cleanup.o pgdisp.o pg.figcurs.o pg.getdata.o pg.getvisuals.o pg.handlexevent.o pg.proccom.o pg.resdb.o exposelgwin.o getcolors.o initlgluts.o initlgwin.o initlock.o initwmattr.o mainloop.o resizelgwin.o returnbuf.o waitevent.o updatelgtitle.o -lX11 -lm
