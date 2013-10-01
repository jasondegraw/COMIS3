#
#  A Simple Makefile for Compiling Comis3 with gfortran
#
#  Uncomment this to force static linking of gcc and gfortran libraries.
#  This will make the executable significantly larger (over twice as large
#  in tests on 64-bit Windows 7).
staticflags = -static-libgcc -static-libgfortran
#
#  Optimization - the default here is pretty agressive.  If it is causing
#  problems, then it might be worth it to back it down to -O1.  With -O2
#  and above, the GCC manual indicates that it might be a good idea to 
#  use -fno-gcse to get better performance.
#
FFLAGS = -O2 -fno-gcse
#
FORTRAN = gfortran
DELETE = del
# DELETE = rm -f
#
.SUFFIXES: .for
#
warns = comv-feq.o \
	comv-rex.o \
	comv-flo.o
#
mainobj = comv-mai.o
#
obj   = comv-ch2.o \
	comv-che.o \
	comv-cra.o \
	comv-dat.o \
	comv-deb.o \
	comv-do2.o \
	comv-eda.o \
	comv-fe8.o \
	comv-flb.o \
	comv-gr2.o \
	comv-gr3.o \
	comv-gr4.o \
	comv-gro.o \
	comv-hva.o \
	comv-in1.o \
	comv-in2.o \
	comv-in3.o \
	comv-in4.o \
	comv-in5.o \
	comv-in6.o \
	comv-in7.o \
	comv-in8.o \
	comv-inh.o \
	comv-inp.o \
	comv-lar.o \
	comv-lsl.o \
	comv-mat.o \
	comv-mul.o \
	comv-new.o \
	comv-occ.o \
	comv-ou2.o \
	comv-ou3.o \
	comv-ou4.o \
	comv-out.o \
	comv-phy.o \
	comv-pol.o \
	comv-pos.o \
	comv-pr2.o \
	comv-pre.o \
	comv-prj.o \
	comv-slv.o \
	comv-tim.o \
	comv-tm1.o \
	comv-tm2.o \
	comv-tm3.o \
	comv-tm4.o \
	comv-tm5.o \
	comv-tm6.o \
	comv-tm7.o \
	comv-tm8.o \
	comv-uni.o \
	comv-unx.o \
	comv-usr.o \
	comv-ut0.o \
	comv-ut1.o \
	comv-ut2.o \
	comv-ut4.o \
	comv-ut5.o \
	comv-uti.o 
#
progs = comis3
#
includes = comv-inp.inc  comv-par.inc  comv-phy.inc  comv-uni.inc
#
all: $(progs)
#
$(obj): $(includes)
#
$(progs): $(obj) $(warns) $(moded)
	$(FORTRAN) $(FFLAGS) -o $@ $(obj) $(warns) $(moded) -lm $(staticflags)
#
.for.o:
	$(FORTRAN) $(FFLAGS) -c $*.for
#
clean:
	$(DELETE) $(progs) $(obj) $(warns) $(moded)
