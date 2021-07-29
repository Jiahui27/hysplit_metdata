# Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
# 2003, 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation,
# Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

# Makefile to be used with ECMWF ecCodes library
#22 jan 2018 (amc) added intel compiler options.
#26 aug 2019 (syz) include a Makefile.inc.
#28 aug 2019 (syz) moved ECCODES path and options to Makefile.inc.

# include ../../Makefile.inc
FC=gfortran

ECCODES_TOPDIR=/usr/local/Cellar/eccodes/2.22.1
ECCODESINC=-I$(ECCODES_TOPDIR)/include
ECCODESLIBS=-L$(ECCODES_TOPDIR)/lib

EXE=./
PGM=era52arl

DATA=../data

LIBHYS = -L../../library -lhysplit

FFLAGS_FREEFORM=-ffree-form

FFLAGS = -ffpe-trap=zero,invalid,overflow,underflow \
	-W -Wall -fbounds-check -pedantic-errors

FCFLAGS = -g $(FFLAGS) $(FFLAGS_FREEFORM)



api2arl_DEPENDENCIES =  \
	$(ECCODES_TOPDIR)/lib/libeccodes_f90.so \
	$(ECCODES_TOPDIR)/lib/libeccodes.so
FCCOMPILE = $(FC) $(ECCODESINC) $(FCFLAGS)
FCLD = $(FC)
FCLINK = $(FCLD) $(FCFLAGS) $(AM_LDFLAGS) \
	 $(LDFLAGS) -o $(EXE)/$@

.SUFFIXES:
.SUFFIXES: .f90 .lo .o .obj

# $(PGM): $(PGM).o $(api2arl_DEPENDENCIES)
$(PGM): $(PGM).o
	@rm -f $(PGM)
	$(FCLINK) $(PGM).o $(ECCODESLIBS) $(ECCODESINC) -leccodes_f90 -leccodes


.f90.o:
	$(FCCOMPILE) -c -o $@ $<

clean :
	 rm -f $(EXE)/apidump
	 rm -f $(EXE)/$(PGM)
	 rm -f *.o

test:
	$(EXE)/$(PGM) -i$(DATA)/ERA5_2021.Jun02.3dpl.grib -a$(DATA)/ERA5_2021.Jun02.2dpl.all.grib