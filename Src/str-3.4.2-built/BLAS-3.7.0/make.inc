####################################################################
#  BLAS make include file.                                         #
#  March 2007                                                      #
####################################################################
#
#SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
#PLAT = _LINUX
#  
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
#FORTRAN  = gfortran
FORTRAN  = $(FCOMP)
#OPTS     = -O3
OPTS     = $(FOPT)
DRVOPTS  = $(OPTS)
NOOPT    =
#LOADER   = gfortran
LOADER   = $(FCOMP)
LOADOPTS =
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
#ARCH     = ar
ARCH     = $(ARC)
#ARCHFLAGS= cr
#RANLIB   = ranlib
RANLIB   = echo
#
#  The location and name of the Reference BLAS library.
#
#BLASLIB      = blas$(PLAT).a
BLASLIB      = libblas.a
