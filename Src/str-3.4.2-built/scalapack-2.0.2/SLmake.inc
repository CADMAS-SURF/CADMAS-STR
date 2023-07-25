############################################################################
#
#  Program:         ScaLAPACK
#
#  Module:          SLmake.inc
#
#  Purpose:         Top-level Definitions
#
#  Creation date:   February 15, 2000
#
#  Modified:        October 13, 2011
#
#  Send bug reports, comments or suggestions to scalapack@cs.utk.edu
#
############################################################################
#
#  C preprocessor definitions:  set CDEFS to one of the following:
#
#     -DNoChange (fortran subprogram names are lower case without any suffix)
#     -DUpCase   (fortran subprogram names are upper case without any suffix)
#     -DAdd_     (fortran subprogram names are lower case with "_" appended)

#CDEFS         = -DAdd_

ifeq ($(FNAME),LOWER_CASE)
  CDEFS=-DNoChange
else ifeq ($(FNAME),UPPER_CASE)
  CDEFS=-DUpCase
else ifeq ($(FNAME),LOWER_CASE_)
  CDEFS=-DAdd_
endif

#
#  The fortran and C compilers, loaders, and their flags
#

#FC            = mpif90
FC            = $(FCOMP)
#CC            = mpicc 
CC            = $(CCOMP)
#NOOPT         = -O0
#FCFLAGS       = -O3
FCFLAGS       = $(FOPT)
#CCFLAGS       = -O3
CCFLAGS       = $(COPT)
FCLOADER      = $(FC)
CCLOADER      = $(CC)
FCLOADFLAGS   = $(FCFLAGS)
CCLOADFLAGS   = $(CCFLAGS)

#
#  The archiver and the flag(s) to use when building archive (library)
#  Also the ranlib routine.  If your system has no ranlib, set RANLIB = echo
#

#ARCH          = ar
ARCH          = $(ARC)
#ARCHFLAGS     = cr
#RANLIB        = ranlib
RANLIB        = echo

#
#  The name of the ScaLAPACK library to be created
#

SCALAPACKLIB  = libscalapack.a

#
#  BLAS, LAPACK (and possibly other) libraries needed for linking test programs
#

BLASLIB       = -lblas
LAPACKLIB     = -llapack
LIBS          = $(LAPACKLIB) $(BLASLIB)
