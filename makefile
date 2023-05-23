# fortran compiler
FC = gfortran
#
# general flags
compile = -c $<
output = -o $@
includes = -I $(INCDIR) -J $(MODDIR)
options = -std=f2008 -fimplicit-none
options_new = -std=f2018
#options := $(options) $(options_new)
warnings = -Wall -Wsurprising -W -pedantic -Warray-temporaries	\
-Wcharacter-truncation -Wimplicit-interface -Wintrinsics-std
warnings_new = -Wconversion-extra -Wimplicit-procedure	\
-Winteger-division -Wreal-q-constant -Wuse-without-only	\
-Wrealloc-lhs-all
warnings := $(warnings) $(warnings_new)
debug = -g -fbacktrace					\
-ffpe-trap=invalid,zero,overflow,underflow,denormal
debug_new = -fcheck=all
debug:= $(debug) $(debug_new)
#
# fortran compiler flags
FCFLAGS = $(options) $(warnings) $(debug)
F77.FLAGS = -fd-lines-as-comments
F90.FLAGS = 
FC.COMPILE = $(FC) $(FCFLAGS) $(compile)
FC.COMPILE.o = $(FC.COMPILE)  $(output) $(F77.FLAGS)
FC.COMPILE.o.f90 = $(FC.COMPILE) $(output) $(F90.FLAGS)
FC.COMPILE.mod = $(FC.COMPILE) -o $(OBJDIR)/$*.o $(F90.FLAGS)
#
# fortran linker flags
FLFLAGS = $(output) $^
FC.LINK = $(FC) $(FLFLAGS)
#
# define subdirectories
OBJDIR := obj
MODDIR := mod
BINDIR := bin
INCDIR := inc
VPATH = $(INCDIR)
#
# source files
SRC.F77 = $(wildcard *.f)
SRC.F90 = $(wildcard *.f90)
SRC = $(SRC.F77) $(SRC.F90)
#
# objects
OBJS.F77 = $(SRC.F77:.f=.o)
OBJS.F90 = $(SRC.F90:.f90=.o)
OBJS.all = $(OBJS.F77) $(OBJS.F90)
#
# dependencies (non-executables)
MODS. = 
SUBS. = 
FUNS. = 
DEPS. = $(MODS.) $(SUBS.) $(FUNS.)

DEPS.o = $(addsuffix .o,$(DEPS.))
OBJS.o = $(filter-out $(DEPS.o),$(OBJS.all))
MODS.mod = $(addsuffix .mod,$(MODS.))

DEPS := $(addprefix $(OBJDIR)/,$(DEPS.o))
OBJS := $(addprefix $(OBJDIR)/,$(OBJS.o))
MODS := $(addprefix $(MODDIR)/,$(MODS.mod))
#
# executables
EXES = $(addprefix $(BINDIR)/,$(OBJS.o:.o=.exe))

all: $(EXES) $(OBJS) $(DEPS) $(MODS)
	@echo "$@ done"
printvars:
	@echo $@:
	@echo
	@echo "SRC = $(SRC)"
	@echo
	@echo "OBJS.all = $(OBJS.all)"

	@echo	
	@echo "----------------------------------------------------"
	@echo

	@echo "MODS. = $(MODS.)"
	@echo
	@echo "SUBS. = $(SUBS.)"
	@echo
	@echo "FUNS. = $(FUNS.)"
	@echo
	@echo "DEPS. = $(DEPS.)"

	@echo
	@echo "----------------------------------------------------"
	@echo

	@echo "OBJS.o = $(OBJS.o)"
	@echo
	@echo "DEPS.o = $(DEPS.o)"
	@echo
	@echo "MODS.mod = $(MODS.mod)"

	@echo
	@echo "----------------------------------------------------"
	@echo

	@echo "EXES = $(EXES)"
	@echo
	@echo "OBJS = $(OBJS)"
	@echo
	@echo "DEPS = $(DEPS)"
	@echo
	@echo "MODS = $(MODS)"
	@echo

	@echo "$@ done"
#
# generic recipies
$(BINDIR)/%.exe: $(OBJDIR)/%.o $(DEPS) | $(BINDIR)
	@echo "\nlinking generic executable $@..."
	$(FC.LINK)	
$(OBJDIR)/%.o: %.f $(MODS) | $(OBJDIR)
	@echo "\ncompiling generic object $@..."
	$(FC.COMPILE.o)
$(OBJDIR)/%.o: %.f90 $(MODS) | $(OBJDIR)
	@echo "\ncompiling generic f90 object $@..."
	$(FC.COMPILE.o.f90)
$(MODDIR)/%.mod: %.f | $(OBJDIR) $(MODDIR)
	@echo "\ncompiling generic module $@..."
	$(FC.COMPILE.mod)
$(MODDIR)/%.mod: %.f90 | $(OBJDIR) $(MODDIR)
	@echo "\ncompiling generic f90 module $@..."
	$(FC.COMPILE.mod)
#
# define directory creation
$(OBJDIR):
	@mkdir -v $(OBJDIR)
$(BINDIR):
	@mkdir -v $(BINDIR)
$(MODDIR):
	@mkdir -v $(MODDIR)
# keep intermediate object files
.SECONDARY: $(OBJS) $(MODS)
#
# recipes without outputs
.PHONY: clean out distclean
#
# clean up routines
CMD = @rm -vfrd
clean:
	@echo removing files...
# remove compiled binaries
	$(CMD) $(TARGET)
	$(CMD) $(OBJDIR)/*.o
	$(CMD) $(OBJDIR)
	$(CMD) *.o *.obj
	$(CMD) $(MODDIR)/*.mod
	$(CMD) $(MODDIR)
	$(CMD) *.mod
	$(CMD) $(BINDIR)/*.exe
	$(CMD) $(BINDIR)
	$(CMD) *.exe
	$(CMD) *.out
	$(CMD) fort.*
	@echo "$@ done"
out:
# remove outputs produced by executables
	@echo "$@ done"
distclean: clean out
# remove Git versions
	$(CMD) *.~*~
# remove Emacs backup files
	$(CMD) *~ \#*\#
	@echo "$@ done"
test: distclean printvars all
# test the makefile
	@echo "$@ done"
