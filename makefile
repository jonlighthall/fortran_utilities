# fortran compiler
FC = gfortran
# general flags
compile_flags = -c $<
output_flags = -o $@
#module_flags = -J $(MODDIR)
options = -std=f2018 -fimplicit-none
warnings = -Wall -Wsurprising -W -pedantic -Warray-temporaries		\
-Wcharacter-truncation -Wconversion-extra -Wimplicit-interface		\
-Wimplicit-procedure -Winteger-division -Wintrinsics-std		\
-Wreal-q-constant -Wuse-without-only -Wrealloc-lhs-all -Wno-tabs	\
#
# fortran compiler flags
FCFLAGS =  $(options) $(warnings)
#
# fortran compile flags
FC.COMPILE = $(FC) $(FCFLAGS) $(compile_flags) $(module_flags)
FC.COMPILE.o   = $(FC.COMPILE) $(output_flags)
FC.COMPILE.mod = $(FC.COMPILE) -o $(OBJDIR)/$*.o
#
# fortran link flags
flflags = $(output_flags) $^
FC.LINK = $(FC) $(FCFLAGS) $(flflags) $(module_flags)
#
# define subdirectories
OBJDIR := obj
MODDIR := mod
BINDIR := bin
#
# dependencies
SRC=$(wildcard *.f90)
OBJS = $(addprefix $(OBJDIR)/,$(SRC:.f90=.o))

MODS = 
SUBS = 
FUNS = 
DEPS := $(addprefix $(OBJDIR)/,$(addsuffix .o,$(MODS) $(SUBS) $(FUNS)))
MODS := $(addprefix $(MODDIR)/,$(addsuffix .mod,$(MODS)))

#
# executable name
EXES = $(addprefix $(BINDIR)/,$(SRC:.f90=.exe))

all: $(EXES) $(OBJS) $(DEPS) $(MODS)
	@echo "$@ done"

$(BINDIR)/%.exe: $(OBJDIR)/%.o | $(BINDIR)
	@echo "compiling executable $@..."
	$(FC.LINK)

$(OBJDIR)/%.o : %.f90 $(MODS) | $(OBJDIR)
	@echo "compiling f90 object $@..."
	$(FC.COMPILE.o)

$(MODDIR)/%.mod : %.f90 | $(MODDIR)
	@echo "compiling f90 module $@..."
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
distclean: clean
# remove Git versions
	$(CMD) *.~*~
# remove Emacs backup files
	$(CMD) *~ \#*\#
	@echo "$@ done"
test: distclean all
# test the makefile
	@echo "$@ done"	
