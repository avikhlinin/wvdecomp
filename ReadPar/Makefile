FC = gfortran
CC = gcc
FFLAGS = -O -ffixed-line-length-132 -fno-second-underscore -Wall -g -fbackslash

CFLAGS=-g -O

# explicit rule for F90:
%.mod: %.f90
%.o %.mod: %.f90
	$(COMPILE.f) $(OUTPUT_OPTION) $<

RANLIB = ranlib

# If readline is installed, use it, otherwise leave blank
READLINE    = -DHAVEREADLINE
READLINELIBS= -lreadline -lhistory -ltermcap


CPPFLAGS = $(READLINE) -DEXPANDVARS=1

OBJ = recordtype.o remleadblanks.o parstring.o irafstring.o parse_p_str.o \
	chomp.o getarg.o lcase.o commandline.o logics.o parse_cont.o \
	parfile.o parse_group.o readparf77_c.o readparf77.o \
	get_pv_def.o expand_vars.o stringpar.o cl_r_par.o numbered_par.o

../librp.a: ../librp.a($(OBJ))
	$(RANLIB) ../librp.a

commandline.o: commandline.c commandline_code.h
	$(CC) -c $(CFLAGS) -o commandline.o commandline.c

stringpar.o: stringpar.c commandline_code.h
	$(CC) -c $(CFLAGS) -o stringpar.o stringpar.c

clean:
	@$(RM) *.a *.so *.so.? *.dylib *.o errs *pure* .nfs* \
	foo* *~ *.log \#* TAGS *.E errors gmon.out *.pg *.bak \
	$(PROGS) a.out *.exe core
