FC = gfortran
CC = gcc
FFLAGS = -O -ffixed-line-length-132 -fno-second-underscore -Wall -g -fbackslash

CFLAGS=-g -O

# explicit rule for F90:
%.mod: %.f90
%.o %.mod: %.f90
	$(COMPILE.f) $(OUTPUT_OPTION) $<

#include $(ZHTOOLS)/Make.libraries

# Use Readline if it exists, but also check ReadPar's Makefile
READLINE    = -DHAVEREADLINE
READLINELIBS= -L/soft/readline/lib -lreadline -lhistory -ltermcap

# wvdecomp needs cfitsio
FITSLIBS = -L/soft/cfitsio/lib -lcfitsio

OBJ = 	wvdecomp.o	\
	dodecomp.o	\
	calc_poisson_rms.o	\
	conv_eq_gauss.o	\
	conv_wavelet.o	\
	gausswv.o	\
	atrouswv.o	\
	class.o		\
	classshape.o	\
	restore.o	\
	thresholds.o	\
	smallutil.o	\
	memmngmnt.o	\
	atrousstep0.o	\
	memmngmnt_c.o	\
	marith_c.o \
	conv_rect1_1D_d.o yes.o no.o zhhelp.o \
	w_f_i.o readwrite.o strcat.o splitwords.o \
	rmblanks.o replace_char.o r_f_i.o r_f_i_u.o ft_printerr.o o_f_i.o \
	none.o newunit.o msarith.o mcopy.o marith.o floodfill.o \
	imsmo.o idistance.o if_peak.o cr_f_i.o conv_rect1.o check_naxis.o \
	exiterror.o xcopynoscale.o abs_path.o resize_img.o xcopyscale.o \
	real_path.o u_wants_clob.o fixstring.o fcstln.o

#OBJ = wvdecomp.o atrousdecomp.o atrousimg.o atrousstep.o atroussmo.o \
#	atrousdec.o restore.o

MAIN = wvdecomp

all:	librp.a	${MAIN}

librp.a:
	cd ReadPar; make

${MAIN}:	$(OBJ)
		${LINK.f} -o  $(MAIN) $(OBJ) librp.a \
			$(FITSLIBS) $(READLINELIBS)

clean:
	@$(RM) *.a *.so *.so.? *.dylib *.o errs *pure* .nfs* \
	foo* *~ *.log \#* TAGS *.E errors gmon.out *.pg *.bak \
	$(PROGS) a.out *.exe core
