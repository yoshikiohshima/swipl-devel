################################################################
# Install CHR stuff for the MS-Windows built
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
LIBDIR=$(PLBASE)\library
EXDIR=$(PKGDOC)\examples\chr
CHR=$(LIBDIR)\chr
PL="$(PLBASE)\bin\plcon.exe"

EXAMPLES=	chrfreeze.chr fib.chr gcd.chr primes.chr \
		bool.chr family.chr fibonacci.chr leq.chr listdom.chr \
		chrdif.chr

all:		chr_translate.pl

chr_translate:	chr_translate.chr
		$(PL) -q -f chr_swi_bootstrap.pl \
		      -g "chr_compile_step1('$<','$@'),halt" \
		      -t 'halt(1)'
		$(PL) -q -f chr_swi_bootstrap.pl \
		      -g "chr_compile_step2('$<','$@'),halt" \
		      -t 'halt(1)'


!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		@if not exist "$(CHR)\$(NULL)" $(MKDIR) "$(CHR)"
		copy chr_runtime.pl "$(CHR)"
		copy chr_op.pl "$(CHR)"
		copy chr_translate.pl "$(CHR)"
		copy chr_debug.pl "$(CHR)"
		copy hprolog.pl "$(CHR)"
		copy pairlist.pl "$(CHR)"
		copy chr_swi.pl "$(LIBDIR)\chr.pl"
		copy README "$(CHR)\README.TXT"
		$(MAKEINDEX)
!ENDIF

html-install:	install-examples
pdf-install:	install-examples

install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"

xpce-install::

uninstall::
		del "$(CHR)\chr_runtime.pl"
		del "$(CHR)\chr_op.pl"
		del "$(CHR)\chr_translate.pl"
		del "$(CHR)\hprolog.pl"
		del "$(CHR)\pairlist.pl"
		del "$(CHR)\chr_debug.pl"
		del "$(CHR)\README.TXT"
		del "$(LIBDIR)\chr.pl"
		$(MAKEINDEX)

clean::
		if exist *~ del *~

distclean:	clean


